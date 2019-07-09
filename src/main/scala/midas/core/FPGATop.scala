// See LICENSE for license details.

package midas
package core

import junctions._
import widgets._
import chisel3._
import chisel3.util._
import chisel3.core.ActualDirection
import chisel3.core.DataMirror.directionOf
import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.util.{DecoupledHelper}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

case object DMANastiKey extends Field[NastiParameters]
case object FpgaMMIOSize extends Field[BigInt]

// The AXI4 widths for a single host-DRAM channel
case object HostMemChannelNastiKey extends Field[NastiParameters]
// The number of host-DRAM channels -> all channels must have the same AXI4 widths
case object HostMemNumChannels extends Field[Int]
// The aggregate memory-space seen by masters wanting DRAM
case object MemNastiKey extends Field[NastiParameters]

class FPGATopIO(implicit val p: Parameters) extends WidgetIO {
  val dma  = Flipped(new NastiIO()(p alterPartial ({ case NastiKey => p(DMANastiKey) })))
  val mem  = Vec(4, new NastiIO()(p alterPartial ({ case NastiKey => p(HostMemChannelNastiKey) })))
}

// Platform agnostic wrapper of the simulation models for FPGA 
// TODO: Tilelink2 Port
class FPGATop(simIoType: SimWrapperIO)(implicit p: Parameters) extends Module with HasWidgets {
  val io = IO(new FPGATopIO)
  // Simulation Target
  val sim = Module(new SimBox(simIoType.cloneType))
  val simIo = sim.io.channelPorts
  // This reset is used to return the emulation to time 0.
  val master = addWidget(new EmulationMaster, "Master")
  val simReset = master.io.simReset

  sim.io.clock     := clock
  sim.io.reset     := reset.toBool || simReset
  sim.io.hostReset := simReset

  //val defaultIOWidget = addWidget(new PeekPokeIOWidget(
  //  simIo.pokedInputs,
  //  simIo.peekedOutputs,
  //  simIo.pokedReadyValidInputs,
  //  simIo.peekedReadyValidOutputs),
  //  "DefaultIOWidget")
  //defaultIOWidget.io.step <> master.io.step
  //master.io.done := defaultIOWidget.io.idle
  //defaultIOWidget.reset := reset.toBool || simReset
  

  // Note we are connecting up target reset here; we override part of this
  // assignment below when connecting the memory models to this same reset
  //simIo.pokedInputs.foreach({case (wire, name) => simIo.elements(name) <> defaultIOWidget.io.ins.elements(name) })
  //simIo.peekedOutputs.foreach({case (wire, name) => defaultIOWidget.io.outs.elements(name) <> simIo.elements(name)})
  //simIo.pokedReadyValidInputs.foreach({case (wire, name) => simIo.elements(name) <> defaultIOWidget.io.rvins.elements(name) })
  //simIo.peekedReadyValidOutputs.foreach({case (wire, name) => defaultIOWidget.io.rvouts.elements(name) <> simIo.elements(name)})

  //if (p(EnableSnapshot)) {
  //  val daisyController = addWidget(new strober.widgets.DaisyController(simIo.daisy), "DaisyChainController")
  //  daisyController.reset := reset.toBool || simReset
  //  daisyController.io.daisy <> simIo.daisy
  // KElvin was here
  //  val traceWidget = addWidget(new strober.widgets.IOTraceWidget(
  //    simIo.wireInputs map SimUtils.getChunks,
  //    simIo.wireOutputs map SimUtils.getChunks,
  //    simIo.readyValidInputs,
  //    simIo.readyValidOutputs),
  //    "IOTraces")
  //  traceWidget.reset := reset.toBool || simReset
  //  traceWidget.io.wireIns <> simIo.wireInTraces
  //  traceWidget.io.wireOuts <> simIo.wireOutTraces
  //  traceWidget.io.readyValidIns <> simIo.readyValidInTraces
  //  traceWidget.io.readyValidOuts <> simIo.readyValidOutTraces
  //  simIo.traceLen := traceWidget.io.traceLen
  //}

  private def channels2Port[T <: Data](port: HostPortIO[T], wires: T): Unit = {
    val valid = ArrayBuffer[Bool]()
    val ready = ArrayBuffer[Bool]()
    def loop[T <: Data](arg: (T, T)): Unit = arg match {
      case (target: ReadyValidIO[_], rv: ReadyValidIO[_]) =>
        val channel = simIo(rv)
        directionOf(channel.fwd.hValid) match {
          case ActualDirection.Input =>
            import chisel3.core.ExplicitCompileOptions.NotStrict // to connect nasti & axi4
            channel.target <> target
            channel.fwd.hValid := port.fromHost.hValid
            channel.rev.hReady := port.toHost.hReady
            ready += channel.fwd.hReady
            valid += channel.rev.hValid
          case ActualDirection.Output =>
            import chisel3.core.ExplicitCompileOptions.NotStrict // to connect nasti & axi4
            target <> channel.target
            channel.fwd.hReady := port.toHost.hReady
            channel.rev.hValid := port.fromHost.hValid
            ready += channel.rev.hReady
            valid += channel.fwd.hValid
          case _ => throw new RuntimeException(s"Unexpected valid direction: ${directionOf(channel.fwd.hValid)}")
        }
      case (target: Record, b: Record) =>
        b.elements.toList foreach { case (name, wire) =>
          loop(target.elements(name), wire)
        }
      case (target: Vec[_], v: Vec[_]) =>
        require(target.size == v.size)
        (target.zip(v)).foreach(loop)
      case (target: Bits, wire: Bits) => directionOf(wire) match {
        case ActualDirection.Input =>
          val channel = simIo(wire)
          channel.bits  := target
          channel.valid := port.fromHost.hValid
          ready += channel.ready
        case ActualDirection.Output =>
          val channel = simIo(wire)
          target := channel.bits
          channel.ready := port.toHost.hReady
          valid += channel.valid
        case _ => throw new RuntimeException(s"Unexpected Bits direction: ${directionOf(wire)}")
      }
      case (t: Clock, c: Clock) => throw new RuntimeException(s"Unexpected Clock in token channels: $c")
    }

    loop(port.hBits -> wires)
    port.toHost.hValid := valid.foldLeft(true.B)(_ && _)
    port.fromHost.hReady := ready.foldLeft(true.B)(_ && _)
  }

  val memPorts = new ListBuffer[NastiIO]
  case class DmaInfo(name: String, port: NastiIO, size: BigInt)
  val dmaInfoBuffer = new ListBuffer[DmaInfo]

  // Instantiate endpoint widgets. Keep a tuple of each endpoint's reset channel enq.valid and enq.ready
  //                      Valid, Ready
  (simIo.endpoints flatMap { endpoint =>
    Seq.tabulate(endpoint.size)({ i =>
      val widgetName = s"${endpoint.widgetName}_$i"
      val widget = addWidget(endpoint.widget(p), widgetName)
      widget.reset := reset.toBool || simReset
      widget match {
        case model: midas.models.FASEDMemoryTimingModel =>
          memPorts += model.io.host_mem
          model.io.tNasti.hBits.aw.bits.user := DontCare
          model.io.tNasti.hBits.aw.bits.region := DontCare
          model.io.tNasti.hBits.ar.bits.user := DontCare
          model.io.tNasti.hBits.ar.bits.region := DontCare
          model.io.tNasti.hBits.w.bits.id := DontCare
          model.io.tNasti.hBits.w.bits.user := DontCare
          case _ =>
      }
      channels2Port(widget.io.hPort, endpoint(i)._2)

      widget match {
        case widget: HasDMA => dmaInfoBuffer += DmaInfo(widgetName, widget.dma, widget.dmaSize)
        case _ => Nil
      }

    })
  // HACK: Need to add the tranformed-RTL channel as well
  })


  // Host Memory Channels
  // Masters = Target memory channels + loadMemWidget
  val numMemModels = memPorts.length
  val nastiP = p.alterPartial({ case NastiKey => p(MemNastiKey) })
  val loadMem = addWidget(new LoadMemWidget(MemNastiKey), s"LOADMEM_0")
  loadMem.reset := reset.toBool || simReset
  memPorts += loadMem.io.toSlaveMem

  val channelSize = BigInt(1) << p(HostMemChannelNastiKey).addrBits
  val hostMemAddrMap = new AddrMap(Seq.tabulate(p(HostMemNumChannels))(i =>
    AddrMapEntry(s"memChannel$i", MemRange(i * channelSize, channelSize, MemAttr(AddrMapProt.RW)))))

  val mem_xbar = Module(new NastiRecursiveInterconnect(numMemModels + 1, hostMemAddrMap)(nastiP))

  io.mem.zip(mem_xbar.io.slaves).foreach({ case (mem, slave) => mem <> NastiQueue(slave)(nastiP) })
  memPorts.zip(mem_xbar.io.masters).foreach({ case (mem_model, master) => master <> mem_model })


  // Sort the list of DMA ports by address region size, largest to smallest
  val dmaInfoSorted = dmaInfoBuffer.sortBy(_.size).reverse.toSeq
  // Build up the address map using the sorted list,
  // auto-assigning base addresses as we go.
  val dmaAddrMap = dmaInfoSorted.foldLeft((BigInt(0), List.empty[AddrMapEntry])) {
    case ((startAddr, addrMap), DmaInfo(widgetName, _, reqSize)) =>
      // Round up the size to the nearest power of 2
      val regionSize = 1 << log2Ceil(reqSize)
      val region = MemRange(startAddr, regionSize, MemAttr(AddrMapProt.RW))

      (startAddr + regionSize, AddrMapEntry(widgetName, region) :: addrMap)
  }._2.reverse
  val dmaPorts = dmaInfoSorted.map(_.port)

  if (dmaPorts.isEmpty) {
    val dmaParams = p.alterPartial({ case NastiKey => p(DMANastiKey) })
    val error = Module(new NastiErrorSlave()(dmaParams))
    error.io <> io.dma
  } else if (dmaPorts.size == 1) {
    dmaPorts(0) <> io.dma
  } else {
    val dmaParams = p.alterPartial({ case NastiKey => p(DMANastiKey) })
    val router = Module(new NastiRecursiveInterconnect(
      1, new AddrMap(dmaAddrMap))(dmaParams))
    router.io.masters.head <> NastiQueue(io.dma)(dmaParams)
    dmaPorts.zip(router.io.slaves).foreach { case (dma, slave) => dma <> NastiQueue(slave)(dmaParams) }
  }

  genCtrlIO(io.ctrl, p(FpgaMMIOSize))

  val addrConsts = dmaAddrMap.map {
    case AddrMapEntry(name, MemRange(addr, _, _)) =>
      (s"${name.toUpperCase}_DMA_ADDR" -> addr.longValue)
  }

  val headerConsts = addrConsts ++ List[(String, Long)](
    "CTRL_ID_BITS"   -> io.ctrl.nastiXIdBits,
    "CTRL_ADDR_BITS" -> io.ctrl.nastiXAddrBits,
    "CTRL_DATA_BITS" -> io.ctrl.nastiXDataBits,
    "CTRL_STRB_BITS" -> io.ctrl.nastiWStrobeBits,
    // These specify channel widths; used mostly in the test harnesses
    "MEM_ADDR_BITS"  -> io.mem(0).nastiXAddrBits,
    "MEM_DATA_BITS"  -> io.mem(0).nastiXDataBits,
    "MEM_ID_BITS"    -> io.mem(0).nastiXIdBits,
    // These are fixed by the AXI4 standard, only used in SW DRAM model
    "MEM_SIZE_BITS"  -> io.mem(0).nastiXSizeBits,
    "MEM_LEN_BITS"   -> io.mem(0).nastiXLenBits,
    "MEM_RESP_BITS"  -> io.mem(0).nastiXRespBits,
    "MEM_STRB_BITS"  -> io.mem(0).nastiWStrobeBits,
    // Address width of the aggregated host-DRAM space
    "DMA_ID_BITS"    -> io.dma.nastiXIdBits,
    "DMA_ADDR_BITS"  -> io.dma.nastiXAddrBits,
    "DMA_DATA_BITS"  -> io.dma.nastiXDataBits,
    "DMA_STRB_BITS"  -> io.dma.nastiWStrobeBits,
    "DMA_WIDTH"      -> p(DMANastiKey).dataBits / 8,
    "DMA_SIZE"       -> log2Ceil(p(DMANastiKey).dataBits / 8)
  )
}
