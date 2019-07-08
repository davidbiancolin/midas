// See LICENSE for license details.

package midas
package widgets

import midas.core.SimUtils
import midas.core.ChannelWidth

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters

import scala.collection.mutable
import scala.collection.immutable.ListMap

trait HasChannels {
  // A higher order funciton that takes a hardware elaborator for each channel.
  // Based on the number of chunks in each signal binds the appropriate
  // number of programmable registers to them
  def bindChannels(bindSignal: (String, Int) => Int)(
      signals: Seq[(String, Int)], offset: Int): Seq[Int] = signals match {
    case Nil => Nil
    case (name: String, width: Int) :: sigs => {
      val address = width match {
        case 1 =>  bindSignal(name, offset)
        case _ =>
          // Need to append an offset to the name for each chunk
          (0 until width).toSeq.map(chunk => bindSignal(s"${name}_$chunk", offset + chunk)).head
      }
      // Bind the next signal; moving further down
      address +: bindChannels(bindSignal)(sigs, offset + width)
    }
  }
}

class PeekPokeIOWidgetIO(private val peekPokeEndpointIO: PeekPokeEndpointIO)(implicit val p: Parameters)
    extends WidgetIO()(p) {
  // Channel width == width of simulation MMIO bus
  val hPort = peekPokeEndpointIO.cloneType

  val step = Flipped(Decoupled(UInt(ctrl.nastiXDataBits.W)))
  val idle = Output(Bool())
}

// The interface to this widget is temporary, and matches the Vec of channels
// the sim wrapper produces. Ultimately, the wrapper should have more coarsely
// tokenized IOs.
// Maximum channel decoupling puts a bound on the number of cycles the fastest
// channel can advance ahead of the slowest channel
class PeekPokeIOWidget(
    peekPokeIO: PeekPokeEndpointIO,
    maxChannelDecoupling: Int = 2) (implicit p: Parameters) extends Widget()(p) {
  val io = IO(new PeekPokeIOWidgetIO(peekPokeIO))

  require(maxChannelDecoupling > 1, "A smaller channel decoupling could FMR")
  // Tracks the number of tokens the slowest channel has to produce or consume
  // before we reach the desired target cycle
  val ctrlWidth = p(ChannelWidth) // Hack: remove in Midas2
  val cycleHorizon = RegInit(0.U(ctrlWidth.W))
  val tCycleName = "tCycle"
  val tCycle = genWideRORegInit(0.U(64.W), tCycleName)
  val tCycleAdvancing = Wire(Bool())

  val hCycleName = "hCycle"
  val hCycle = genWideRORegInit(0.U(64.W), hCycleName)

  // needs back pressure from reset queues
  io.idle := cycleHorizon === 0.U

  def genWideReg(name: String, field: Data): Seq[UInt] = Seq.tabulate(
      (field.getWidth + ctrlWidth - 1) / ctrlWidth)({ i =>
    val chunkWidth = math.min(ctrlWidth, field.getWidth - (i * ctrlWidth))
    Reg(UInt(chunkWidth.W)).suggestName(s"target_${name}_{i}")
  })

  // Asserted by a channel when it is advancing or has advanced ahead of tCycle
  val channelDecouplingFlags = mutable.ArrayBuffer[Bool]()
  val channelPokes           = mutable.ArrayBuffer[(Seq[Int], Bool)]()

  def bindInputs(name: String, channel: DecoupledIO[Data]): Seq[Int] = {
    val reg = genWideReg(name, channel.bits)
    // Track local-channel decoupling
    val cyclesAhead = SatUpDownCounter(maxChannelDecoupling)
    val isAhead = !cyclesAhead.empty || channel.fire
    cyclesAhead.inc := channel.fire
    cyclesAhead.dec := tCycleAdvancing

    // If a channel is being poked, allow it to enqueue it's output token
    // This lets us peek outputs that depend combinationally on inputs we poke
    // Asserted when a memory-mapped register associated with channel is being written to
    val poke = Wire(Bool())
    // Handle the fields > 32 bits
    val wordsReceived = RegInit(0.U(log2Ceil(reg.size + 1).W))
    val advanceViaPoke = wordsReceived === reg.size.U

    when (poke) {
      wordsReceived := wordsReceived + 1.U 
    }.elsewhen(channel.fire) {
      wordsReceived := 0.U
    }

    channel.bits := Cat(reg.reverse).asTypeOf(channel.bits.asUInt)
    channel.valid := cyclesAhead.value < cycleHorizon || advanceViaPoke

    val regAddrs = reg.zipWithIndex.map({ case (chunk, idx) => attach(chunk,  s"${name}_${idx}", ReadWrite) })
    channelDecouplingFlags += isAhead
    channelPokes += regAddrs -> poke
    regAddrs
  }

  def bindOutputs(name: String, channel: DecoupledIO[Data]): Seq[Int] = {
    val reg = genWideReg(name, channel.bits)
    // Track local-channel decoupling
    val cyclesAhead = SatUpDownCounter(maxChannelDecoupling)
    val isAhead = !cyclesAhead.empty || channel.fire
    cyclesAhead.inc := channel.fire
    cyclesAhead.dec := tCycleAdvancing

    // Let token sinks accept one additional token that sources can produce (if
    // they aren't poked) This, to allow, us to peek outputs that depend
    // combinationally on inputs
    channel.ready := cyclesAhead.value < (cycleHorizon + 1.U)
    when (channel.fire) {
      reg.zipWithIndex.foreach({ case (reg, i) =>
        val msb = math.min(ctrlWidth * (i + 1) - 1, channel.bits.getWidth - 1)
        reg := channel.bits.asUInt()(msb, ctrlWidth * i)
      })
    }

    channelDecouplingFlags += isAhead
    reg.zipWithIndex.map({ case (chunk, idx) => attach(chunk,  s"${name}_${idx}", ReadOnly) })
  }

  val inputAddrs = io.hPort.ins.map({ case (name, channel) => bindInputs(name, channel) })
  val outputAddrs = io.hPort.outs.map({ case (name, channel) => bindOutputs(name, channel) })

  tCycleAdvancing := channelDecouplingFlags.reduce(_ && _)
  // tCycleAdvancing can be asserted if all inputs have been poked; but only increment
  // tCycle if we've been asked to step (cycleHorizon > 0.U)
  when (tCycleAdvancing && cycleHorizon > 0.U) {
    tCycle := tCycle + 1.U
    cycleHorizon := cycleHorizon - 1.U
  }

  hCycle := hCycle + 1.U

  when (io.step.fire) {
    cycleHorizon := io.step.bits
  }
  // For now do now, do not allow the block to be stepped further, unless
  // it has gone idle
  io.step.ready := io.idle

  val crFile = genCRFile()
  // Now that we've bound registers, snoop the poke register addresses for writes
  // Yay Chisel!
  channelPokes.foreach({ case (addrs: Seq[Int], poked: Bool) =>
    poked := addrs.map(i => crFile.io.mcr.write(i).valid).reduce(_ || _)
  })

  override def genHeader(base: BigInt, sb: StringBuilder): Unit = {
    import CppGenerationUtils._

    val name = getWName.toUpperCase
    def genOffsets(signals: Seq[String]): Unit = (signals.zipWithIndex) foreach {
      case (name, idx) => sb.append(genConstStatic(name, UInt32(idx)))}

    super.genHeader(base, sb)
    val inputs = io.hPort.targetInputs
    sb.append(genComment("Pokeable target inputs"))
    sb.append(genMacro("POKE_SIZE", UInt64(inputs.size)))
    genOffsets(inputs.unzip._2)
    sb.append(genArray("INPUT_ADDRS", inputAddrs.map(off => UInt32(base + off.head)).toSeq))
    sb.append(genArray("INPUT_NAMES", inputs.unzip._2 map CStrLit))
    sb.append(genArray("INPUT_CHUNKS", inputAddrs.map(addrSeq => UInt32(addrSeq.size)).toSeq))

    val outputs = io.hPort.targetOutputs
    sb.append(genComment("Peekable target outputs"))
    sb.append(genMacro("PEEK_SIZE", UInt64(outputs.size)))
    genOffsets(outputs.unzip._2)
    sb.append(genArray("OUTPUT_ADDRS", outputAddrs.map(off => UInt32(base + off.head)).toSeq))
    sb.append(genArray("OUTPUT_NAMES", outputs.unzip._2 map CStrLit))
    sb.append(genArray("OUTPUT_CHUNKS", outputAddrs.map(addrSeq => UInt32(addrSeq.size)).toSeq))
  }
}

class PeekPokeEndpointIO(private val targetIO: Record) extends ChannelizedHostPort(targetIO) {
  val (targetInputs, targetOutputs) = SimUtils.parsePorts(targetIO)
  val ins  = targetInputs.map({ case (field, name) => name -> InputChannel(field) })
  val outs = targetOutputs.map({ case (field, name) => name -> OutputChannel(field) })
  override val elements = ListMap((ins ++ outs):_*)
  override def cloneType = new PeekPokeEndpointIO(targetIO).asInstanceOf[this.type]
}

class PeekPokeEndpoint[T <: Record](gen: T) extends BlackBox with IsEndpoint {
  val io = IO(gen)
  val endpointIO = new PeekPokeEndpointIO(io)
  def widget = (p: Parameters) => new PeekPokeIOWidget(endpointIO)(p)
  generateAnnotations()
}
