// See LICENSE for license details.

package midas
package core

import chisel3._

// Adapted from DecoupledIO in Chisel3
class HostDecoupledIO[+T <: Data](gen: T) extends Bundle
{
  val hReady = Input(Bool())
  val hValid = Output(Bool())
  val hBits  = gen.cloneType
  def fire(): Bool = hReady && hValid
  override def cloneType: this.type =
    new HostDecoupledIO(gen).asInstanceOf[this.type]
}

/** Adds a ready-valid handshaking protocol to any interface.
  * The standard used is that the consumer uses the flipped interface.
  */
object HostDecoupled {
  def apply[T <: Data](gen: T): HostDecoupledIO[T] = new HostDecoupledIO(gen)
}

class HostReadyValid extends Bundle {
  val hReady = Input(Bool())
  val hValid = Output(Bool())
  def fire(): Bool = hReady && hValid
}

/**
  * Hack: A note on tokenFlip:
  * Previously we had difficulties generating hostPortIOs with flipped
  * aggregates of aggregates. We thus had to manually flip the subfields of the
  * aggregate in a new class (ex. the FlipNastiIO). tokenFlip captures
  * whether hBits should be flipped when it is cloned.
  *
  * thus what would ideally be expressed as HostPort(Flipped(new NastiIO)) must
  * be expressed as HostPort((new NastiIO), tokenFlip = true)
  */

class HostPortIO[+T <: Data](gen: T, tokenFlip: Boolean) extends Bundle
{
  val fromHost = Flipped(new HostReadyValid)
  val toHost = new HostReadyValid
  val hBits  = if (tokenFlip) Flipped(gen) else gen
  override def cloneType: this.type =
    new HostPortIO(gen, tokenFlip).asInstanceOf[this.type]
}

object HostPort {
  def apply[T <: Data](gen: T, tokenFlip : Boolean = false): HostPortIO[T] = new HostPortIO(gen, tokenFlip)
}
