package midas.passes

import firrtl._
import firrtl.ir._
import firrtl.passes.Pass
import firrtl.Utils.BoolType

package object xilinx {
  val BUFGCE = ExtModule(
    NoInfo,
    "BUFGCE",
    Seq(
      Port(NoInfo, "I", Input, ClockType),
      Port(NoInfo, "CE", Input, BoolType),
      Port(NoInfo, "O", Output, ClockType)),
    "BUFGCE",
    Nil)

  object DefineBUFGCE extends EnsureDefined(BUFGCE)

  object ReplaceAbstractClockBuffers extends Pass with FunctionalPass {
    val transformer = StatementTransformer {
      case wi: WDefInstance if wi.module == midas.passes.AbstractClockGate.name =>
        wi.copy(module = BUFGCE.name)
    }
  }

  object HostSpecialization extends SeqTransform {
    val inputForm = LowForm
    val outputForm = LowForm
    val transforms = Seq(DefineBUFGCE, ReplaceAbstractClockBuffers)
  }
}
