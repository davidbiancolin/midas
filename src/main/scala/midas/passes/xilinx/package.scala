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

  object DefineBUFGCE extends Transform with NoAnalysis with UnchangedAnnotations {
    val transformer = { c: Circuit => c.copy(modules = c.modules :+ BUFGCE) }
  }

  object ReplaceAbstractClockBuffers extends Transform with NoAnalysis with UnchangedAnnotations {
    val transformer = StatementTransformer {
      case wi: WDefInstance if wi.module == DefineAbstractClockGate.blackbox.name =>
        wi.copy(module = BUFGCE.name)
    }
  }

  object HostSpecialization extends SeqTransform {
    val inputForm = LowForm
    val outputForm = LowForm
    val transforms = Seq(DefineBUFGCE, ReplaceAbstractClockBuffers)
  }
}
