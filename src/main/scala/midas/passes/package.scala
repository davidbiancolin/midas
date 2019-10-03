package midas

import firrtl._
import firrtl.ir._
import firrtl.passes.Pass
import firrtl.Mappers._
import firrtl.WrappedExpression._

import annotations._
import TargetToken.{Instance, OfModule}
import firrtl.Utils.BoolType

import scala.language.implicitConversions


package object passes {

  trait WrappedComponent {
    val decl: Statement
    val assigns: Statement
    val ref: Expression
  }

  case class SignalInfo(decl: Statement, assigns: Statement, ref: Expression) extends WrappedComponent

  object PassThru {
    def apply(source: WRef)(implicit ns: Namespace): SignalInfo = apply(source, source.name)
    def apply(source: WRef, name: String)(implicit ns: Namespace): SignalInfo = {
      val decl = DefWire(NoInfo, ns.newName(source.name), source.tpe)
      val ref = WRef(decl)
      SignalInfo(decl, Connect(NoInfo, WRef(decl), source), ref)
    }
  }

  object InstanceInfo {
    def apply(m: Module)(implicit ns: Namespace): InstanceInfo = {
      val inst = fame.Instantiate(m, ns.newName(m.name))
      InstanceInfo(inst, Block(Nil), WRef(inst))
    }
  }

  case class InstanceInfo(decl: WDefInstance, assigns: Block, ref: WRef) extends WrappedComponent {
    def addAssign(s: Statement): InstanceInfo = {
      copy(assigns = Block(assigns.stmts :+ s))
    }
    def connect(pName: String, rhs: Expression): InstanceInfo = {
      addAssign(Connect(NoInfo, WSubField(ref, pName), rhs))
    }
    def connect(lhs: Expression, pName: String): InstanceInfo = {
      addAssign(Connect(NoInfo, lhs, WSubField(ref, pName)))
    }
  }


  val AbstractClockGate = Module(
    info = NoInfo,
    name = "AbstractClockGate",
    ports =  Seq(
      Port(NoInfo, "I", Input, ClockType),
      Port(NoInfo, "CE", Input, BoolType),
      Port(NoInfo, "O", Output, ClockType)),
    body = Connect(
      NoInfo,
      WRef("O", ClockType, PortKind, FEMALE),
      DoPrim(PrimOps.AsClock, Seq(
        DoPrim(PrimOps.And, Seq(
          DoPrim(PrimOps.AsUInt, Seq(WRef("I", ClockType, PortKind, MALE)), Nil, BoolType),
          WRef("CE", BoolType, PortKind, MALE)
        ), Nil, BoolType)
      ), Nil, ClockType)))


  object OrElseIdentity {
    def apply[T](f: PartialFunction[T, T]): T => T = {
      f.orElse({ case x => x }: PartialFunction[T, T])
    }
  }

  object ModuleTransformer {
    def apply(f: PartialFunction[DefModule, DefModule]): Circuit => Circuit = {
      c => c mapModule OrElseIdentity(f)
    }
  }

  object StatementTransformer {
    def apply(f: PartialFunction[Statement, Statement]): Circuit => Circuit = {
      ModuleTransformer { case x => x mapStmt OrElseIdentity(f) }
    }
  }

  object ExpressionTransformer {
    def apply(f: PartialFunction[Expression, Expression]): Circuit => Circuit = {
      StatementTransformer { case x => x mapExpr OrElseIdentity(f) }
    }
  }

  object ReplaceExpression {
    type ReplMap = Map[WrappedExpression, Expression]
    private def onExpr(repls: ReplMap)(e: Expression): Expression = repls.getOrElse(we(e), e map onExpr(repls))
    def apply(repls: ReplMap)(s: Statement): Statement = s map apply(repls) map onExpr(repls)
  }

  trait AnyFormPass {
    final val inputForm: CircuitForm = UnknownForm
    final val outputForm: CircuitForm = UnknownForm
    def run(c: Circuit): Circuit
    final def execute(state: CircuitState): CircuitState = state.copy(circuit = run(state.circuit))
  }

  trait FunctionalPass {
    val transformer: Circuit => Circuit
    def run(c: Circuit): Circuit = transformer(c)
  }

  class EnsureDefined(m: DefModule) extends Transform with AnyFormPass {
    def run(c: Circuit): Circuit = {
      val moduleMap = c.modules.view.map({ m => m.name -> m }).toMap
      require(moduleMap.getOrElse(m.name, m) == m, s"EnsureDefined encountered conflicting definition of ${m.name}")
      if (moduleMap.contains(m.name)) {
        c
      } else {
        c.copy(modules = c.modules :+ m)
      } 
    }
  }

}
