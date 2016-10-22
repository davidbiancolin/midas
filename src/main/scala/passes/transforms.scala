package strober
package passes

import firrtl._
import firrtl.ir._
import firrtl.Mappers._
import firrtl.passes.memlib._
import firrtl.Annotations.{AnnotationMap, CircuitName, TransID}
import Utils._
import StroberTransforms._
import scala.collection.mutable.{HashMap, LinkedHashSet, ArrayBuffer}
import scala.util.DynamicVariable
import java.io.{File, FileWriter}

private object StroberTransforms {
  type ChainMap = HashMap[String, ArrayBuffer[ir.Statement]]
  type ChildMods = HashMap[String, LinkedHashSet[String]]
  type ChildInsts = HashMap[String, ArrayBuffer[String]]
  type InstModMap = HashMap[(String, String), String]
}

private class TransformAnalysis(
    childMods: ChildMods,
    childInsts: ChildInsts,
    instModMap: InstModMap) extends firrtl.passes.Pass {
  def name = "[strober] Analyze Circuit"

  def collectChildren(mname: String, blackboxes: Set[String])(s: Statement): Statement = {
    s match {
      case s: WDefInstance if !blackboxes(s.module) =>
        childMods(mname) += s.module
        childInsts(mname) += s.name
        instModMap(s.name -> mname) = s.module
      case _ =>
    }
    s map collectChildren(mname, blackboxes)
  }

  def collectChildrenMod(blackboxes: Set[String])(m: DefModule) = {
    childInsts(m.name) = ArrayBuffer[String]()
    childMods(m.name) = LinkedHashSet[String]()
    m map collectChildren(m.name, blackboxes)
  }

  def run(c: Circuit) = {
    val blackboxes = (c.modules collect { case m: ExtModule => m.name }).toSet
    c copy (modules = c.modules map collectChildrenMod(blackboxes))
  }
}

private[strober] class StroberTransforms(
    dir: File,
    io: chisel3.Data)
   (implicit param: cde.Parameters) extends Transform with SimpleRun {
  val childMods = new ChildMods
  val childInsts = new ChildInsts
  val instModMap = new InstModMap
  val chains = (ChainType.values.toList map (_ -> new ChainMap)).toMap
  lazy val sim = new SimWrapper(new TargetBox(io))

  def execute(circuit: Circuit, map: AnnotationMap) = {
    ((map get TransID(-2)): @unchecked) match {
      case Some(p) => ((p get CircuitName(circuit.main)): @unchecked) match {
        case Some(ReplSeqMemAnnotation(t, _)) =>
          val conf = new File(PassConfigUtil.getPassOptions(t)(OutputConfigFileName))
          val seqMems = (MemConfReader(conf) map (m => m.name -> m)).toMap
          val chainFile = new FileWriter(new File(dir, s"${circuit.main}.chain"))
          run(circuit, Seq(
            new Fame1Transform(seqMems),
            new TransformAnalysis(childMods, childInsts, instModMap),
            new AddDaisyChains(childMods, childInsts, instModMap, chains, seqMems),
            new SimulationMapping(sim, chainFile, childInsts, instModMap, chains, seqMems),
            new PlatformMapping(sim.io, circuit.main, dir, chainFile)
          ))
      }
    }
  }
}
