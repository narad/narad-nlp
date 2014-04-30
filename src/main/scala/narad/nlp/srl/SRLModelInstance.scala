package narad.nlp.srl

import narad.bp.structure.{MessageNode, ModelInstance, FactorGraph}
import narad.bp.util.PotentialExample
import collection.mutable.Queue
import javax.management.remote.rmi._RMIConnection_Stub
import collection.mutable

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 8/16/13
 * Time: 9:19 AM
 */
class SRLModelInstance(fg: FactorGraph, ex: PotentialExample) extends ModelInstance(fg, ex) {
  val SRL_ARG_VAR_PATTERN     = """srlArgVar\(([0-9]+),([0-9]+)\)""".r
  val SRL_ARG_ATMOST_PATTERN  = """srlIsAtMost\(([0-9]+),([0-9]+)\)""".r
  val SRL_LABEL_VAR_PATTERN   = """srlLabelVar\(([0-9]+),([0-9]+),(.+)\)""".r

  val ARG_VALENCY_FAC_PATTERN   = """achainFac\(([0-9]+),(.+)\)""".r
  val ARG_VALENCY_VAR_PATTERN   = """A\(([0-9]+),(.+)\)""".r
  val ARG_TETHER_FAC_PATTERN  = """argSenseTether\(([0-9]+)\)""".r

  val ROLE_VALENCY_FAC_PATTERN   = """rchainFac\(([0-9]+),([0-9]+),(.+)\)""".r
  val ROLE_VALENCY_VAR_PATTERN   = """R\(([0-9]+),([0-9]+),(.+)\)""".r
  val ROLE_TETHER_FAC_PATTERN  = """roleSenseTether\(([0-9]+),([0-9]+)\)""".r

  val SRL_SENSE_VAR_PATTERN   = """senseVar\(([0-9]+)\)""".r

  val DP_VAR_PATTERN = """un\(([0-9]+),([0-9]+)\)""".r
  val DP_CONNECT_PATTERN = """sslink\(([0-9]+),([0-9]+)\)""".r
  val DP_TREE_PATTERN = """PTREE""".r

  override def messageOrder(graph: FactorGraph): Iterator[MessageNode] = {
    // Assumes that unary factors are prioritized and push messages only once
    // at the start of inference...
    val srlqueue = new Queue[MessageNode]()

    // Queue Syntax
    val dpqueue = new Queue[MessageNode]()
    val dpnodes = graph.nodes.filter(_.name.startsWith("linkVar"))
    val treenodes = graph.nodes.filter(_.name.startsWith("PTREE"))
    val connectnodes = graph.nodes.filter(_.name.startsWith("sslink"))
    dpqueue ++= dpnodes ++ treenodes ++ dpnodes ++ connectnodes

    // Queue SRL
    val srlnodes = graph.nodes.map { n =>
      val sortIdx = n.name match {
        case SRL_ARG_VAR_PATTERN(sidx, eidx) => 1
        case SRL_ARG_ATMOST_PATTERN(sidx, eidx) => 2
        case SRL_LABEL_VAR_PATTERN(sidx, eidx, ridx) => 3
        case _ => -1
      }
      (n, sortIdx)
    }
    srlqueue ++= srlnodes.filter(_._2 > 0).sortBy(_._2).map(_._1)

    // Queue VALENCY Nodes
    val argValencyOrder = graph.nodes.map { n =>
      val sortIdx = n.name match {
        case SRL_ARG_VAR_PATTERN(sidx, eidx) => (sidx.toInt + (eidx.toInt * 100))
        case ARG_VALENCY_FAC_PATTERN(sidx, eidx) => (sidx.toInt + (eidx.toInt * 100)) + 2
        case ARG_VALENCY_VAR_PATTERN(sidx, eidx) => (sidx.toInt + (eidx.toInt * 100)) + 3
        case ARG_TETHER_FAC_PATTERN(sidx) => 1000000000
        case SRL_SENSE_VAR_PATTERN(sidx) => 1000000000 + 1
        case _ => -1
      }
      (n, sortIdx)
    }
    val avalencyqueue = new mutable.Queue[MessageNode]()
    avalencyqueue ++= argValencyOrder.filter(_._2 > 0).sortBy(_._2).map(_._1)
    avalencyqueue ++= avalencyqueue.reverse

    val roleValencyOrder = graph.nodes.map { n =>
      val sortIdx = n.name match {
        case SRL_LABEL_VAR_PATTERN(sidx, eidx, ridx) => (sidx.toInt + (eidx.toInt * 100) + (ridx.toInt * 1000))
        case ROLE_VALENCY_FAC_PATTERN(sidx, eidx, ridx) => (sidx.toInt + (eidx.toInt * 100) + (ridx.toInt * 1000)) + 2
        case ROLE_VALENCY_VAR_PATTERN(sidx, eidx, ridx) => (sidx.toInt + (eidx.toInt * 100) + (ridx.toInt * 1000)) + 3
        case ROLE_TETHER_FAC_PATTERN(sidx, ridx) => 1000000000
        case SRL_SENSE_VAR_PATTERN(sidx) => 1000000000 + 1
        case _ => -1
      }
      (n, sortIdx)
    }
    val rvalencyqueue = new mutable.Queue[MessageNode]()
    rvalencyqueue ++= roleValencyOrder.filter(_._2 > 0).sortBy(_._2).map(_._1)
    rvalencyqueue ++= rvalencyqueue.reverse

    dpqueue.iterator ++ srlqueue ++ avalencyqueue ++ rvalencyqueue ++ srlqueue.reverseIterator ++ connectnodes
  }
}

class SRLHiddenModelInstance(fg: FactorGraph, ex: PotentialExample) extends SRLModelInstance(fg, ex) {

  override def clampedFactors = {
    fg.factors.filter{ f =>
      !(f.name.startsWith("linkFac") || f.name.startsWith("sslink") || f.name.startsWith("brack") || f.name.startsWith("clink")
        || f.name.contains("chain"))
    }.iterator
  }
}