package narad.nlp.parser.constituent

import narad.bp.inference.InferenceOrder
import narad.bp.structure.{ModelInstance, MessageNode, FactorGraph}
import collection.mutable.Queue

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 4/10/13
 * Time: 6:14 PM
 * To change this template use File | Settings | File Templates.
 */
trait ConstituentParserInferenceOrder extends InferenceOrder {

  self: ConstituentParserModelInstance =>

  override def messageOrder(graph: FactorGraph): Iterator[MessageNode] = {
    val mqueue = new Queue[MessageNode]()
    val q1 = new Queue[MessageNode]()
    val q2 = new Queue[MessageNode]()
    val q3 = new Queue[MessageNode]()
    val q4 = new Queue[MessageNode]()

    for (n <- graph.nodes) {
      n match {
        case x if x.name.startsWith("brackvar") => q3 += n
        case x if x.name.startsWith("unaryvar") => q3 += n
        case x if x.name.startsWith("isAtMost") => q2 += n
        case x if x.name.startsWith("labelvar") => q1 += n
        case x if x.name.startsWith("unaryLabelvar") => q1 += n
        case x if x.name.startsWith("CKY") => q4 += n
        case x if x.arity == 1 && x.isFactor => {}
      }
    }

    mqueue ++= q1
    mqueue ++= q2
    mqueue ++= q3
//    mqueue ++= q4
    mqueue.iterator ++ q4 ++ mqueue.reverseIterator
  }
}






















//  System.err.println("Using Parser Inference Order...")
//    println(graph.toString)

/*
val idxpattern = new Regex(".*\\(([0-9]+)[^0-9].*")
val mqueue = scala.collection.mutable.Queue[MessageNode]()
//    graph.nodes.filter(n => n.isFactor && n.arity == 1 && n.name.contains("brack")).foreach(mqueue += _)
//    graph.nodes.filter(n => n.isFactor && n.arity == 1 && n.name.contains("label")).foreach(mqueue += _)
graph.nodes.filter(n => n.isVariable && n.name.contains("label")).foreach(mqueue += _)
graph.nodes.filter(n => n.isFactor && n.arity > 1 && !n.name.contains("CKY")).sortBy(_.arity).foreach(mqueue += _)
graph.nodes.filter(n => n.isVariable && n.name.contains("brack")).foreach(mqueue += _)
graph.nodes.filter(n => n.isFactor && n.arity > 1 && n.name.contains("CKY"))
mqueue.iterator ++ mqueue.reverseIterator
*/









/*
      if (n.name.startsWith("CKY")) {
        q4 += n
      }
      if (n.isFactor && n.arity > 1 && !n.name.contains("CKY")) {
        q2 += n // isAtMost1 facs
      }
      if (n.isVariable) {
        if (n.name.startsWith("label") || n.name.startsWith("unaryLabel")) {
          q1 += n   // label variables
        }
        else {
          q3 += n // brack variables
        }
      }
      */