package narad.nlp.tagger

import narad.bp.inference.InferenceOrder
import narad.bp.structure.{MessageNode, FactorGraph}
import util.matching.Regex

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 11/27/12
 * Time: 9:18 PM
 * To change this template use File | Settings | File Templates.
 */

/*
trait TaggerChainInference extends InferenceOrder {

	override def messageOrder(graph: FactorGraph): Iterator[MessageNode] = {
    System.err.println("Using TaggerChain Inference Order...")
		val idxpattern = new Regex(".*\\(([0-9]+)\\).*")
		val mqueue = scala.collection.mutable.Queue[MessageNode]()
		val groups = graph.nodes.groupBy{node =>
			val idxpattern(idx) = node.name
			idx
		}
    val slen = groups.size

		val bigram = graph.factors.exists(_.name.contains("blabel"))
		for (i <- 1 to slen) {
			val inodes = groups(i.toString)
      inodes.foreach{n => if (n.isFactor && n.arity == 1) mqueue += n}
      inodes.foreach{n => if (n.isVariable)  mqueue += n}
      inodes.foreach{n => if (n.isFactor && n.arity > 1) mqueue += n}
    }
		(mqueue ++ mqueue.reverse).iterator
	}

}
  */

// Below code matches BPDP chain order
trait TaggerChainInference extends InferenceOrder {

  override def messageOrder(graph: FactorGraph): Iterator[MessageNode] = {
 //   System.err.println("Using TaggerChain Inference Order...")
//    println(graph.toString)
    val idxpattern = new Regex(".*\\(([0-9]+)[^0-9].*")
    val mqueue = scala.collection.mutable.Queue[MessageNode]()
    val groups = graph.nodes.groupBy{node =>
      val idxpattern(time) = node.name
      time
    }
    val slen = groups.size

    //   val bigram = graph.factors.exists(_.name.contains("blabel"))
    for (i <- 1 to slen) {
      val inodes = groups(i.toString)
      inodes.foreach{n => if (n.isFactor && n.arity == 1) mqueue += n}
    }
    for (i <- 1 to slen) {
      val inodes = groups(i.toString)
//      println("TEST: " + i + " = " + inodes.mkString)
      inodes.foreach{n => if (n.isVariable)  mqueue += n}
      inodes.foreach{n => if (n.isFactor && n.arity > 1) mqueue += n}
    }
 //   for (m <- mqueue) println(m.name)
    (mqueue ++ mqueue.reverse).iterator
  }

}