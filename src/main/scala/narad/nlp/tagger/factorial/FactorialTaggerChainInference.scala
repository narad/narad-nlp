package narad.nlp.tagger.factorial

import narad.bp.inference.InferenceOrder
import narad.bp.structure.{MessageNode, FactorGraph}
import util.matching.Regex

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 5/12/13
 * Time: 2:06 AM
 * To change this template use File | Settings | File Templates.
 */

trait FactorialTaggerChainInference extends InferenceOrder {
  val LABEL_FAC_PATTERN = """labelfac\(([0-9]+),([0-9]+)\)""".r
  val LABEL_VAR_PATTERN = """labelvar\(([0-9]+),([0-9]+)\)""".r
  val CHAIN_NODE_PATTERN = """chain.*\(([0-9]+),([0-9]+),([0-9]+),([0-9]+).*\)""".r
  val SLICE_NODE_PATTERN = """slice.*\(([0-9]+),([0-9]+),([0-9]+),([0-9]+).*\)""".r
  val DEPENDENCY_PATTERN = """\(([0-9]+),([0-9]+)\)""".r

  override def messageOrder(graph: FactorGraph): Iterator[MessageNode] = {
    val idxpattern = new Regex(".*\\(([0-9]+),([0-9]+),([0-9]+),([0-9]+).*")
    val mqueue = scala.collection.mutable.Queue[MessageNode]()
    var numSlices = 0
    var numChains = 0
    val groups = graph.nodes.groupBy { node =>
      node.name match {
        case LABEL_FAC_PATTERN(chain, slice) => {
          ("UNARY_FACTOR")
        }
        case LABEL_VAR_PATTERN(chain, slice) => {
          val c = chain.toInt
          val s = slice.toInt
          if (c > numChains) numChains = c
          if (s > numSlices) numSlices = s
          (c, s, "VARIABLE")
        }
        case CHAIN_NODE_PATTERN(chain1, chain2, slice1, slice2) => {
          (chain1.toInt, slice1.toInt, "CHAIN")
        }
        case SLICE_NODE_PATTERN(chain1, chain2, slice1, slice2) => {
          (chain1.toInt, slice1.toInt, "SLICE")
        }
      }
    }
    numChains += 1

    for (f <- groups("UNARY_FACTOR")) mqueue += f

    for (i <- 1 to numSlices; j <- 0 until numChains) {
      mqueue += groups((j, i, "VARIABLE"))(0)
      if (groups.contains((j, i, "CHAIN"))) mqueue += groups(j, i, "CHAIN")(0)
    }
    for (i <- numSlices-1 to 1 by -1; j <- numChains-1 to 0 by -1) {
      if (groups.contains((j, i, "CHAIN"))) {
        mqueue += groups(j, i, "CHAIN")(0)
        mqueue += groups((j, i, "VARIABLE"))(0)
      }
    }

    if (numChains > 1) {
      for (i <- 1 to numSlices) {
        for (j <- 0 until numChains) {
          if (groups.contains((j, i, "SLICE"))) {
            mqueue += groups((j, i, "VARIABLE"))(0)
            mqueue ++= groups(j, i, "SLICE")
          }
        }

        mqueue += groups((numChains-1, i, "VARIABLE"))(0)

        for (j <- numChains-1 to 0 by -1) {
          if (groups.contains((j, i, "SLICE"))) {
            mqueue ++= groups(j, i, "SLICE")
            mqueue += groups((j, i, "VARIABLE"))(0)
          }
        }
      }
    }
//    println("MQUEUE (%d x %d):\n".format(numSlices, numChains))
//    println(mqueue.map(_.toString()).mkString("\n"))
    mqueue.iterator
  }
}



/*
  override def messageOrder(graph: FactorGraph): Iterator[MessageNode] = {
    val idxpattern = new Regex(".*\\(([0-9]+),([0-9]+),([0-9]+),([0-9]+).*")
    val mqueue = scala.collection.mutable.Queue[MessageNode]()
    val groups = graph.nodes.groupBy { node =>
      node.name match {
        case LABEL_NODE_PATTERN(chain, slice) => {
          (slice.toInt)
        }
        case CHAIN_NODE_PATTERN(chain1, chain2, slice1, slice2) => {
          (slice1.toInt)
        }
        case SLICE_NODE_PATTERN(chain1, chain2, slice1, slice2) => {
          (slice1.toInt)
        }
      }
    }

    val slen = groups.size
    for (i <- 1 to slen) {
      val inodes = groups(i)
      inodes.foreach{n => if (n.isFactor && n.arity == 1) mqueue += n}
    }
    for (i <- 1 to slen) {
      val inodes = groups(i)
      inodes.foreach{n => if (n.isVariable)  mqueue += n}
      inodes.foreach{n => if (n.isFactor && n.arity > 1) mqueue += n}
    }
    (mqueue ++ mqueue.reverse).iterator
  }
 */
