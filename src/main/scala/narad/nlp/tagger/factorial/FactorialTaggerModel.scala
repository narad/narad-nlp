package narad.nlp.tagger.factorial

import narad.bp.util.{Feature, PotentialExample}
import narad.bp.structure._
import collection.mutable.{HashMap, ArrayBuffer, HashSet, Map}
import java.io.FileWriter
import edu.stanford.nlp.international.morph.MorphoFeatures
import narad.bp.util.index.Index
import narad.io.conll.{CoNLLDatum, CoNLLReader}
import narad.bp.inference.InferenceOrder
import narad.bp.util.PotentialExample
import util.matching.Regex
import narad.nlp.tagger._

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 2/12/13
 * Time: 6:19 PM
 * To change this template use File | Settings | File Templates.
 */
class FactorialTaggerModel(params: FactorialTaggerParams) extends TaggerModel(params) {
  // t, k, l(t,k)
  val LABEL_FAC_PATTERN = """label\(([0-9]+),([0-9]+),([0-9]+)\)""".r
  // t, t+1, k, k, l(t,k), l(t+1,k)
  val CHAIN_FAC_PATTERN = """chain\(([0-9]+),([0-9]+),([0-9]+),([0-9]+),([0-9]+),([0-9]+)\)""".r
  // t, t, k, k+1, l(t,k), l(t,k+1)
  val SLICE_FAC_PATTERN = """slice\(([0-9]+),([0-9]+),([0-9]+),([0-9]+),([0-9]+),([0-9]+)\)""".r
  // i, j
  val DEPENDENCY_PATTERN = """\(([0-9]+),([0-9]+)\)""".r

  override def constructFromExample(ex: PotentialExample, pv: Array[Double]): ModelInstance = {
    val len    = ex.attributes.getOrElse("len", "-1").toInt
    val chains = ex.attributes.getOrElse("chains", "-1").toInt
//    val dependencies = DEPENDENCY_PATTERN.findAllIn(ex.attributes.getOrElse("dependencies", "")).matchData.foreach { m =>
//      (m.group(1).toInt, m.group(2).toInt)
//    }

    val pots = ex.exponentiated(pv)
    val labelPots = pots.filter(_.name.startsWith(params.LABEL_NAME)).groupBy { pot =>
      val LABEL_FAC_PATTERN(chain, slice, value) = pot.name
      (chain.toInt, slice.toInt)
    }
    val chainPots = pots.filter(_.name.startsWith(params.CHAIN_NAME)).groupBy { pot =>
      val CHAIN_FAC_PATTERN(chain1, chain2, slice1, slice2, value1, value2) = pot.name
      (chain1.toInt, chain2.toInt, slice1.toInt, slice2.toInt)
    }
    val slicePots = pots.filter(_.name.startsWith(params.SLICE_NAME)).groupBy { pot =>
      val SLICE_FAC_PATTERN(chain1, chain2, slice1, slice2, value1, value2) = pot.name
      (chain1.toInt, chain2.toInt, slice1.toInt, slice2.toInt)
    }

    val idxs = Array.ofDim[Int](len+1, len+1)
    val arities = Array.ofDim[Int](len+1, len+1)
    val fg = new FactorGraphBuilder(pots)
    for (slice <- 1 to len; chain <- 0 until chains) {
      if (labelPots.contains((chain, slice))) {
        idxs(chain)(slice) = fg.addTable1Variable("labelvar(%d,%d)".format(chain, slice),
                             "labelfac(%d,%d)".format(chain, slice),
                             labelPots((chain, slice)))
        arities(chain)(slice) = labelPots((chain, slice)).size
      }
    }
    // Add chain dependencies (between time i and i+1, etc)
    if (pots.exists(_.name.startsWith(params.CHAIN_NAME))) {
      for (chain <- 0 until chains; slice1 <- 1 until len; slice2 <- slice1+1 to len) {
        if (chainPots.contains((chain, chain, slice1, slice2))) {
          fg.addTable2FactorByIndex(idxs(chain)(slice1), idxs(chain)(slice2),
            arities(chain)(slice1), arities(chain)(slice2),
            "chainFac(%d,%d,%d,%d)".format(chain, chain, slice1, slice2),
            chainPots((chain, chain, slice1, slice2)))
        }
      }
    }
    // Add slice dependencies (between variables chain i and chain j and time t)
    if (pots.exists(_.name.startsWith(params.SLICE_NAME))) {
      for (slice <- 1 until len; chain1 <- 0 until chains; chain2 <- chain1 until chains if chain1 != chain2) {
        if (slicePots.contains((chain1, chain2, slice, slice))) {
          fg.addTable2FactorByIndex(idxs(chain1)(slice), idxs(chain2)(slice),
            arities(chain1)(slice), arities(chain2)(slice),
            "sliceFac(%d,%d,%d,%d)".format(chain1, chain2, slice, slice),
            slicePots((chain1, chain2, slice, slice)))
        }
      }
    }
    return new FactorialTaggerModelInstance(fg.toFactorGraph, ex)
  }
}

class FactorialTaggerModelInstance(graph: FactorGraph, ex: PotentialExample)
  extends TaggerModelInstance(graph, ex) with FactorialTaggerChainInference




















/*
  val slen   = ex.attributes.getOrElse("slen", "-1").toInt
    val bigram = ex.attributes.getOrElse("bigram", "false").toLowerCase.trim == "true"
    val oracle = ex.attributes.getOrElse("oracle", "false").toLowerCase.trim == "true"
    val useDependency = ex.attributes.getOrElse("dependency", "false").toLowerCase().trim == "true"
    val useBigrams = bigram || oracle
    //    System.err.println("bigram = " + useBigrams)
    //    System.err.println("Constructing (%s) Tagger...".format(if (useBigrams) "bigram" else "unigram"))
    val pots = ex.exponentiated(pv)
    //    System.err.println("%d pots found.".format(pots.size))
    val fg = new FactorGraphBuilder(pots)
    val pidxs = new HashSet[Int]


    // Unigram Model
    val arities = new Array[Int](slen+1)
    val ugroups = pots.filter(_.name.contains("ulabel")).groupBy{pot =>
      val glabelPattern(widx) = pot.name
      widx.toInt
    }
    /*
        for (idx <- 1 to slen) {
          val upots = ugroups(idx).filter(_.name.startsWith("ulabel"))
          val varName = "labelVar(%d)".format(idx)
          fg.addVariable(varName, arity=upots.size)
          fg.addNamed1Factor(varName, "labelFac(%d)".format(idx), upots)
          arities(idx) = upots.size
        }
    */
    //    System.err.println("arities: " + arities.mkString(", "))
    assert(ugroups.size == slen, "# of unary factors (%d) not equal to sentence length (%d).".format(ugroups.size, slen))
    for (ugroup <- ugroups) {
      val idx = ugroup._1.toInt
      val upots = ugroup._2.filter(_.name.startsWith("ulabel"))
      val varName = "labelVar(%d)".format(idx)
      fg.addVariable(varName, arity=upots.size)
      fg.addNamed1Factor(varName, "labelFac(%d)".format(idx), upots)
      arities(idx) = upots.size
    }

    // Bigram Model
    val bgroups = pots.filter(_.name.contains("blabel")).groupBy{pot =>
      val BIGRAM_PATTERN(i, j, misc) = pot.name
      (i.toInt, j.toInt)
    }
    if (useBigrams) {
      for (bgroup <- bgroups) {
        System.err.println(bgroup._1 + ": " + bgroup._2.size)
        val (v1, v2) = bgroup._1
        val varName1 = "labelVar(%s)".format(v1)
        val varName2 = "labelVar(%s)".format(v2)
        val arity1 = arities(v1.toInt)
        val arity2 = arities(v2.toInt)
        fg.addTable2Factor(varName1, varName2, arity1, arity2, "bigramFac(%s,%s)".format(v1, v2), bgroup._2)
      }
    }

    // Dependency Model
    //    val useDependency = true
    if (useDependency) {
      fg.addVariable("linkVar(1,4)", 2)
      fg.addTable1Factor("linkVar(1,4)", "linkFac(1,4)", pots.filter(_.name.startsWith("un")))

      val dgroups = pots.filter(_.name.contains("tlabel")).groupBy{pot =>
        val TRIGRAM_PATTERN(i, j, misc) = pot.name
        (i.toInt, j.toInt)
      }
      for (dgroup <- dgroups) {
        val (v1, v2) = dgroup._1
        val varName1 = "labelVar(%s)".format(v1)
        val varName2 = "labelVar(%s)".format(v2)
        val varName3 = "linkVar(%s,%s)".format(v1, v2)
        val arity1 = arities(v1.toInt)
        val arity2 = arities(v2.toInt)
        System.err.println(dgroup._2.size + " trigram pots found")
        fg.addTable3Factor(varName3, varName1, varName2,
          2, arity1, arity2, "trigramFac(%s,%s)".format(v1, v2), dgroup._2)
      }
    }

    if (bigram) {
      return new ChainTaggerModelInstance(fg.toFactorGraph, ex)
    }
    else {
      return new TaggerModelInstance(fg.toFactorGraph, ex)
    }
  }

*/


  /*
    val beliefs = instance.marginals
    val labels   = instance.ex.attributes.getOrElse("tags", "").split(" ")
    val tags = new ArrayBuffer[String]
    val groups = beliefs.filter(_.name.startsWith("ulabel")).groupBy{pot =>
      val labelPattern(widx, lidx) = pot.name
      widx
    }
    for (group <- groups.toArray.sortBy(_._1.toInt)) {
      val idx = group._1.toInt
      val pots = group._2
      System.err.println(pots.mkString("\n"))
      assert(!pots.isEmpty, "Pots for group %d are empty in decoding?".format(idx))
      val maxpot = narad.util.Functions.argmax[Potential](_.value, pots)
      //		System.err.println("max = " + maxpot)
      val labelPattern(i,j) = maxpot.name
      tags += labels(j.toInt)
      //tags += j //labels(j.toInt)
    }
    val out = new FileWriter("test.itagged", true)
    out.write(tags.mkString("\n") + "\n")
    out.write("\n")
    out.close()
  }
  */


