package narad.nlp.tagger

import narad.bp.structure._
import narad.bp.inference.BeliefPropagation
import narad.bp.util.PotentialExample
import collection.mutable.{ArrayBuffer, HashSet}
import java.io.FileWriter
import narad.bp.structure.Potential
import narad.bp.util.PotentialExample

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 2/10/13
 * Time: 10:22 AM
 * To change this template use File | Settings | File Templates.
 */

class TaggerModel(params: TaggerParams) extends FactorGraphModel[MultiTaggedSentence] with TaggerFeatures with BeliefPropagation {

  val glabelPattern = """.*label\(([0-9]+),.+""".r
  val labelPattern  = """ulabel\(([0-9]+),(.+)\)""".r
  val blabelPattern = """blabel\(([0-9]+),(.+)\)""".r
  val BIGRAM_PATTERN = """blabel\(([0-9]+),([0-9]+),(.+)\)""".r
  val TRIGRAM_PATTERN = """tlabel\(([0-9]+),([0-9]+),(.+)\)""".r

  def constructFromExample(ex: PotentialExample, pv: Array[Double]): ModelInstance = { //(pots: Array[Potential], slen: Int, useBigrams: Boolean = false, useSyntax: Boolean = false): TaggerModel = {
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

  def decode(instance: ModelInstance): MultiTaggedSentence = {
    System.err.println("IN T DECODING!")
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
    null.asInstanceOf[MultiTaggedSentence]
  }

  def options = params
}

class TaggerModelInstance(graph: FactorGraph, ex: PotentialExample) extends ModelInstance(graph, ex)

class ChainTaggerModelInstance(graph: FactorGraph, ex: PotentialExample) extends TaggerModelInstance(graph, ex) with TaggerChainInference
