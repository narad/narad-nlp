package narad.nlp.parser.constituent

import narad.bp.structure._
import narad.bp.inference.BeliefPropagation
import narad.bp.util.PotentialExample
import collection.mutable.ArrayBuffer
import narad.nlp.trees.{ConstituentTreeFactory, Span, ConstituentTree}
import java.io.FileWriter
import narad.bp.structure.Potential
import narad.nlp.trees.Span
import narad.bp.util.PotentialExample
import narad.nlp.ling.TaggedToken
import math._

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 4/10/13
 * Time: 6:16 PM
 * To change this template use File | Settings | File Templates.
 */
class ConstituentParserModel(params: ConstituentParserParams) extends FactorGraphModel[ConstituentTree] with BeliefPropagation
    with ConstituentBracketFeatures with ConstituentParserPrediction with ConstituentParserDecoding {

  def constructFromExample(ex: PotentialExample, pv: Array[Double]): ModelInstance = {
    val slen   = ex.attributes.getOrElse("slen", "-1").toInt
    val pots = ex.exponentiated(pv)
    val fg = new FactorGraphBuilder(pots)
    if (params.MIN_PARSE_LEN > 2) {
      val hash = addBracketPrediction(fg, pots, slen, params.TREE_FACTOR)
      if (params.PREDICT_LABELS) {
        addLabelPrediction(fg, pots, slen, hash)
      }
    }
    if (params.PREDICT_UNARIES) {
      addUnaryPrediction(fg, pots, slen)
    }
    if (params.MARGINALIZATION) {
      return new LatentBinarizationConstituentParserModelInstance(fg.toFactorGraph, ex)
    }
    else {
      return new ConstituentParserModelInstance(fg.toFactorGraph, ex)
    }
  }


  def decode(instance: ModelInstance): ConstituentTree = {
//    println("in decoding")
    val slen    = instance.ex.attributes.getOrElse("slen", "-1").toInt
    val words   = instance.ex.attributes.getOrElse("words", "").trim.split(" ")
    val tags    = instance.ex.attributes.getOrElse("tags", "").trim.split(" ")
    val beliefs = instance.marginals
    val spans = if (slen > params.MIN_PARSE_LEN) {
      labelDecoding(beliefs, slen) ++ unaryDecoding(beliefs)
    }
    else {
      unaryDecoding(beliefs)
    }
    val tree = if (params.UNBINARIZE) {
      ConstituentTreeFactory.constructFromSpans(spans.toArray.filter(s => !s.label.contains("@")), slen, words, tags)
    }
    else {
      ConstituentTreeFactory.constructFromSpans(spans.toArray, slen, words, tags)
    }
    if (params.OUTPUT_FILE != null) {
      val out = new FileWriter(params.OUTPUT_FILE, true)
      out.write(tree.toString + "\n")
      out.close()
    }
    tree
  }

  def options = params
}

class ConstituentParserModelInstance(graph: FactorGraph, ex: PotentialExample)
  extends ModelInstance(graph, ex) with ConstituentParserInferenceOrder {

  override def isExact = true

}

class LatentBinarizationConstituentParserModelInstance(graph: FactorGraph, ex: PotentialExample)
  extends ConstituentParserModelInstance(graph, ex) {

  override def clampedFactors = {
    println("Latent Binarization")
    graph.factors.filter{ f => (f.name.startsWith("labelfac") && f.isCorrect)|| (f.name.startsWith("brack") && f.isCorrect) }.iterator
  }
}


















//        fg.addVariable("unaryvar(%d,%d)".format(start, end), arity=2)
//  fg.addUnaryFactor("unaryvar(%d,%d)".format(start, end), "unaryfac(%d,%d)".format(start, end), upots(0))
//          labelIdxs(start)(end) += fg.addVariable("unaryLabelvar(%d,%d,%s)".format(start, end, label), arity=2)
//          fg.addUnaryFactor("unaryLabelvar(%d,%d,%s)".format(start, end, label), "unaryLabelfac(%d,%d,%s)".format(start, end, label), gpot)
//          fg.addIsAtMost1Factor(new Regex("unaryvar\\(%d,%d\\)".format(start, end)), new Regex("unaryLabelvar\\(%d,%d,.+\\)".format(start, end)), "isAtMost(%d,%d)".format(start, end))
