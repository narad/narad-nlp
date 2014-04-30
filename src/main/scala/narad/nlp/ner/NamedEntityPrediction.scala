package narad.nlp.ner

import narad.bp.structure.{Potential, FactorGraphBuilder}
import util.matching.Regex

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 4/10/14
 * Time: 6:56 PM
 */
trait NamedEntityPrediction {

  self: NamedEntityModel =>

  def addNamedEntityPrediction(fg: FactorGraphBuilder, pots: Array[Potential], slen: Int, maxWidth: Int, useSemiCRF: Boolean=true) = {
    val groups = pots.filter(_.name.startsWith("ner")).groupBy{p =>
      val INDICES_PATTERN(start, end) = p.name
      (start.toInt, end.toInt)
    }
    for (width <- 1 to maxWidth; start <- 0 until slen if start+width <= slen) {
      val end = start + width
      val gpots = groups((start, end))
      fg.addVariable("nerspanvar(%d,%d)".format(start, end), arity=2)
      fg.addUnaryFactor("nerspanvar(%d,%d)".format(start, end), "nerspanfac(%d,%d)".format(start, end), gpots(0))
      assert(gpots(0).name.contains("nerbracket"), "First pot in set was not for ner bracketing!")
      if (gpots.size > 1) {
        val arity = gpots.size-1
        fg.addVariable("nerlabelvar(%d,%d)".format(start, end), arity=arity)
        fg.addTable1Factor("nerlabelvar(%d,%d)".format(start, end), "nerlabelfac(%d,%d)".format(start, end), gpots.tail)
        fg.addEPUFactorByName("nerspanvar(%d,%d)".format(start, end),
          "nerlabelvar(%d,%d)".format(start, end),
          arity,
          "nerEPUfac(%d,%d)".format(start, end))
      }
    }
    if (useSemiCRF) fg.addSegmentationFactor(new Regex("nerspanvar"), slen=slen, maxWidth=maxWidth)
  }


  def addConnectionPrediction(fg: FactorGraphBuilder, pots: Array[Potential], slen: Int, cnerLabels: Array[String], maxWidth: Int) = {
    val groups = pots.filter(_.name.startsWith("agree")).groupBy{p =>
      val INDICES_PATTERN(start, end) = p.name
      (start.toInt, end.toInt)
    }
    for (width <- 2 to Math.min(slen, maxWidth); start <- 0 to (slen - width)) {
      val end = start + width
      if (groups.contains((start, end))) {
        val gpots = groups((start, end))
        val addImplies = false
        if (params.CONNECT_NP) {
          val iidx = fg.addVariable("agreevar(%d,%d)".format(start, end), 2)
          fg.addNandFactor(new Regex("""nerspanvar\(%d,%d\)""".format(start, end)),
            new Regex("""agreevar\(%d,%d\)""".format(start, end)),
            "agree(%d,%d)".format(start, end), gpots.head)
          val didxs = cnerLabels.map(c => fg.getVariableIndex("labelvar(%d,%d,%s)".format(start, end, c))).filter(_ >= 0)
          fg.addIsAtMost1FactorByIndex(iidx, didxs, "IsAtMost1Connect(%d,%d)".format(start, end))
        }
        else {
          if (addImplies) {
            fg.addImpliesFactor(new Regex("""nerspanvar\(%d,%d\)""".format(start, end)),
              new Regex("""labelvar\(%d,%d,%s\)""".format(start, end, "NP")),
              "agree(%d,%d)".format(start, end), gpots.head)
          }
          else {
            fg.addNandFactor(new Regex("""nerspanvar\(%d,%d\)""".format(start, end)),
              new Regex("""brackvar\(%d,%d\)""".format(start, end)),
              "agree(%d,%d)".format(start, end), gpots.head)
          }
        }
      }
    }
  }
}
