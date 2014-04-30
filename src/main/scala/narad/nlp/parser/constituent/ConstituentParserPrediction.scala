package narad.nlp.parser.constituent

import narad.bp.structure.{Potential, FactorGraphBuilder}
import collection.mutable.ArrayBuffer
import narad.nlp.trees.Span

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 8/30/13
 * Time: 6:25 PM
 */
trait ConstituentParserPrediction {
  private val INDICES_PATTERN = """.*\(([0-9]+),([0-9]+).*""".r
  private val LABEL_PATTERN1   = """spanLabel(.*)\(([0-9]+),([0-9]+)\).*""".r
  private val LABEL_PATTERN2   = """.*\(([0-9]+),([0-9]+),([0-9]+).*""".r

  private val BRACK_PATTERN       = """brack\(([0-9]+),([0-9]+)\)""".r
  private val UNARY_PATTERN       = """unary\(([0-9]+),([0-9]+)\)""".r
  private val UNARY_LABEL_PATTERN1 = """unaryLabel(.*)\(([0-9]+),([0-9]+)\)""".r
  private val UNARY_LABEL_PATTERN2 = """unaryLabel\(([0-9]+),([0-9]+),(.+)\)""".r


  def addBracketPrediction(fg: FactorGraphBuilder, pots: Array[Potential], slen: Int, treeFactor: Boolean = true): Array[Array[Int]] = {
    val groups = pots.filter(_.name.startsWith("brack")).groupBy{p =>
      val INDICES_PATTERN(start, end) = p.name
      (start.toInt, end.toInt)
    }
    val brackIdxs = Array.tabulate(slen+1, slen+1)( (x,y) => -1) //Array.ofDim[Int](slen+1, slen+1) //new ArrayBuffer[Int]()
    for (width <- 2 to slen; start <- 0 to (slen - width)) {
      val end = start + width
      val gpots = groups((start, end))
      brackIdxs(start)(end) = fg.addUnaryVariable("brackvar(%d,%d)".format(start, end),
        "brackfac(%d,%d)".format(start, end),
        gpots.head)
    }
    if (treeFactor) {
      fg.addCKYFactorByIndices(brackIdxs.toArray.flatten.filter(_ >= 0), slen=slen)
    }
    brackIdxs
  }

  def addLabelPrediction(fg: FactorGraphBuilder, pots: Array[Potential], slen: Int, brackIdxs: Array[Array[Int]]) {
    val groups = pots.filter(p => p.name.startsWith("brack") || p.name.startsWith("spanLabel")).groupBy{p =>
      val INDICES_PATTERN(start, end) = p.name
      (start.toInt, end.toInt)
    }
    val labelIdxs = Array.fill[ArrayBuffer[Int]](slen+1, slen+1)(new ArrayBuffer[Int])
    for (width <- 2 to slen; start <- 0 to (slen - width)) {
      val end = start + width
      val gpots = groups((start, end))
      if (gpots.size > 1) {
        for (gpot <- gpots.tail) {
          val LABEL_PATTERN1(label, s, e) = gpot.name
          labelIdxs(start)(end) += fg.addUnaryVariable("labelvar(%d,%d,%s)".format(start, end, label),
            "labelfac(%d,%d,%s)".format(start, end, label),
            gpot)
        }
        fg.addIsAtMost1FactorByIndex(brackIdxs(start)(end), labelIdxs(start)(end).toArray, "isAtMost(%d,%d)".format(start, end))
      }
    }
  }

  def addUnaryPrediction(fg: FactorGraphBuilder, pots: Array[Potential], slen: Int) {
    val ugroups = pots.filter(_.name.contains("unary")).groupBy{p =>
      val INDICES_PATTERN(start, end) = p.name
      (start.toInt, end.toInt)
    }
    val brackIdxs = Array.ofDim[Int](slen+1, slen+1) //new ArrayBuffer[Int]()
    val labelIdxs = Array.fill[ArrayBuffer[Int]](slen+1, slen+1)(new ArrayBuffer[Int])
    for (start <- 0 until slen) {
      val end = start + 1
      val upots = ugroups((start, end))
      brackIdxs(start)(end) = fg.addUnaryVariable("unaryvar(%d,%d)".format(start, end),
        "unaryfac(%d,%d)".format(start, end),
        upots.head)
      if (upots.size > 1) {
        for (gpot <- upots.tail) {
          val UNARY_LABEL_PATTERN1(label, s, e) = gpot.name
          labelIdxs(start)(end) += fg.addUnaryVariable("unaryLabelvar(%d,%d,%s)".format(start, end, label),
            "unaryLabelfac(%d,%d,%s)".format(start, end, label),
            gpot)
        }
        fg.addIsAtMost1FactorByIndex(brackIdxs(start)(end), labelIdxs(start)(end).toArray, "isAtMost(%d,%d)".format(start, end))
      }
    }
  }
}
