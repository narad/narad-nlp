package narad.nlp.parser.constituent

import narad.bp.structure.{ModelInstance, Potential}
import narad.nlp.trees.{ConstituentTreeFactory, ConstituentTree, Span}
import collection.mutable.ArrayBuffer
import java.io.FileWriter

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 8/30/13
 * Time: 6:27 PM
 */
trait ConstituentParserDecoding {
  private val INDICES_PATTERN = """.*\(([0-9]+),([0-9]+).*""".r
  private val LABEL_PATTERN1   = """spanLabel(.*)\(([0-9]+),([0-9]+)\).*""".r
  private val LABEL_PATTERN2   = """.*\(([0-9]+),([0-9]+),([0-9]+).*""".r

  private val BRACK_PATTERN       = """brack\(([0-9]+),([0-9]+)\)""".r
  private val UNARY_PATTERN       = """unary\(([0-9]+),([0-9]+)\)""".r
  private val UNARY_LABEL_PATTERN1 = """unaryLabel(.*)\(([0-9]+),([0-9]+)\)""".r
  private val UNARY_LABEL_PATTERN2 = """unaryLabel\(([0-9]+),([0-9]+),(.+)\)""".r

  def decodeTree(instance: ModelInstance, params: ConstituentParserParams): ConstituentTree = {
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

  def labelDecoding(beliefs: Array[Potential], slen: Int): Array[Span] = {
 //   println("Beliefs:\n" + beliefs.mkString("\n"))
    val spans = new ArrayBuffer[Span]
    val labelGroups = beliefs.filter(_.name.contains("spanLabel")).groupBy{p =>
      val INDICES_PATTERN(start, end) = p.name
      (start.toInt, end.toInt)
    }
    val ckybeliefs = ckyBrackets(beliefs.filter(_.name.startsWith("brack(")), slen).groupBy{ p =>
      val INDICES_PATTERN(start, end) = p.name
      (start.toInt, end.toInt)
    }
//    println("Label Group:")
//    println(labelGroups.keys.mkString("\n"))
    for (width <- 2 to slen; start <- 0 to (slen - width)) {
      val end = start + width
      if (ckybeliefs((start, end))(0).value > 0.5) {
        val label = if (labelGroups.contains((start, end))) {
          maxLabel(labelGroups((start, end)))
        }
        else {
          "X" // A Default Label
        }
        spans += new Span(start.toInt, end.toInt, label)
      }
    }
    spans.toArray
  }

  def unaryDecoding(beliefs: Array[Potential]): Array[Span] = {
    val spans = new ArrayBuffer[Span]
    val unaryGroups = beliefs.filter(_.name.contains("unaryLabel")).groupBy{p =>
      val INDICES_PATTERN(start, end) = p.name
      (start.toInt, end.toInt)
    }
    beliefs.filter(p => p.name.contains("unary(") && p.value > 0.5).foreach { unary =>
      val UNARY_PATTERN(start, end) = unary.name
      spans += new Span(start.toInt, end.toInt, maxLabel(unaryGroups((start.toInt, end.toInt))), height=1)
    }
    spans.toArray
  }

  def ckyBrackets(pots: Array[Potential], slen: Int): Array[Potential] = {
    //    System.err.println("%d pots in cky-bracks.".format(pots.size))
    assert(!pots.exists(!_.name.startsWith("brack")), "There is a potential in ckyBrackets that is not a bracket potential.")
    val beta  = Array.ofDim[Double](slen+1, slen+1)
    val split = Array.ofDim[Int](slen+1, slen+1)
    val brack = Array.ofDim[Boolean](slen+1, slen+1)
    val scores = pots.map(_.value)
    //    val sp = scores

    //    System.err.println("POTS:\n" + pots.mkString("\n"))
    val groups = pots.filter(_.name.contains("brack")).groupBy{p =>
      val INDICES_PATTERN(start, end) = p.name
      (start.toInt, end.toInt)
    }
    //        System.err.println("GROUPS =\n" + groups.toArray.mkString("\n"))

    for (w <- 2 until slen; i <- 0 to slen-w) {
      val k = i + w
      var inside = Double.NegativeInfinity
      var best = 0
      for (j <- i+1 until k) {
        val s = beta(i)(j) + beta(j)(k)
        if (s > inside) {
          inside = s
          best = j
        }
      }
      val sp1 = groups((i, k))(0) //pots.filter(_.name == "brack(%d,%d)".format(i,k))(0)
      //			println("Brack(%d,%d) = " + sp)
      beta(i)(k) = inside + sp1.value
      split(i)(k) = best
    }
    var inside = Double.NegativeInfinity
    var best = 0
    for (j <- 1 until slen) {
      val s = beta(0)(j) + beta(j)(slen)
      if (s > inside) {
        inside = s
        best = j
      }
    }
    beta(0)(slen) = inside
    split(0)(slen) = best
    ckyBacktrace(0, slen, split, brack)
    //		var op = out // ???
    for (w <- 2 until slen; i <- 0 to slen-w) {
      val k = i + w
      var sp2 = groups((i, k))(0) //pots.filter(_.name == "brack(%d,%d)".format(i,k))(0)
      sp2.value = if (brack(i)(k)) 1.0 else 0.0
      //			op += 1
    }
    groups((0, slen))(0).value = 1
    //    pots.filter(_.name == "brack(%d,%d)".format(0,slen))(0).value = 1
    //		return beta(0)(slen)
    return pots
  }

  def ckyBacktrace(i: Int, k: Int, split: Array[Array[Int]], brack: Array[Array[Boolean]]): Int = {
    val j = split(i)(k)
    if (j > (i+1)) {
      brack(i)(j) = true
      //			println("backtrace-1(%d,%d)".format(i, j))
      ckyBacktrace(i, j, split, brack)
    }
    if (j < (k-1)) {
      brack(j)(k) = true
      //			println("backtrace-2(%d,%d)".format(j, k))
      ckyBacktrace(j, k, split, brack)
    }
    return 1
  }

  def maxLabel(pots: Array[Potential]): String = {
    // 		println("label potentials size = %d".format(pots.size) + "\n" + pots.mkString("\n"))
    var max = pots(0) //Double.NegativeInfinity
    for (pot <- pots) {
      if (pot.value > max.value) {
        max = pot
      }
    }
    //    println("max pot = " + max)
    if (max.name.contains("unary")) {
      val UNARY_LABEL_PATTERN1(label, start, end) = max.name
      return label
    }
    else {
      val LABEL_PATTERN1(label, start, end) = max.name
      return label
    }
  }
}
