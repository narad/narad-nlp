package narad.nlp.disfluency

import narad.bp.structure.{Potential, FactorGraphBuilder, ModelInstance, FactorGraphModel}
import narad.nlp.parser.constituent.{TreebankStatistics, ConstituentLabelFeatures, ConstituentParserModel}
import narad.bp.util.PotentialExample
import collection.mutable.ArrayBuffer
import util.matching.Regex
import narad.io.disfluency.{DisfluencyReader, DisfluencyDatum}
import java.io.FileWriter
import narad.nlp.ling.{TaggedToken => Token}

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 3/23/13
 * Time: 11:54 PM
 * To change this template use File | Settings | File Templates.
 */

class DisfluencyModel(params: DisfluencyParams) extends FactorGraphModel[DisfluencyDatum] with DisfluencyFeatures with ConstituentLabelFeatures {
  val COPY_PATTERN        = """str-copy\(([0-9]+),([0-9]+),([0-9]+),([0-9]+)\)""".r
  val FILL_PAUSE_PATTERN  = """fp\(([0-9]+),([0-9]+)\)""".r
  val DIS_PATTERN         = """.*\(([0-9]+),([0-9]+),([0-9]+),([0-9]+)\)""".r

  val INDICES_PATTERN = """.*\(([0-9]+),([0-9]+).*""".r
  val LABEL_PATTERN1   = """spanLabel(.*)\(([0-9]+),([0-9]+)\).*""".r
  val LABEL_PATTERN2   = """.*\(([0-9]+),([0-9]+),([0-9]+).*""".r

  val BRACK_PATTERN       = """brack\(([0-9]+),([0-9]+)\)""".r
  val UNARY_PATTERN       = """unary\(([0-9]+),([0-9]+)\)""".r
  val UNARY_LABEL_PATTERN1 = """unaryLabel(.*)\(([0-9]+),([0-9]+)\)""".r
  val UNARY_LABEL_PATTERN2 = """unaryLabel\(([0-9]+),([0-9]+),(.+)\)""".r


  def constructFromExample(ex: PotentialExample, pv: Array[Double]): ModelInstance = {
    val pots = ex.exponentiated(pv)
    println(params.PREDICT_DISFLUENCY)
    println(params.PREDICT_SYNTAX)
    val fg = new FactorGraphBuilder(pots)
    if (params.PREDICT_DISFLUENCY) {
      addDisfluencyPrediction(fg, pots, ex)
    }
    if (params.PREDICT_SYNTAX) {
      System.err.println("Adding Syntax")
      addSyntacticPredictions(fg, pots, ex)
    }
    System.err.println(fg.toFactorGraph.toString)
    new ModelInstance(fg.toFactorGraph, ex)
  }

  def addDisfluencyPrediction(fg: FactorGraphBuilder, pots: Array[Potential], ex: PotentialExample) {
    val slen = ex.attributes.getOrElse("slen", "-1").toInt
    val pauseVarIndex = Array.ofDim[Int](slen+1, slen+1)
    for (pot <- pots) {
      pot.name match {
        case FILL_PAUSE_PATTERN(start, end) => {
          pauseVarIndex(start.toInt)(end.toInt) = fg.addVariable("pauseVar(%s,%s)".format(start, end), arity=2)
          fg.addUnaryFactor("pauseVar(%s,%s)".format(start, end), "pauseFac(%s,%s)".format(start, end), pot)
        }
        case _=>
      }
    }

    val idx = ex.attributes.getOrElse("ex", "-1").toInt
    //    System.err.println("Ex = " + idx)
    val copyVarIndex = Array.ofDim[Int](slen+1, slen+1, slen+1, slen+1)
    val copyPotIndex = pots.filter(_.name.startsWith("str-copy")).groupBy { pot =>
      val COPY_PATTERN(i, j, k, l) = pot.name
      (i.toInt, j.toInt, k.toInt, l.toInt)
    }

    val MAX_REPAR_WIDTH  = ex.attributes.getOrElse("maxrepar", "0").toInt
    val MAX_REPAIR_WIDTH = ex.attributes.getOrElse("maxrepair", "0").toInt
    val MAX_INT_WIDTH    = ex.attributes.getOrElse("maxint", "0").toInt
    val MAX_DISFLUENCY_WIDTH = MAX_REPAR_WIDTH + MAX_INT_WIDTH + MAX_REPAIR_WIDTH

    val MIN_REPAR_WIDTH  = ex.attributes.getOrElse("minrepar", "0").toInt
    val MIN_REPAIR_WIDTH = ex.attributes.getOrElse("minrepair", "0").toInt
    val MIN_INT_WIDTH    = ex.attributes.getOrElse("minint", "0").toInt
    val MIN_DISFLUENCY_WIDTH = MIN_REPAR_WIDTH + MIN_INT_WIDTH + MIN_REPAIR_WIDTH

    val FULL_COPY_NAME = "fullCopy"
    val PART_COPY_NAME = "partCopy"

    for (i <- 0 to slen-MIN_DISFLUENCY_WIDTH; j <- i+MIN_REPAR_WIDTH to i+MAX_REPAR_WIDTH) {
      //      println("iter: " + i + " " + j)
      for (k <- j+MIN_INT_WIDTH to j+MAX_INT_WIDTH; l <- k+MIN_REPAIR_WIDTH to k+MAX_REPAIR_WIDTH if l <= slen) {
        fg.addUnaryVariable("%sVar(%d,%d,%d,%d)".format(FULL_COPY_NAME, i, j, k, l),
          "%sFac(%d,%d,%d,%d)".format(FULL_COPY_NAME, i, j, k, l),
          copyPotIndex((i,j,k,l))(0))

        var numAnalyses = 0
        copyVarIndex(i)(j)(k)(l) = fg.addVariable("%sVar(%d,%d,%d,%d)".format(PART_COPY_NAME, i, j, k, l), 2)
        val analysisVarIdxs = linearGroupings(i,j,k,l).map { g =>
          numAnalyses += 1
          val analysisVarName = "analysisVar(%d,%d,%d,%d,%d)".format(i, j, k, l, numAnalyses)
          val aidx = fg.addVariable(analysisVarName, arity=2)          // FILL INDICES
          val tidxs = g.map{t => println("looking up " + t.toString()); copyVarIndex(t._1)(t._2)(t._3)(t._4)}
          if (tidxs.size > 0) println("TIDX: " + tidxs.mkString(", "))
          if (j == k) {
            fg.addHardImpliesFactorByIndex(aidx, tidxs.toArray, "implies-(%d,%d,%d,%d)-%d".format(i,j,k,l,aidx))
          }
          else {
            fg.addHardImpliesFactorByIndex(aidx, tidxs.toArray ++ Array(pauseVarIndex(j)(k)), "implies-(%d,%d,%d,%d)-%d".format(i,j,k,l,aidx))
          }
          aidx
        }
        fg.addIsAtMost1FactorByIndex(copyVarIndex(i)(j)(k)(l), analysisVarIdxs.toArray, "isAtMost-%d-%d-%d-%d".format(i,j,k,l))
      }
    }
  }


  def addSyntacticPredictions(fg: FactorGraphBuilder, pots: Array[Potential], ex: PotentialExample) {
    val slen = ex.attributes.getOrElse("slen", "-1").toInt
    val groups = pots.filter(!_.name.contains("unary")).groupBy{p =>
      val INDICES_PATTERN(start, end) = p.name
      (start.toInt, end.toInt)
    }
    var useLabels = true
    val brackIdxs = Array.ofDim[Int](slen+1, slen+1) //new ArrayBuffer[Int]()
    val labelIdxs = Array.fill[ArrayBuffer[Int]](slen+1, slen+1)(new ArrayBuffer[Int])
    for (width <- 2 to slen; start <- 0 to (slen - width)) {
      val end = start + width
      val gpots = groups((start, end))
      brackIdxs(start)(end) = fg.addVariable("brackvar(%d,%d)".format(start, end), arity=2)
      fg.addUnaryFactor("brackvar(%d,%d)".format(start, end), "brackfac(%d,%d)".format(start, end), gpots(0))
      if (gpots.size > 1) {
        useLabels = true
        for (gpot <- gpots.tail) {
          val LABEL_PATTERN1(label, s, e) = gpot.name
          labelIdxs(start)(end) += fg.addVariable("labelvar(%d,%d,%s)".format(start, end, label), arity=2)
          fg.addUnaryFactor("labelvar(%d,%d,%s)".format(start, end, label), "labelfac(%d,%d,%s)".format(start, end, label), gpot)
        }
      }
    }
    if (useLabels) {
      for (width <- 2 to slen; start <- 0 to (slen - width)) {
        val end = start + width
        //fg.addIsAtMost1Factor(new Regex("brackvar\\(%d,%d\\)".format(start, end)), new Regex("labelvar\\(%d,%d,.+\\)".format(start, end)), "isAtMost(%d,%d)".format(start, end))
        fg.addIsAtMost1FactorByIndices(brackIdxs(start)(end), labelIdxs(start)(end).toArray, "isAtMost(%d,%d)".format(start, end))
      }
    }
    fg.addCKYFactor(new Regex("brackvar"), slen=slen)
  }

  def linearGroupings(i: Int, j: Int, k: Int, l: Int):Seq[Seq[(Int, Int, Int, Int)]] = {
    if (i == 4 && j == 6 && k == 10 && l == 12) {
      val ab = new ArrayBuffer[Seq[(Int, Int, Int, Int)]]()
      for (ss1 <- linearGroupings2(i,j); ss2 <- linearGroupings2(k, l)) {
        if (ss1.size == ss2.size) {
          val abb = new ArrayBuffer[(Int, Int, Int, Int)]()
          for (i <- 0 until ss1.size) {
            abb += ((ss1(i)._1, ss1(i)._2, ss2(i)._1, ss2(i)._2))
          }
          if (abb.size > 1 || !(abb(0)._1 == i && abb(0)._2 == j && abb(0)._3 == k && abb(0)._4 == l))  {
            ab += abb.toSeq
          }
        }
      }
      // println("FOUND INDICES = " + ab.toArray.toSeq.mkString(", "))
      ab.toSeq
    }
    else {
      Seq[Seq[(Int, Int, Int, Int)]]() //(Seq((1,2,3,4)))
    }
  }

  def linearGroupings2(i: Int, j: Int): ArrayBuffer[ArrayBuffer[(Int, Int)]] = {
    if (i == j) {
      ArrayBuffer(ArrayBuffer())
    }
    else {
      val ab = new ArrayBuffer[ArrayBuffer[(Int, Int)]]()
      for (w <- 1 to j-i if (i+w <= j)) {
        for (lg <- linearGroupings2(i+w, j)) {
          ab += ((i, i+w) +: lg)
        }
      }
      ab
    }
  }


  def subsets(start: Int, end: Int, count: Int) :Seq[Seq[Int]] = (
    if (count == 0)
      List(Nil)
    else
      for(head <- start to end; tail <- subsets(head + 1, end, count -1))
      yield head +: tail
    )

  def decode(instance: ModelInstance): DisfluencyDatum = {
    System.err.println("Decoding...")
    val words = instance.ex.attributes.getOrElse("words", "").split(" ")
    val tags  = instance.ex.attributes.getOrElse("tags", "").split(" ")
    val disStr = words.zip(tags).map{case(w,t) => "%s/%s".format(w,t)}.mkString(" ")
    val dis = new DisfluencyDatum(disStr)
    println("MARGS:")
    instance.marginals.foreach(println(_))
    for (pot <- instance.marginals.filter(_.value > 0.5)) {
      pot.name match {
        case FILL_PAUSE_PATTERN(start, end) => {
          System.err.println("Found Fill Pause!")
          dis.setFillPause(start.toInt, end.toInt)
        }
        case DIS_PATTERN(i, j, k, l) => {
          dis.setReparandum(i.toInt, j.toInt)
          dis.setRepair(k.toInt, l.toInt)
          if (j != k) dis.setFillPause(j.toInt, k.toInt)
        }
        case _=> {}
      }
    }
    val out = new FileWriter("out.dps", true)
    out.write(dis.toString + "\n")
    out.close()
    val parser = new ConstituentParserModel(params)
    val tree = parser.decode(instance) //instance.ex.attributes.getOrElse("tree", "")
    val out2 = new FileWriter("out.mrg", true)
    out2.write(tree + "\n")
    out2.close()
    null.asInstanceOf[DisfluencyDatum]
  }

  def options = params

}