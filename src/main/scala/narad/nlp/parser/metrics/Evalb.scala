package narad.nlp.parser.metrics
import java.text.DecimalFormat
import narad.nlp.trees.ConstituentTree
import narad.io.util.ZippedReader
import narad.io.tree.TreebankReader
import narad.util.{ArgParser, HashCounter}
import narad.util.eval._
import scala.collection.mutable.{HashMap, HashSet}
import narad.util.StringOps
import narad.bp.optimize.EvalContainer
import javax.management.remote.rmi._RMIConnection_Stub

object EvalBContainer {
  val NON_WORDS = Array(".", ",")

  def construct(goldTree: ConstituentTree, testTree: ConstituentTree, verbose: Boolean = false): EvalBContainer = {
//    println("GOLD TREE: " + goldTree)
//    println("TEST TREE: " + testTree)
//    println
    val goldTokens = goldTree.tokens.toArray
    val testTokens = testTree.tokens.toArray
    val slen = goldTokens.size
    val goldSpans = goldTree.toSpans.filter(!_.isTerminal).filter(s => !(s.width == slen && s.label == "TOP")).toArray
    val testSpans = testTree.toSpans.filter(!_.isTerminal).filter(s => !(s.width == slen && s.label == "TOP")).toArray
//    println("TEST SPANS: " + testSpans.mkString(", "))
//    println("width = " + slen)

    val labels = (goldSpans ++ testSpans).map(_.label).distinct

    assert(goldTokens.size == testTokens.size, "Trees are not over sentences of the same length (%d gold vs %d test).".format(goldTokens.size, testTokens.size))
    val e = new EvalBContainer
    val taggedTokens = goldTokens.map(_.pos).zip(testTokens.map(_.pos)).filter(tpair => !NON_WORDS.contains(tpair._1))
    e("tags") = taggedTokens.filter(p => p._1 == p._2).size
    e("tokens") = slen
    e("words") = taggedTokens.size
    e("tagging accuracy") = e.count("tags") / e.count("words")


    val lfound = new HashSet[Int]
    val ufound = new HashSet[Int]
    for (gspan <- goldSpans; tidx <- 0 until testSpans.size) {
      val tspan = testSpans(tidx)
      if (gspan == tspan && !lfound.contains(tidx)) {
        e.increment("labeled correct")
        e.increment(gspan.label + " correct")
        e.increment("labeled correct width " + gspan.width)
        lfound += tidx
      }
      if (gspan.start == tspan.start && gspan.end == tspan.end && !ufound.contains(tidx)) {
        e.increment("unlabeled correct")
        e.increment("unlabeled correct width " + gspan.width)
        ufound += tidx
      }
    }
    e("gold comps") = goldSpans.size
    e("test comps") = testSpans.size
    goldSpans.foreach { gspan => e.increment("gold comps width " + gspan.width) }


    var ccount = 0
    for (tspan <- testSpans) {
      if (goldSpans.exists(_.crosses(tspan))) ccount += 1
    }
    e("crosses") = ccount
    if (ccount == 0) e.increment("no crosses")
    if (ccount <= 2) e.increment("<=2 crosses")


    for (label <- labels) {
      e("gold %s comps".format(label)) = goldSpans.filter(_.label == label).size
      e("test %s comps".format(label)) = testSpans.filter(_.label == label).size
    }


    val up  = e.count("unlabeled correct") / e.count("test comps")
    val ur  = e.count("unlabeled correct") / e.count("gold comps")
    val uf1 = if ((up + ur) == 0) 0 else 2 * ((up * ur) / (up + ur))
    e("up") = up
    e("ur") = ur
    e("uf1") = uf1

    val lp  = e.count("labeled correct") / e.count("test comps")
    val lr  = e.count("labeled correct") / e.count("gold comps")
    val lf1 = if ((lp + lr) == 0) 0 else 2 * ((lp * lr) / (lp + lr))
    e("lp") = lp
    e("lr") = lr
    e("lf1") = lf1
    if (lf1 == 1) e.increment("complete match")
    e
  }
}


class EvalBContainer extends HashCounter[String] with EvalContainer {

  def combine(that: EvalContainer): EvalBContainer = {
//    println("COMBINING")
    that match {
      case x: EvalBContainer => {
        val e = new EvalBContainer
        for (k <- this.keys) {
          e.increment(k, this(k))
        }
        for (k <- x.keys) {
          e.increment(k, x(k))
        }
        return e
      }
      case _ => {
        this
      }
    }
  }

  override def toString = {
    val lp  = count("labeled correct") / count("test comps")
    val lr  = count("labeled correct") / count("gold comps")
    val lf1 = if ((lp + lr) == 0) 0 else 2 * ((lp * lr) / (lp + lr))
    "(P=%f/R=%f) = %f".format(lp, lr, lf1) //lf1.toString
    //"%f\t%f\t%f".format(count("labeled correct"), count("gold comps"), count("test comps"))
  }
}


object Evalb {

  def main(args: Array[String]) = {
    val options = new ArgParser(args)
    val goldFile = options.getString("--gold.file")
    val testFile = options.getString("--test.file")
    val verbose  = options.getBoolean("--verbose", false)

    val gparses = TreebankReader.read(goldFile, options).toArray
    val tparses = TreebankReader.read(testFile, options).toArray

    assert(gparses.size == tparses.size, "Files not equal in length.")

    val labels = new HashSet[String]
    for (p <- (gparses ++ tparses)) p.toSpans.map(_.label).foreach(labels += _)

    var evals = gparses.zip(tparses).map(p => EvalBContainer.construct(p._1, p._2))
    var stats = evals.foldLeft(new EvalBContainer)(_.combine(_))

    val formatter = new DecimalFormat("0.00")
    println("  Sent.                        Matched  Bracket   Cross        Correct Tag")
    println(" ID  Len.  Stat. Recal  Prec.  Bracket gold test Bracket Words  Tags Accracy")
    println("============================================================================")
    var count = 1
    for (e <- evals) {
      println(Array(
        StringOps.pad(count.toString, 4, dir="LEFT"),
        StringOps.pad(e.count("tokens").toInt.toString, 5, dir="LEFT"),
        "    0",
        StringOps.pad(formatter.format(e.count("lr") * 100).toString, 8, dir="LEFT"),
        StringOps.pad(formatter.format(e.count("lp") * 100).toString, 7, dir="LEFT"),
        StringOps.pad(e.count("labeled correct").toInt.toString, 6, dir="LEFT"),
        StringOps.pad(e.count("gold comps").toInt.toString, 7, dir="LEFT"),
        StringOps.pad(e.count("test comps").toInt.toString, 5, dir="LEFT"),
        StringOps.pad(e.count("crosses").toInt.toString, 7, dir="LEFT"),
        StringOps.pad(e.count("words").toInt.toString, 7, dir="LEFT"),
        StringOps.pad(e.count("tags").toInt.toString, 6, dir="LEFT"),
        StringOps.pad(formatter.format(e.count("tagging accuracy") * 100).toString, 9, dir="LEFT")).mkString(""))
      count += 1
    }
    println("============================================================================")
    println(Array(
      StringOps.pad(formatter.format(100 * stats.count("labeled correct") / stats.count("gold comps")) , 22, dir="LEFT"),
      StringOps.pad(formatter.format(100 * stats.count("labeled correct") / stats.count("test comps")) , 7, dir="LEFT"),
      StringOps.pad(stats.count("labeled correct").toInt.toString, 7, dir="LEFT"),
      StringOps.pad(stats.count("gold comps").toInt.toString, 6, dir="LEFT"),
      StringOps.pad(stats.count("test comps").toInt.toString, 6, dir="LEFT"),
      StringOps.pad((evals.filter(_.count("no crosses") == 1).size).toString, 7, dir="LEFT"),
      //			StringOps.pad(stats.count("crosses").toInt.toString, 7, dir="LEFT"),
      StringOps.pad(stats.count("tokens").toInt.toString, 7, dir="LEFT"),
      StringOps.pad(stats.count("tags").toInt.toString, 6, dir="LEFT"),
      StringOps.pad(formatter.format(100 * stats.count("tags") / stats.count("tokens")) , 9, dir="LEFT")).mkString(""))
    //		println("                 78.97  83.85  18269 23135 21787    630  27639 26468    95.76")
    // crossing bracket score in summary not correct
    println("=== Summary ===")
    println()

    println("-- All --")
    println("Number of sentence        =" + StringOps.pad(evals.size.toString, 7, dir="LEFT"))
    println("Number of Error sentence  =      0")
    println("Number of Skip  sentence  =      0")
    println("Number of Valid sentence  =" + StringOps.pad(evals.size.toString, 7, dir="LEFT"))
    println("Bracketing Recall         =" + StringOps.pad(formatter.format(100 * stats.count("labeled correct") / stats.count("gold comps")), 7, dir="LEFT"))
    println("Bracketing Precision      =" + StringOps.pad(formatter.format(100 * stats.count("labeled correct") / stats.count("test comps")), 7, dir="LEFT"))
    println("Complete match            =" + StringOps.pad(formatter.format(100 * stats.count("complete match") / evals.size), 7, dir="LEFT"))
    println("Average crossing          =" + StringOps.pad(formatter.format(stats.count("crosses") / evals.size), 7, dir="LEFT"))
    println("No crossing               =" + StringOps.pad(formatter.format(100 * stats.count("no crosses") / evals.size), 7, dir="LEFT"))
    println("2 or less crossing        =" + StringOps.pad(formatter.format(100 * stats.count("<=2 crosses") / evals.size), 7, dir="LEFT"))
    println("Tagging accuracy          =" + StringOps.pad(formatter.format(100 * stats.count("tags") / stats.count("tokens")) , 7, dir="LEFT"))
    println()

    evals = gparses.zip(tparses).filter(_._1.length <= 40).map(p => EvalBContainer.construct(p._1, p._2))
    stats = evals.foldLeft(new EvalBContainer)(_.combine(_))
    println("-- len<=40 --")
    println("Number of sentence        =" + StringOps.pad(evals.size.toString, 7, dir="LEFT"))
    println("Number of Error sentence  =      0")
    println("Number of Skip  sentence  =      0")
    println("Number of Valid sentence  =" + StringOps.pad(evals.size.toString, 7, dir="LEFT"))
    println("Bracketing Recall         =" + StringOps.pad(formatter.format(100 * stats.count("labeled correct") / stats.count("gold comps")), 7, dir="LEFT"))
    println("Bracketing Precision      =" + StringOps.pad(formatter.format(100 * stats.count("labeled correct") / stats.count("test comps")), 7, dir="LEFT"))
    println("Complete match            =" + StringOps.pad(formatter.format(100 * stats.count("complete match") / evals.size), 7, dir="LEFT"))
    println("Average crossing          =" + StringOps.pad(formatter.format(stats.count("crosses") / evals.size), 7, dir="LEFT"))
    println("No crossing               =" + StringOps.pad(formatter.format(100 * stats.count("no crosses") / evals.size), 7, dir="LEFT"))
    println("2 or less crossing        =" + StringOps.pad(formatter.format(100 * stats.count("<=2 crosses") / evals.size), 7, dir="LEFT"))
    println("Tagging accuracy          =" + StringOps.pad(formatter.format(100 * stats.count("tags") / stats.count("tokens")) , 7, dir="LEFT"))
    println()

    val lp  = 100 * stats.count("labeled correct") / stats.count("test comps")
    val lr  = 100 * stats.count("labeled correct") / stats.count("gold comps")
    val lf1 = if ((lp + lr) == 0) 0 else 2 * ((lp * lr) / (lp + lr))
    println("Labeled F1                =" + StringOps.pad(formatter.format(lf1) , 7, dir="LEFT"))
    for (label <- labels.toArray.sortBy{l => -1 * (stats.count("%s correct".format(l)) / stats.count("test %s comps".format(l)))}) {
      val lcorrect = stats.count("%s correct".format(label))
      val ltest = stats.count("test %s comps".format(label))
      val lgold = stats.count("gold %s comps".format(label))
      val lprec = lcorrect / ltest
      val lrec = lcorrect / lgold
      val lf1 = if ((lprec + lrec) == 0) 0 else 2 * ((lprec *lrec) / (lprec + lrec))
      println("%s\t%d\t%d\t%d\t%f\t%f\t%f".format(label, lcorrect.toInt, ltest.toInt, lgold.toInt, lprec, lrec, lf1))
    }

    if (verbose) {
      val maxLen = 60
      println("Additional Statistics:")
      println("======================")
      println("Correctly Predicted and Labeled Spans at Widths:")
      for (i <- 2 to maxLen) {
        val cw = stats.count("labeled correct width " + i)
        val aw = stats.count("gold comps width " + i)
        println("  %d: (%f/%f) = %f".format(i, cw, aw, cw / aw))
      }
      println
      println("Correctly Predicted Spans at Widths:")
      for (i <- 2 to maxLen) {
        val cw = stats.count("unlabeled correct width " + i)
        val aw = stats.count("gold comps width " + i)
        println("  %d: (%f/%f) = %f".format(i, cw, aw, cw / aw))
      }
      println("Errors:")
      println("======================")
      gparses.zip(tparses).foreach { case(gparse, tparse) =>
        val slen = gparse.length
        val tokens = gparse.tokens.toArray
        val gspans = gparse.toSpans
        val tspans = tparse.toSpans
        for (w <- 1 to slen; i <- 0 to slen-w) {
          val j = i+w
          if (gparse.containsSpan(i, j) && tparse.containsSpan(i, j)) {
            val gspans = gparse.spansAt(i, j)
            val tspans = tparse.spansAt(i, j)
            if (tspans.size == 1 && gspans.size == 1 && gspans.head.label != tspans.head.label) {
              println("Labeled %s as %s spanning [%s]".format(gspans.head.label, tspans.head.label, tokens.slice(i, j).mkString(" ")))
            }
          }
        }
      }
    }
  }
}

















































// what spans are incorrect most?
// what are they incorrectly labeled as most?
// what are examples of these sentences?
// what length of spans are getting predicted correctly

//		if (verbose) {
//			for (tspan <- testSpans) {
//				if (gold)
//			}
//		}

/*		
    for (gspan <- goldSpans; tidx <- 0 until testSpans.size) {
      val tspan = testSpans(tidx)
      if (gspan == tspan){ //} && !correctLabeled.contains(tidx)) {
        correctLabeled += tidx
        e.increment(gspan.label + " correct")
      }
      if (gspan.start == tspan.start && gspan.end == tspan.end && !correctUnlabeled.contains(tidx)) correctUnlabeled += tidx

    }


    var ccount1 = 0
    var ccount2 = 0
    val ggspans = goldSpans
    val ttspans = testSpans
    for (tspan <- ttspans) {
      if (ggspans.exists(_.crosses(tspan))) ccount2 += 1
    }
    */

/*		

val up  = stats.count("unlabeled correct") / stats.count("test comps")
val ur  = stats.count("unlabeled correct") / stats.count("gold comps")
val uf1 = if ((up + ur) == 0) 0 else 2 * ((up * ur) / (up + ur))
println("Unlabeled Precision: %f".format(up))
println("Unlabeled Recall:    %f".format(ur))
println("Unlabeled F-Measure: %f".format(uf1))

val lp  = stats.count("labeled correct") / stats.count("test comps")
val lr  = stats.count("labeled correct") / stats.count("gold comps")
val lf1 = if ((lp + lr) == 0) 0 else 2 * ((lp * lr) / (lp + lr))
println("Labeled Precision: %f".format(lp))
println("Labeled Recall:    %f".format(lr))
println("Labeled F-Measure: %f".format(lf1))

println("Part-of-speech Accuracy: %f".format(stats.count("tags") / stats.count("tokens")))

for (label <- labels) {
val lcorrect = stats.count("%s correct".format(label))
val ltest = stats.count("test %s comps".format(label))
val lgold = stats.count("gold %s comps".format(label))
val lprec = lcorrect / ltest
val lrec = lcorrect / lgold
val lf1 = if ((lprec + lrec) == 0) 0 else 2 * ((lprec *lrec) / (lprec + lrec))
println("%s\t%d\t%d\t%d\t%f\t%f\t%f".format(label, lcorrect.toInt, ltest.toInt, lgold.toInt, lprec, lrec, lf1))
}

}
*/
