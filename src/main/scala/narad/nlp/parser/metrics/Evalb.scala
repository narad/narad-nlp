package narad.nlp.parser.metrics
import java.text.DecimalFormat
import narad.nlp.trees.{Tree, TreebankReader}
import narad.util.{ArgParser, HashCounter, ZippedReader}
import scala.collection.mutable.{HashMap, HashSet}

object EvalContainer {
	def construct(goldTree: Tree, testTree: Tree, verbose: Boolean = false): EvalContainer = {
		goldTree.annotateWithIndices(0)
		testTree.annotateWithIndices(0)
		val goldTokens = goldTree.tokens
		val testTokens = testTree.tokens
		val goldSpans = goldTree.getSpans.toArray
		val testSpans = testTree.getSpans.toArray

		val labels = (goldSpans ++ testSpans).map(_.label).distinct

		// what spans are incorrect most?
		// what are they incorrectly labeled as most?
		// what are examples of these sentences?
		// what length of spans are getting predicted correctly

		assert(goldTokens.size == testTokens.size, "Trees are not over sentences of the same length (%d gold vs %d test).".format(goldTokens.size, testTokens.size))
		val slen = goldTokens.size
		val e = new EvalContainer
		e("tags") = goldTokens.map(_.pos).zip(testTokens.map(_.pos)).filter(p => p._1 == p._2).size
		e("tokens") = slen
		e("tagging accuracy") = e.count("tags") / e.count("tokens")


	val lfound = new HashSet[Int]
	val ufound = new HashSet[Int]
	for (gspan <- goldSpans; tidx <- 0 until testSpans.size) {
		val tspan = testSpans(tidx)
			if (gspan == tspan && !lfound.contains(tidx)) {
				e.increment("labeled correct")
				e.increment(gspan.label + " correct")
				lfound += tidx
			}
			if (gspan.start == tspan.start && gspan.end == tspan.end && !ufound.contains(tidx)) {
				e.increment("unlabeled correct")
				ufound += tidx
			}			
	}
	e("gold comps") = goldSpans.size
	e("test comps") = testSpans.size


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


class EvalContainer extends HashCounter {

	def combine(that: EvalContainer): EvalContainer = {
		val e = new EvalContainer
		for (k <- this.keys) {
			e.increment(k, this(k))
		}
		for (k <- that.keys) {
			e.increment(k, that(k))
		}
		return e
	}
}


object Evalb {

	def main(args: Array[String]) = {
		val options = new ArgParser(args)
		val goldFile = options.getString("--gold.file")
		val testFile = options.getString("--test.file")

		val gparses = TreebankReader.read(goldFile, options).toArray
		val tparses = TreebankReader.read(testFile, options).toArray
		gparses.foreach(_.annotateWithIndices(0))
		tparses.foreach(_.annotateWithIndices(0))
		assert(gparses.size == tparses.size, "Files not equal in length.")

		val labels = new HashSet[String]
		for (p <- (gparses ++ tparses)) p.getSpans.map(_.label).foreach(labels += _)

		var evals = gparses.zip(tparses).map(p => EvalContainer.construct(p._1, p._2))
		var stats = evals.foldLeft(new EvalContainer)(_.combine(_))

		val formatter = new DecimalFormat("0.00")
		println("  Sent.                        Matched  Bracket   Cross        Correct Tag")
		println(" ID  Len.  Stat. Recal  Prec.  Bracket gold test Bracket Words  Tags Accracy")
		println("============================================================================")
		var count = 1
		for (e <- evals) {
			println(Array(
				pad(count.toString, 4, dir="LEFT"),
				pad(e.count("tokens").toInt.toString, 5, dir="LEFT"),
				"    0",
				pad(formatter.format(e.count("lr") * 100).toString, 8, dir="LEFT"),
				pad(formatter.format(e.count("lp") * 100).toString, 7, dir="LEFT"),
				pad(e.count("labeled correct").toInt.toString, 6, dir="LEFT"),
				pad(e.count("gold comps").toInt.toString, 7, dir="LEFT"),
				pad(e.count("test comps").toInt.toString, 5, dir="LEFT"),
				pad(e.count("crosses").toInt.toString, 7, dir="LEFT"),
				pad(e.count("tokens").toInt.toString, 7, dir="LEFT"),
				pad(e.count("tags").toInt.toString, 6, dir="LEFT"),
				pad(formatter.format(e.count("tagging accuracy") * 100).toString, 9, dir="LEFT")).mkString(""))
				count += 1			
			}
			println("============================================================================")
			println(Array(
				pad(formatter.format(100 * stats.count("labeled correct") / stats.count("gold comps")) , 22, dir="LEFT"),
				pad(formatter.format(100 * stats.count("labeled correct") / stats.count("test comps")) , 7, dir="LEFT"),
				pad(stats.count("labeled correct").toInt.toString, 7, dir="LEFT"),
				pad(stats.count("gold comps").toInt.toString, 6, dir="LEFT"),
				pad(stats.count("test comps").toInt.toString, 6, dir="LEFT"),
				pad((evals.filter(_.count("no crosses") == 1).size).toString, 7, dir="LEFT"),
				//			pad(stats.count("crosses").toInt.toString, 7, dir="LEFT"),
				pad(stats.count("tokens").toInt.toString, 7, dir="LEFT"),
				pad(stats.count("tags").toInt.toString, 6, dir="LEFT"),
				pad(formatter.format(100 * stats.count("tags") / stats.count("tokens")) , 9, dir="LEFT")).mkString(""))
				//		println("                 78.97  83.85  18269 23135 21787    630  27639 26468    95.76")
				// crossing bracket score in summary not correct
				println("=== Summary ===")
				println

				println("-- All --")
				println("Number of sentence        =" + pad(evals.size.toString, 7))
				println("Number of Error sentence  =      0")
				println("Number of Skip  sentence  =      0")
				println("Number of Valid sentence  =" + pad(evals.size.toString, 7))
				println("Bracketing Recall         =" + pad(formatter.format(100 * stats.count("labeled correct") / stats.count("gold comps")), 7, dir="LEFT"))
				println("Bracketing Precision      =" + pad(formatter.format(100 * stats.count("labeled correct") / stats.count("test comps")), 7, dir="LEFT"))
				println("Complete match            =" + pad(formatter.format(100 * stats.count("complete match") / evals.size), 7, dir="LEFT"))
				println("Average crossing          =" + pad(formatter.format(stats.count("crosses") / evals.size), 7, dir="LEFT"))
				println("No crossing               =" + pad(formatter.format(100 * stats.count("no crosses") / evals.size), 7, dir="LEFT"))
				println("2 or less crossing        =" + pad(formatter.format(100 * stats.count("<=2 crosses") / evals.size), 7, dir="LEFT"))
				println("Tagging accuracy          =" + pad(formatter.format(100 * stats.count("tags") / stats.count("tokens")) , 7, dir="LEFT"))		
				println

				evals = gparses.zip(tparses).filter(_._1.tokens.size <= 40).map(p => EvalContainer.construct(p._1, p._2))
				stats = evals.foldLeft(new EvalContainer)(_.combine(_))
				println("-- len<=40 --")
				println("Number of sentence        =" + pad(evals.size.toString, 7))
				println("Number of Error sentence  =      0")
				println("Number of Skip  sentence  =      0")
				println("Number of Valid sentence  =" + pad(evals.size.toString, 7))
				println("Bracketing Recall         =" + pad(formatter.format(100 * stats.count("labeled correct") / stats.count("gold comps")), 7, dir="LEFT"))
				println("Bracketing Precision      =" + pad(formatter.format(100 * stats.count("labeled correct") / stats.count("test comps")), 7, dir="LEFT"))
				println("Complete match            =" + pad(formatter.format(100 * stats.count("complete match") / evals.size), 7, dir="LEFT"))
				println("Average crossing          =" + pad(formatter.format(stats.count("crosses") / evals.size), 7, dir="LEFT"))
				println("No crossing               =" + pad(formatter.format(100 * stats.count("no crosses") / evals.size), 7, dir="LEFT"))
				println("2 or less crossing        =" + pad(formatter.format(100 * stats.count("<=2 crosses") / evals.size), 7, dir="LEFT"))
				println("Tagging accuracy          =" + pad(formatter.format(100 * stats.count("tags") / stats.count("tokens")) , 7, dir="LEFT"))
				println
				
				for (label <- labels.toArray.sortBy{l => -1 * (stats.count("%s correct".format(l)) / stats.count("test %s comps".format(l)))}) {
					val lcorrect = stats.count("%s correct".format(label))
					val ltest = stats.count("test %s comps".format(label))
					val lgold = stats.count("gold %s comps".format(label))
					val lprec = lcorrect / ltest
					val lrec = lcorrect / lgold
					val lf1 = if ((lprec + lrec) == 0) 0 else 2 * ((lprec *lrec) / (lprec + lrec))
					println("%s\t%d\t%d\t%d\t%f\t%f\t%f".format(label, lcorrect.toInt, ltest.toInt, lgold.toInt, lprec, lrec, lf1))
				}
			}

def pad(str: String, len: Int, dir: String="LEFT"): String = {
	val builder = new StringBuilder(str)
	dir match {
		case "LEFT" => {
			while (builder.size < len) {
				builder.insert(0, " ")
			}
		}
		case "RIGHT" => {
			while (builder.size < len) {
				builder.append(" ")
			}
		}
	}
	return builder.toString
}
}









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
