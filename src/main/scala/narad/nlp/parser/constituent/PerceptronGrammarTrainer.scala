/*
package narad.nlp.parser.constituent
import narad.nlp.trees._
import narad.util.ArgParser
import narad.io.tree.TreeReader
import scala.collection.mutable.HashMap

object PerceptronGrammarTrainer {
	val rulePattern = """\"([^\"= ]+) => ([^\"]+)\" (.*)""".r
	
	def incrementCounts(tree: Tree, counts: HashMap[String, Int], backoff: Boolean) = {
		for (t <- tree if !t.isPreterminal) {
			val parent = t.label
			val children = t.children.map(_.label)
			val rule = Rule(parent, children).toString
			if (counts.contains(rule)) counts(rule) += 1 else counts(rule) = 1
			if (backoff && children.size == 2) {
				val pbackoff = "@ => %s %s".format(children(0), children(1))
				val lcbackoff = "%s => @ %s".format(parent, children(1))
				val rcbackoff = "%s => %s @".format(parent, children(0))
				if (counts.contains(pbackoff)) counts(pbackoff) += 1 else counts(pbackoff) = 1
				if (counts.contains(lcbackoff)) counts(lcbackoff) += 1 else counts(lcbackoff) = 1
				if (counts.contains(rcbackoff)) counts(rcbackoff) += 1 else counts(rcbackoff) = 1
			}
		}		
	}
	
	def calculateUpdates(testTrees: Array[Tree], trueTrees: Array[Tree], oldParams: HashMap[String, Double], rate: Double, backoff: Boolean, verbose: Boolean): HashMap[String, Double] = {

		// Collect counts
		val trueCounts = new HashMap[String, Int]
		val testCounts = new HashMap[String, Int]			
		for (i <- 0 until testTrees.size) {
			incrementCounts(trueTrees(i), trueCounts, backoff)
			incrementCounts(testTrees(i), testCounts, backoff)
		}

		if (verbose) println("\"x\"")
		val updates = new HashMap[String, Double]
		for (rule <- trueCounts.keys) {
			val weight = oldParams.getOrElse(rule, 0.0) + (trueCounts(rule) - testCounts.getOrElse(rule, 0)) * rate
			updates(rule) = weight
			if (verbose) println("\"%s\" %f".format(rule, weight))
		}
		updates
	}
	
	def readGrammarParams(grammarFile: String): HashMap[String, Double] = {
		val oldParams = new HashMap[String, Double]
		if (grammarFile != null) {
			for (line <- io.Source.fromFile(grammarFile).getLines if line != "\"x\"") {
				val rulePattern(parent, childrenStr, weight) = line
				val children = childrenStr.split(" ")
				val ruleStr = "%s => %s".format(parent, childrenStr)
				oldParams(ruleStr) = weight.toFloat
			}
		}
		oldParams
	}

	def main(args: Array[String]) = {
		val options = new ArgParser(args)
		assert(options.getString("--true.trees") != null && options.getString("--test.trees") != null, "Options --true.trees and --test.trees were not both specified.")
		val trueTrees = TreeReader.read(options.getString("--true.trees"), options)
		val testTrees = TreeReader.read(options.getString("--test.trees"), options)
		assert(trueTrees.size == testTrees.size, "# of true trees does not match # of test trees.")
		val rate = options.getDouble("--rate", 0.0001) // / trueTrees.size.toDouble)
		val backoff = options.getBoolean("--backoff", true)
		val verbose = options.getBoolean("--verbose", true)
		val grammarFile = options.getString("--grammar.file")
		val oldParams = readGrammarParams(grammarFile)
		calculateUpdates(testTrees, trueTrees, oldParams, rate, backoff, verbose)
	}
}
*/