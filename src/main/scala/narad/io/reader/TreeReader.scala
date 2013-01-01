package narad.io.reader
import java.io._
import narad.nlp.trees._
import narad.util._
import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet}

class TreeReader(filename: String, options: ArgParser) extends TreebankReader(filename, options) {

}


/*
object TreeReader {
	
	def read(options: ArgParser): Array[ConstituentTree] = {
		val treeFile      = options.getString("--treebank")
		val spanFile      = options.getString("--span.file")
		val sentenceFile  = options.getString("--sentence.file")
		val unaryFile     = options.getString("--unary.file")
		val setLabels     = options.getString("--set.labels")
		val useLabels     = options.getBoolean("--use.labels", false)
		val removeTop			= options.getBoolean("--remove.top", false)

		val shouldPrintSentences = options.getBoolean("--print.sentences", false)
		val shouldPrintTrees     = options.getBoolean("--print.trees", false)
		val shouldPrintTokens    = options.getBoolean("--print.tokens", false)
		val min = options.getInt("--min", 0)
		val max = options.getInt("--max", 999999999)
		
		assert(treeFile != null || spanFile != null, "Neither tree file (--treebank) nor span file (--span.file) options specfied.")
		var trees = if (spanFile != null) {
			SpanReader.read(spanFile, options)
		}
		else {
			TreebankReader.read(treeFile, options).toArray
		}


		trees = trees.filter(_.tokens().size >= min)
		trees = trees.filter(_.tokens().size <= max)
		trees.foreach(_.annotateWithIndices(0))

		val transformer = new TreeTransformer(options)
		trees = trees.map(t => transformer.transformTree(t))
		
		if (setLabels != null) {
			trees.foreach(_.setLabels(setLabels))
		}

		if (shouldPrintTrees) {
			trees.foreach(println(_))			
		}
		if (shouldPrintSentences) {
			trees.foreach{t => println(t.tokens().map(_.word).mkString(" "))}
		}
		trees
	}

	def read(filename: String): Array[ConstituentTree] = {
		read(new ArgParser(Array("--treebank", filename)))
	}
	
	def read(filename: String, options: ArgParser): Array[ConstituentTree] = {
		val transformer = new TreeTransformer(options)
		read(new ArgParser(Array("--treebank", filename))).map(t => transformer.transformTree(t))
	}

	def main(args: Array[String]) {
		var options = new ArgParser(args)
		read(options)
	}
}

             */