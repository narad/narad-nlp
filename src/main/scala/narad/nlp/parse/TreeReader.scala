package narad.nlp.parse

import java.io._
import narad.util._
import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet}

object TreeReader {
	

	def printSentences(trees: Array[Tree]) = {
		val out = new FileWriter("sentences");
		out.write(trees.map(_.tokens.map(_.word).mkString(" ")).mkString("\n"))
		out.close
	}




	def printSpans(trees: Array[Tree]) {
		var count = 1
		for (tree <- trees) {
			tree.annotateWithIndices(0)
			for (t <- tree.filter(!_.isLeaf)) {
				println("%d\t%d\t%d\t%s".format(count, t.start, t.end, t.label))
			}
			println
			count += 1
		}
	}

	def printUnarySpans(trees: Array[Tree]) {
		var count = 1
		for (tree <- trees) {
			tree.annotateWithIndices(0)
			for (t <- tree.filter(_.isUnary)) {
				println("%d\t%d\t%d\t%s".format(count, t.start, t.end, t.label))
			}
			println
			count += 1
		}
	}



	def transformTree(tree: Tree, options:ArgParser): Tree = {
		val removeNones					= options.getBoolean("--remove.nones", true)
		val removeUnaryChains		= options.getBoolean("--remove.unaries", false)
		val binarize            = options.getBoolean("--binarize", false)
		val coarseTags 		 		  = options.getBoolean("--coarse.tags", true)
		var t = tree
		if (removeNones)       t = t.removeNones
		if (removeUnaryChains) t = t.removeUnaryChains
		if (binarize)          t = t.binarize
		return t
	}




	def read(filename: String): Array[Tree] = read(new ArgParser(Array("--treebank", filename)))

	def read(filename: String, options: ArgParser): Array[Tree] = {
		read(new ArgParser(Array("--treebank", filename))).map(t => transformTree(t, options))
	}

	def main(args: Array[String]) {
		var options = new ArgParser(args)
		read(options)
	}
	
	
	def read(options: ArgParser): Array[Tree] = {
		val treeFile      = options.getString("--treebank")
		val spanFile      = options.getString("--span.file")
		val sentenceFile  = options.getString("--sentence.file")
		val unaryFile     = options.getString("--unary.file")
		val useLabels     = options.getBoolean("--use.labels", false)
		val removeTop			= options.getBoolean("--remove.top", false)

		val unifyNonterms      = options.getBoolean("--unify.nonterms", false)
		val shouldPrintSentences = options.getBoolean("--print.sentences", false)
		val shouldPrintStats     = options.getBoolean("--print.stats", false)
		val shouldPrintTrees     = options.getBoolean("--print.trees", false)
		var shouldPrintSpans		 = options.getBoolean("--print.spans", false)
		val shouldPrintNewlines  = options.getBoolean("--print.newlines", false)
		val shouldPrintTokens    = options.getBoolean("--print.tokens", false)
		val min = options.getInt("--min", 0)
		val max = options.getInt("--max", 99999)
		

		var trees = null.asInstanceOf[Array[Tree]]
		if (spanFile != null) {
			trees = SpanReader.read(spanFile, options)
		}
		else if (treeFile != null) {
			trees = TreebankReader.read(treeFile, options)
		}

		if (removeTop) {
			trees = trees.map(_.removeTop)
		}

		trees = trees.filter(_.tokens.size >= min)
		trees = trees.filter(_.tokens.size <= max)
		trees.foreach(_.annotateWithIndices(0))

		if (options.getBoolean("--fix.berkeley", false)) {
			trees.foreach(_.annotation += "label" -> "TOP")
		}
		
		trees = trees.map(t => transformTree(t, options))
		if (shouldPrintTrees) {
			trees.foreach(println(_))			
		}
		if (shouldPrintSentences) {
			trees.foreach{t => println(t.tokens.map(_.word).mkString(" "))}
		}
		trees
	}
}


object TreebankReader {
	val tokenPattern = """\(([^ \(]+) ([^ \)]+)\).*""".r
	val constPattern = """\(([^ ]+) .*""".r
	val emptyPattern = """\( .*""".r
		
	def iterator(filename: String, options: ArgParser, defaultLabel: String="TOP") = narad.util.SexpReader.read(filename, parseExpression(_, options, defaultLabel=defaultLabel))

	def read(filename: String, options: ArgParser): Array[Tree] = {
		val text = "(DOC %s)".format(io.Source.fromFile(filename).getLines.mkString)
		val defaultLabel	 = options.getString("--default.label", "TOP")
		parseExpression(text, defaultLabel=defaultLabel).children //, coarseTags).children
	}
	
	def coarsenLabel(label: String): String = {
		var l = label
		if (l.contains("-"))
		l = l.substring(0, l.indexOf("-"))
		if (l.contains("="))
		l = l.substring(0, l.indexOf("="))
		return l
	}
	
	
	// TODO - implement transformations into tree iterator style reader
	def parseExpression(str: String, options: ArgParser = null.asInstanceOf[ArgParser], defaultLabel: String = "TOP"): Tree = {
		val coarseTags = true
		str match {
			case tokenPattern(tag, word) => TreeFactory.buildTree(label=tag, word=word)
			case constPattern(label) => {
				val children = subexpressions(str).map(parseExpression(_, defaultLabel=defaultLabel)).toArray
				if (coarseTags)
				return TreeFactory.buildTree(label=coarsenLabel(label), children=children)
				//					return transformTree(TreeFactory.buildTree(label=coarsenLabel(label), children=children), options)
				else
				//					return transformTree(TreeFactory.buildTree(label=label, children=children), options)
				return TreeFactory.buildTree(label=label, children=children)
			}
			case emptyPattern() => {
				val children = subexpressions(str).map(parseExpression(_, defaultLabel=defaultLabel)).toArray				
				return TreeFactory.buildTree(label=defaultLabel, children=children)
			}
			case entry => {
				if (str != null)
				System.err.println("Not recognized: %s".format(str))
				return null.asInstanceOf[Tree]
			}
		}
	}
	
	def subexpressions(str: String, ldelim: Char='(', rdelim: Char=')'): Array[String] = {
		val subs = new ArrayBuffer[String]
		var count = -1; var start = 0
		str.zipWithIndex.foreach { case(letter, index) =>
			if (letter == ldelim) {
				count += 1
				if (count == 1)
				start = index
			}
			else if (letter == rdelim) {
				count -= 1
				if (count == 0)
				subs += str.substring(start, index+1)
			}
		}
		subs.toArray
	}
	
}

object SpanReader {
	val bpdpBracketSpanPattern = """([0-9]+)\t([0-9]+)\t([0-9]+).*""".r
	val bpdpLabelSpanPattern   = """([0-9]+)\t([0-9]+)\t([0-9]+)\t(.*)""".r
	val featureBracketSpanPattern     = """brack\(([0-9]+),([0-9]+)\)\t\+.*""".r
	val featureLabelSpanPattern       = """spanLabel([^\(]+)\(([0-9]+),([0-9]+)\)\t\+.*""".r
	val wordsPattern = """@words\t(.*)""".r
	val tagsPattern = """@tags\t(.*)""".r
	val lenPattern = """@slen\t([0-9]+)""".r
	val tokenSubPattern = """\(([^ ]+) ([^\)]+)\)""".r
	
	
	def read(filename: String, options: ArgParser) = {
		val unbinarize = options.getBoolean("--unbinarize", false)
		val wrapTop 	 = options.getBoolean("--wrapTop", true)
		readSpanFile(filename, unbinarize, "CST", wrapTop)
	}
	
	def readSpanFile(filename: String, unbinarize: Boolean, defaultLabel: String = "CST", wrapTop: Boolean): Array[Tree] = {
		val trees = new ArrayBuffer[Tree]
		val binarySymbol = "@"
		for (chunk <- narad.util.ChunkReader.read(filename)) {
			var words = Array[String]()		   
			var tags  = Array[String]()
			var spans = new ArrayBuffer[Span]
			var len = -1
			for (line <- chunk.split("\n")) {
				line match {
					case lenPattern(lenString) => len = lenString.toInt
					case wordsPattern(wordString) => words = wordString.split(" ")
					case tagsPattern(tagString) => tags = tagString.split(" ")
					case bpdpLabelSpanPattern(index, left, right, label) => spans += new Span(left.toInt, right.toInt, label)
					case bpdpBracketSpanPattern(index, left, right) => spans += new Span(left.toInt, right.toInt, "CST")
					case default => None
				}
			}
			var tree = null.asInstanceOf[Tree]
			if (unbinarize) {
				tree = spansToTree(spans.filter(!_.label.contains(binarySymbol)).toArray, len)				
			}
			else {
				tree = spansToTree(spans.toArray, len)				
			}
			tree.annotateWithIndices()
			tree.setYield(words, tags)
			trees += tree
		}
		return trees.toArray
	}	


	def spansToTree(spans: Array[Span], slen: Int): Tree = {
		return TreeFactory.buildTree(label="TOP", 
							children=findChildren(spans.sortBy(_.width).reverse, 0, slen))
	}

	def findChildren(spans: Array[Span], start: Int, end: Int): Array[Tree] = {
		val children = new ArrayBuffer[Tree]
		val max = findMaxSpan(spans, start, end)
		max match {
			case None => return Array.fill(end-start)(TreeFactory.buildTree(label="TAG", children=Array()))
			case Some(maxspan) => {
				if (maxspan.start > start) {
					val leftChildren = findChildren(spans, start, maxspan.start)
					if (leftChildren.size > 0) children ++= leftChildren
				}
				val cchildren = findChildren(spans.filter(_ != maxspan), maxspan.start, maxspan.end)
				children += TreeFactory.buildTree(label=maxspan.label, children=cchildren)
				if (maxspan.end < end) {
					val rightChildren = findChildren(spans, maxspan.end, end)
					if (rightChildren.size > 0) children ++= rightChildren	
				}
				return children.toArray	
			}
		}
	}

	def findMaxSpan(spans: Array[Span], start: Int, end: Int): Option[Span] = {
		for (span <- spans) {
			if (span.start >= start && span.end <= end) return Some(span)
		}
		return None
	}
	
	
}

