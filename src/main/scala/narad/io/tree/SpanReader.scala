package narad.io.tree

import narad.nlp.trees._
import narad.util.ArgParser
import scala.collection.mutable.ArrayBuffer

object SpanReader {
	val bpdpBracketSpanPattern = """([0-9]+)\t([0-9]+)\t([0-9]+).*""".r
	val bpdpLabelSpanPattern   = """([0-9]+)\t([0-9]+)\t([0-9]+)\t(.*)""".r
	val featureBracketSpanPattern     = """brack\(([0-9]+),([0-9]+)\)\t\+.*""".r
	val featureLabelSpanPattern       = """spanLabel([^\(]+)\(([0-9]+),([0-9]+)\)\t\+.*""".r
	val wordsPattern = """@words\t(.*)""".r
	val tagsPattern = """@tags\t(.*)""".r
	val lenPattern = """@slen\t([0-9]+)""".r
	val tokenSubPattern = """\(([^ ]+) ([^\)]+)\)""".r
	
	def main(args: Array[String]) = {
		val options = new ArgParser(args)
		val filename = options.getString("--span.file")
		for (tree <- read(filename, options)) {
			println(tree)
		}
	}
	
	def read(filename: String, options: ArgParser): Array[ConstituentTree] = {
		val trees = new ArrayBuffer[ConstituentTree]
		val starts = io.Source.fromFile(filename).getLines().zipWithIndex.filter{case(line, i) => line.startsWith("@slen")}.map(_._2)
//		System.err.println("Starts:\n" + starts.mkString("\n"))
		val ends = io.Source.fromFile(filename).getLines().zipWithIndex.filter{case(line, i) => line.isEmpty}.map(_._2)
//		System.err.println("Ends:\n" + ends.mkString("\n"))
		val lines = io.Source.fromFile(filename).getLines().toArray
//		System.err.println("# lines = " + lines.size)
		for (s <- starts) {
			val end = ends.filter(_ > s).next() //find(_ > s)
			val slen = lines(s).split("\t")(1).trim.toInt
			val words = lines(s+1).split("\t")(1).trim.split(" ")
			val tags  = lines(s+2).split("\t")(1).trim.split(" ")
			val spans = new ArrayBuffer[Span]
			for (i <- s to end) {
				lines(i) match {
					case bpdpBracketSpanPattern(idx, ss, es) => spans += new Span(ss.toInt, es.toInt, "CST")
					case _=>
				}
			}
			val tree = TreeFactory.constructFromSpans(spans.toArray, slen)
			tree.annotateWithIndices()
			tree.setYield(words, tags)
			trees += tree
			
		}
		return trees.toArray //.iterator //null.asInstanceOf[Iterable[ConstituentTree]]
	}
}





















//		for (h <- header.findAllIn(io.Source.fromFile(filename).getLines.mkString("\n"))) {
//			println(h)
//		}
//		for (line <- io.Source.fromFile(filename).getLines) {
/*			
		} 
		if line.startsWith("@") || line.matches("\d+\t\d+\t\d+")) {
			println(line)
		}
		*/

/*
object SpanReader {
	val bpdpBracketSpanPattern = """([0-9]+)\t([0-9]+)\t([0-9]+).*""".r
	val bpdpLabelSpanPattern   = """([0-9]+)\t([0-9]+)\t([0-9]+)\t(.*)""".r
	val featureBracketSpanPattern     = """brack\(([0-9]+),([0-9]+)\)\t\+.*""".r
	val featureLabelSpanPattern       = """spanLabel([^\(]+)\(([0-9]+),([0-9]+)\)\t\+.*""".r
	val wordsPattern = """@words\t(.*)""".r
	val tagsPattern = """@tags\t(.*)""".r
	val lenPattern = """@slen\t([0-9]+)""".r
	val tokenSubPattern = """\(([^ ]+) ([^\)]+)\)""".r
	
	def main(args: Array[String]) = {
		val options = new ArgParser(args)
		val filename = options.getString("--span.file")
		for (tree <- read(filename, options)) {
			println(tree)
		}
	}
	
	def read(filename: String, options: ArgParser) = {
		val unbinarize = options.getBoolean("--unbinarize", false)
		val wrapTop 	 = options.getBoolean("--wrapTop", true)
		readSpanFile(filename, unbinarize, "CST", wrapTop)
	}
	
	def readSpanFile(filename: String, unbinarize: Boolean, defaultLabel: String = "CST", wrapTop: Boolean): Array[ConstituentTree] = {
		val trees = new ArrayBuffer[ConstituentTree]
		val binarySymbol = "@"
		for (chunk <- ChunkReader.read(filename)) {
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
			var tree = null.asInstanceOf[ConstituentTree]
			if (unbinarize) {
				tree = spansToTree(spans.filter(!_.label.contains(binarySymbol)).toArray, len)				
			}
			else {
				tree = spansToTree(spans.toArray, len)				
			}
			tree.annotateWithIndices()
			System.err.println("words = " + words.mkString(" "))
			System.err.println("tags = " + tags.mkString(" "))
			tree.setYield(words, tags)
			trees += tree
		}
		return trees.toArray
	}	


	def spansToTree(spans: Array[Span], slen: Int): ConstituentTree = {
		return TreeFactory.buildTree(label="TOP", 
							children=findChildren(spans.sortBy(_.width).reverse, 0, slen))
	}

	def findChildren(spans: Array[Span], start: Int, end: Int): Array[ConstituentTree] = {
		val children = new ArrayBuffer[ConstituentTree]
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
*/