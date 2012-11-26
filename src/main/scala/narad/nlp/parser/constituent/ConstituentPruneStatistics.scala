/*
package narad.nlp.parser.constituent
import narad.util.ArgParser
import narad.nlp.trees.Tree
import narad.io.reader.TreebankReader
import scala.collection.mutable.HashSet

class ParserStatistics(labels: HashSet[String], unaries: HashSet[String], vpots: Array[HashSet[String]]) {
	
	def constituentLabels: Iterator[String] = {
		labels.iterator
	}
	
	def constituentLabelsOfSize(size: Int): Iterator[String] = {
		vpots(size).iterator
	}
	
	def unaryLabels: Iterator[String] = {
		unaries.iterator
	}
}


object PruneStatistics {
	
	def main(args: Array[String]) = {
		var options = new ArgParser(args)
		val treebankFile = options.getString("--treebank")
		val maxSpan      = options.getInt("--max.span", 25)
		val stats = construct(TreebankReader.read(treebankFile, options), maxSpan)
	}

	def construct(trees: Iterator[Tree], maxSpan: Int): ParserStatistics = {
		val vpots = Array.fill[HashSet[String]](maxSpan+1)(new HashSet[String])
		val labels  = new HashSet[String]
		val unaries = new HashSet[String]
		for (ctree <- trees) {
			val btree = ctree.binarize.removeUnaryChains
			ctree.annotateWithIndices()
			btree.annotateWithIndices()
			for (cspan <- ctree.getSpans) {
				if (cspan.isUnary) {
					unaries += cspan.label
				}
				else {
					labels += cspan.label
				}
			}
			for (bspan <- btree.getSpans) {
				labels += bspan.label
				if (bspan.width > maxSpan) {
					vpots.last += bspan.label
				}
				else {
					vpots(bspan.width) += bspan.label
				}
			}
		}
		new ParserStatistics(labels, unaries, vpots)
	}	
}

	/*
	def construct(trees: Iterator[Tree], maxSpan: Int): ParserStatistics = {
//		val trees = TreebankReader.read(treebankFile, options)
		// Extract nonterminal sets by span width
		val labels = new HashSet[String]
		val vpots = Array.fill[HashSet[String]](maxSpan+1)(new HashSet[String])
		for (tree <- trees) {
			tree.annotateWithIndices(0)
			labels += tree.label
			for (subtree <- tree) {
				val width = subtree.width
				labels += subtree.label
				if (width > maxSpan) {
					vpots.last += subtree.label
				}
				else {
					vpots(width) += subtree.label
				}
			}
		}
		// Add labels to gaps across spans
		for (i <- 1 until vpots.size) {
			if (vpots(i).isEmpty) vpots(i) ++= vpots(i-1)
		}
		// Add any labels present in larger spans to smaller spans
		for (i <- vpots.size-1 to 0 by -1) {
			for (j <- i to 0 by -1) {
				vpots(j) ++= vpots(i)				
			}
		}
		// Print statistics
		for (i <- 0 until vpots.size) {
			println("%d\t%s".format(i, vpots(i).mkString(" ")))
		}
		new ParserStatistics(labels, vpots)
	}
}
*/
*/