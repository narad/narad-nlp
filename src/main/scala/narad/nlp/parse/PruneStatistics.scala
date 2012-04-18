package narad.nlp.parse
import narad.util.ArgParser
import scala.collection.mutable.HashSet

object PruneStatistics {
	
	def main(args: Array[String]) = {
		var options = new ArgParser(args)
		val treebankFile = options.getString("--treebank")
		val maxSpan      = options.getInt("--max.span", 25)
		
		val trees = TreebankReader.read(treebankFile, options)
		// Extract nonterminal sets by span width
		val vpots = Array.fill[HashSet[String]](maxSpan+1)(new HashSet[String])
		for (tree <- trees) {
			tree.annotateWithIndices(0)
			for (subtree <- tree) {
				val width = subtree.width
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
	}
}