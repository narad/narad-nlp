package narad.nlp.parse

object TreebankStatistics {
	
	
	def printStatistics(trees: Array[Tree]) {
		var unCount = 0
		var unTop = 0
		var unLeaf = 0
		var all = 0
		for (tree <- trees; subtree <- tree) {
			all += 1
			if (subtree.isUnary) {
				unCount += 1
				println("%s -> %s".format(subtree.label, subtree.children.mkString(",")))
			}
			if (subtree.isUnary && subtree.isRoot)
			unTop  += 1
			if (subtree.isUnary && subtree.children.size > 0 && subtree.children(0).isLeaf)
			unLeaf += 1
		}
		println("There are %d unary spans.".format(unCount))
		println("%d of them are top level, %d are leaves, leaving %d for the body".format(unTop, unLeaf, unCount - (unTop+unLeaf)))
		println("There are %d spans in total, meaning unaries account for %f percent of all spans.".format(all, (unCount * 1.0 / all)))
		println("%d & %d & %d & %d & %f".format(unTop, unCount - (unTop+unLeaf), unLeaf, all, (unCount * 1.0 / all)))
	}


}