package narad.nlp.trees
import narad.io.tree.TreebankReaderOptions

class TreeTransformer(options: TreebankReaderOptions) {

	def transformTree(tree: ConstituentTree): ConstituentTree = {
		var t = tree
		if (options.REMOVE_NONES)
			t = t.removeNones()
		if (options.REMOVE_UNARY_CHAINS)
			t = t.removeUnaryChains()
		if (options.BINARIZE)
			t = t.binarize()
		if (options.REMOVE_TOP) {
      if (t.getChildren.size == 1) t = t.getChildren.head
    }
		if (options.FIX_ROOT) {
      t = new ConstituentTree(NonterminalNode("TOP"), t.getChildren)
    }
    if (options.COARSEN_LABELS) {
      t = t.coarsenLabels
    }
		return t
	}

}
