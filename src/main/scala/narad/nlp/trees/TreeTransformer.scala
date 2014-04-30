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
			t = t.binarize(options.BINARIZE_MODE)
		if (options.REMOVE_TOP) {
      if (t.getChildren.size == 1) t = t.removeTop
    }
		if (options.FIX_ROOT) {
      t = new ConstituentTree(NonterminalNode("TOP"), t.getChildren)
    }
    if (options.COARSEN_LABELS) {
      t = t.coarsenLabels
    }
    if (options.UNIFY_NONTERMS) {
      t = t.nodemap(n => {
        if (n.isNonterminal) new NonterminalNode("X")
        else n
      })
    }
		return t
	}

}
