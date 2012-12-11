package narad.nlp.trees
import narad.util.ArgParser

class TreeTransformer(options: ArgParser) {
	val removeNones					= options.getBoolean("--remove.nones", true)
	val removeUnaryChains		= options.getBoolean("--remove.unaries", false)
	val binarize            = options.getBoolean("--binarize", false)
	val coarseTags 		 		  = options.getBoolean("--coarse.tags", true)
	val removeTop			      = options.getBoolean("--remove.top", false)
	val unifyNonterms       = options.getBoolean("--unify.nonterms", false)
	val fixRoot  					  = options.getBoolean("--fix.root", false)
	
	def transformTree(tree: Tree): Tree = {
		var t = tree
		if (removeNones)       
			t = t.removeNones
		if (removeUnaryChains) 
			t = t.removeUnaryChains
		if (binarize)          
			t = t.binarize
		if (removeTop)
			t = t.removeTop
		if (fixRoot)
			t.annotation += "label" -> "TOP"
		t.annotateWithIndices(0)
		return t
	}	
}
