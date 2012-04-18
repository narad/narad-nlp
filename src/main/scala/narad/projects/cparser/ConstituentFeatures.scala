package narad.projects.cparser
import narad.nlp.parse._
import narad.util.ArgParser

object ConstituentFeatures {

	def syntaxFeatures(tree: Tree, labels: Array[String], mode: String, prune: Boolean, prunelabels: Array[Array[String]], spanName: String = "brack", labelName: String = "spanLabel") = {
		val tokens = tree.tokens
		val length = tokens.length
		for ( width <- 2 to length; start <- 0 to (length - width)) {
			val end = start + width
			val features = FeatureFactory.syntaxFeatures(tree, start, end)
			val labelSet = tree.labels(start, end)
			println("%s(%d,%d)\t%s%s".format(spanName, start, end, if (labelSet.size > 0) "+" else "", features.mkString(" ")))
			if (prune) {
				val wlabels = if (width >= 25) prunelabels(25) else prunelabels(width)
				for (label <- wlabels) {
					println("%s%s(%d,%d)\t%s%s".format(labelName, label, start, end, if (labelSet contains label) "+" else "", (Array(label) ++ features.map("%s_%s".format(label, _))).mkString(" ")))					
				}								
			}
			else {
				for (label <- labels) {
					println("%s%s(%d,%d)\t%s%s".format(labelName, label, start, end, if (labelSet contains label) "+" else "", (Array(label) ++ features.map("%s_%s".format(label, _))).mkString(" ")))					
				}								
			}
		}
	}

	def unaryFeatures(tree: Tree, uterms: Array[String], testMode: Boolean=false, spanName: String = "unary", labelName: String = "unaryLabel") = {
		tree.annotateWithIndices(0)
		val tokens = tree.tokens
		val length = tokens.length
		for (idx <- 0 until length) {
			val features = FeatureFactory.unaryFeatures(tree.removeUnaryChains, idx)
			if (testMode) {
				println("unary(%d,%d)\t%s".format(idx, idx+1, features.mkString(" ")))			
				for (uterm <- uterms) {
					println("unaryLabel%s(%d,%d)\t%s".format(uterm, idx, idx+1, (Array(uterm) ++ features.map("%s_%s".format(uterm, _))).mkString(" ")))						
				}
			}
			else {				
				val hasUnary = tree.containsUnarySpan(idx, idx+1)
				println("%s(%d,%d)\t%s%s".format(spanName, idx, idx+1, if (hasUnary) "+" else "", features.map("U-%s".format(_)).mkString(" ")))
				var ccount = 0
				for (uterm <- uterms) {
					val correctLabel = tree.containsUnarySpan(idx, idx+1, uterm)
					if (correctLabel) {
						ccount += 1
					}
					println("%s%s(%d,%d)\t%s%s".format(labelName, uterm, idx, idx+1, if (correctLabel) "+" else "", (Array("U-" + uterm) ++ features.map("%s_%s".format("U-" + uterm, _))).mkString(" ")))
				}
			}
		}			
	}
}
