package narad.nlp.parser.constituent
import narad.nlp.trees.{Tree, TreebankReader}
import narad.util.ArgParser

object ConstituentFeatures {

	def syntaxFeatures(tree: Tree, labels: Array[String], mode: String, prune: Boolean, prunelabels: Array[Array[String]], spanName: String = "brack", labelName: String = "spanLabel") = {
		val tokens = tree.tokens
		val length = tokens.length
		for ( width <- 2 to length; start <- 0 to (length - width)) {
			val end = start + width
			val features = ConstituentFeatureFactory.syntaxFeatures(tree, start, end)
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
			val features = ConstituentFeatureFactory.unaryFeatures(tree.removeUnaryChains, idx)
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


object Featurize{

	def main(args: Array[String]) : Unit = {
		val options			= new ArgParser(args)
		val treeFile 		= options.getString("--treebank")
		val ntermFile 	= options.getString("--nonterms")
		val featureFile	= options.getString("--feature.file")
		val mode 				= options.getString("--mode", "train")
		val features 		= options.getString("--features", "CM")
		val pruneFile		= options.getString("--prune.file")
		val correctOnly 	= options.getBoolean("--correct.only", false)
		val prune					= options.getBoolean("--prune", false) || pruneFile != null


		val labels = if (ntermFile == null) Array[String]() 
		else io.Source.fromFile(ntermFile).getLines.toArray.map(_.trim).distinct.filter(_ != "TOP")

		val vpots = if (pruneFile == null) Array[Array[String]]() 
		else io.Source.fromFile(pruneFile).getLines.map(_.split("\t")(1).split(" ").toArray).toArray

		System.err.println("Pruned labels (over %d length spans):".format(vpots.size))
		var vc = 0
		for (vpot <- vpots) { System.err.println(vc + "\t" + vpot.mkString(" ")); vc += 1 }

		val stime = System.nanoTime
		var elapsed = 0.0
		var count = 0
		for (t <- TreebankReader.iterator(treeFile, options)) {
			if (count % 10 == 0) { 
				elapsed = (System.nanoTime - stime)/1000000000.0
				System.err.println("...sentence %d,\t".format(count) + elapsed + " seconds.") 
			}
//			val tree = TreebankReader.transformTree(t, options)
			val tree = t
	System.err.println("Commented out important code that is being referenced in ConstituentFeatures.scala")
			val tokens = tree.tokens
			val length = tokens.size
			println("@slen\t%d".format(length))
			println("@bracks\t0 0 1 1")
			println("@words\t%s".format(tokens.map(_.word).mkString(" ")))
			println("@tags\t%s".format(tokens.map(_.pos).mkString(" ")))
			println("@grammar\t%s".format((labels.mkString(" "))))
			printFeatures(tree.removeTopNode.removeNones, labels, features, mode, prune, vpots, options)
			println
			count += 1
		}
		elapsed = (System.nanoTime - stime)/1000000000.0
		val avg     = elapsed / count
		System.err.println("Elapsed Extraction Time: " + elapsed + " seconds, average " + avg + "per sentence.")
	}				


	def printFeatures(tree: Tree, ctlabels: Array[String], features: String, mode: String, prune: Boolean, vpots: Array[Array[String]], options: ArgParser) = {
		val clabels = if (features contains "brack") Array[String]() else ctlabels
		val spanName = options.getString("--span.name", "brack")
		val labelName = options.getString("--label.name", "spanLabel")

		if (features contains "syntax") {
			var ftree = tree.binarize.removeUnaryChains
			ftree.annotateWithIndices(0)
			ConstituentFeatures.syntaxFeatures(ftree, clabels, mode, prune, vpots, spanName, labelName)
		}
		if (features contains "unary") {
			var ftree = tree
			ftree.annotateWithIndices(0)
			ConstituentFeatures.unaryFeatures(tree, clabels.filter(!_.contains("@"))) 
		}
	}
}
