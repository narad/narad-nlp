package narad.nlp.parser.constituent
//import narad.nlp.ner.
import narad.nlp.trees.{Token, Tree}
import narad.io.reader.TreebankReader
import narad.util.ArgParser
import scala.collection.mutable.ArrayBuffer


object Featurize{
	val STARTPOS = "START"
	val ENDPOS = "END"


	def main(args: Array[String]) : Unit = {
		val options			= new ArgParser(args)
		val treeFile 		= options.getString("--treebank")
		val ntermFile 	= options.getString("--nonterms")
		val featureFile	= options.getString("--feature.file")
		val mode 				= options.getString("--mode", "train")
		val features 		= options.getString("--features", "CCM").toLowerCase
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
			val tree = t
			val tokens = tree.tokens
			val length = tokens.size
			println("@slen\t%d".format(length))
			println("@words\t%s".format(tokens.map(_.word).mkString(" ")))
			println("@tags\t%s".format(tokens.map(_.pos).mkString(" ")))
			printFeatures(tree.removeTopNode.removeNones, labels, features, mode, prune, vpots, options)
			println
			count += 1
		}
		elapsed = (System.nanoTime - stime)/1000000000.0
		val avg     = elapsed / count
		System.err.println("Elapsed Extraction Time: " + elapsed + " seconds, average " + avg + "per sentence.")
	}				


	def printFeatures(tree: Tree, ctlabels: Array[String], features: String, mode: String, 
		                prune: Boolean, vpots: Array[Array[String]], options: ArgParser) = {
		val clabels = if (features contains "brack") Array[String]() else ctlabels
		val spanName   = options.getString("--span.name", "brack")
		val labelName  = options.getString("--label.name", "spanLabel")
		val uspanName  = options.getString("--unary.span.name", "unary")
		val ulabelName = options.getString("--unary.label.name", "unaryLabel")
		val testMode = false
		if (features contains "syntax") {
			var btree = tree.binarize.removeUnaryChains
			btree.annotateWithIndices(0)
			val window = 5
			val length = btree.tokens.length
			val tokens = pad(btree.tokens, window, window)
			for ( width <- 2 to length; start <- 0 to (length - width)) {
				val end = start + width
				val si = start + window
				val ei = end + window-1
				val spanFeatures = ConstituentFeatureFactory.syntaxSpanFeatures(tokens, si, ei, window=window)
				val labelFeatures = spanFeatures //++ ConstituentFeatureFactory.syntaxLabelFeatures(tokens, si, ei)
				val labelSet = btree.labels(start, end)
				println("%s(%d,%d)\t%s%s".format(spanName, start, end, if (labelSet.size > 0) "+" else "", spanFeatures.mkString(" ")))
				val labels = if (!prune) {
					ctlabels
				}
				else if (prune && width >= 25) {
					vpots(25)
				}
				else {
					vpots(width)
				}
				for (label <- labels) {
					val builder = new StringBuilder()
					for (f <- labelFeatures) builder.append(" " + label + "_" + f)
					println("%s%s(%d,%d)\t%s%s".format(labelName, label, start, end, if (labelSet contains label) "+" else "", builder.toString.trim))					
				}								
			}
		}

		if (features contains "unary") {
			val uterms = clabels.filter(!_.contains("@"))
			tree.annotateWithIndices(0)
			val tokens = tree.tokens
			val length = tokens.length
			for (idx <- 0 until length) {
				val features = ConstituentFeatureFactory.unaryFeatures(tree.removeUnaryChains, idx)
				val hasUnary = tree.containsUnarySpan(idx, idx+1)
				println("%s(%d,%d)\t%s%s".format(uspanName, idx, idx+1, if (hasUnary) "+" else "", features.map("U-%s".format(_)).mkString(" ")))
				var ccount = 0
				for (uterm <- uterms) {
					val correctLabel = tree.containsUnarySpan(idx, idx+1, uterm)
					if (correctLabel) {
						ccount += 1
					}
					val builder = new StringBuilder()
					for (f <- features) {
						builder.append(" [unary-" + uterm + "]-" + f)
					}
					println("%s%s(%d,%d)\t%s%s".format(ulabelName, uterm, idx, idx+1, if (correctLabel) "+" else "", builder.toString.trim))
				}
			}		
		}
		
		if (features contains "ner") {
			
		}
	}

def pad(array: Array[Token], spad: Int, epad: Int): Array[Token] = {
	val buffer = new ArrayBuffer[Token]
	val end = spad + epad + array.size
	for (i <- 0 until end){
		if (i < spad){
			buffer += Token("[START%d]".format(spad-i), STARTPOS)
		}
		else if (i >= array.size + spad){
			buffer += Token("[END%d]".format(1 + i - (array.size + spad)), ENDPOS)
		}
		else{
			buffer += array(i-spad)	
		}	
	}
	return buffer.toArray
}
}
