package narad.nlp.parse
import narad.util.ArgParser
import java.io.FileWriter
import scala.collection.mutable.{HashMap, HashSet}

object GrammarReader {
	
		def main(args: Array[String]) {
			var options = new ArgParser(args)
			read(options)
		}
		
		def printGrammar(trees: Array[Tree]) = {
			val goldGrammar = new HashSet[String]
			val binarizedGrammar = new HashSet[String]
			for(tree <- trees) {
				goldGrammar ++= tree.extractRules
				binarizedGrammar ++= tree.binarize.extractRules
			}
			val f1 = new FileWriter("nonterms.gold")
			f1.write(goldGrammar.map(rule => rule.substring(0, rule.indexOf("=>")-1)).toList.sort((a,b) => a < b).mkString("\n")) 
			f1.close
			val f2 = new FileWriter("nonterms.binarized")
			f2.write(binarizedGrammar.map(rule => rule.substring(0, rule.indexOf("=>")-1)).toList.sort((a,b) => a < b).mkString("\n")) 
			f2.close
			val f3 = new FileWriter("grammar.gold")  
			f3.write(goldGrammar.toList.sort((a,b) => a < b).mkString("\n"))
			f3.close
			val f4 = new FileWriter("grammar.binarized") 
			f4.write(binarizedGrammar.toList.sort((a,b) => a < b).mkString("\n"))
			f4.close
		}
		

		def extractWeightedGrammar(trees: Array[Tree]) = {
			val grammarCounts = new HashMap[String, Int]
			val parentCounts  = new HashMap[String, Int]
			for (tree <- trees; t <- tree if !t.isPreterminal) {
				val rule = Rule(t.label, t.children.map(_.label)).toString
				if (grammarCounts.contains(rule)) {
					grammarCounts(rule) += 1
				}
				else {
					grammarCounts(rule) = 1
				}
				if (parentCounts.contains(t.label)) {
					parentCounts(t.label) += 1
				}			
				else {
					parentCounts(t.label) = 1
				}
			}

			val rules = grammarCounts.keys.toArray
			val probs = rules.map{rule => val parent = rule.split(" ")(0);  Math.log(grammarCounts(rule).toDouble / parentCounts(parent).toDouble)}
			val mean = probs.foldLeft(0.0)(_+_) / probs.size.toDouble
			val sdevs = probs.map(p => Math.pow(p - mean, 2))
			val sd = Math.sqrt(sdevs.foldLeft(0.0)(_+_) / (probs.size - 1))
			val rprobs = probs.map(x => (x - mean) / sd)

			val out = new FileWriter("mle.grammar");
			out.write("\"x\"\n")
			for (i <- 0 until rules.size) {
				out.write("\"%s\" %f".format(rules(i), rprobs(i)) + "\n")
			}
			out.close
		}


		def read(options: ArgParser): Grammar = {
			val treeFile           = options.getString("--treebank")
			val shouldPrintGrammar = options.getBoolean("--printGrammar", false)
			val shouldMLE					 = options.getBoolean("--MLE", false)
			val shouldPruneSpans		 = options.getBoolean("--prune", false)
			val pruneFile						 = options.getString("--prune.file", "prune.pots")

			var trees = TreebankReader.read(treeFile, options)
			printGrammar(trees)
			return null.asInstanceOf[Grammar]
	}
	
}