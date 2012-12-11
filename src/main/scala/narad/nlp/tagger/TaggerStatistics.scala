package narad.nlp.tagger
import narad.io.conll.CoNLLReader
import narad.util.{ArgParser, HashCounter}
import narad.stats.ConditionalProbabilityTable

object TaggerStatistics {
	
	def main(args: Array[String]) = {
		val options = new ArgParser(args)
		val conllFile = options.getString("--input.file")
		val rFile = options.getString("--output.file")
		val cpt = new ConditionalProbabilityTable[String]
    val reader = new CoNLLReader(conllFile)
		for (datum <- reader) {
			var prev = "START"
			for (tag <- datum.postags) {
				cpt.increment(prev, tag)
				prev = tag
			}
		}
//		val hm = HeatMap.constructFromCPT(cpt)
//		hm.writeToFile(new File(rFile))
	}
}