package narad.nlp.tagger
import narad.io.conll.CoNLLReader
import narad.util.{ArgParser, HashCounter}
import narad.stats.ConditionalProbabilityTable

object FrequencyTagger {
	
	def main(args: Array[String]) = {
		val options = new ArgParser(args)
		val trainFile = options.getString("--train.file")
		val testFile  = options.getString("--test.file")
		val tcounts = new HashCounter[String]
		val cpt = new ConditionalProbabilityTable[String]
    val trainReader = new CoNLLReader(trainFile)
		for (datum <- trainReader) {
			for (token <- datum.forms.zip(datum.postags)) {
				cpt.increment(token._1.toLowerCase, token._2)
				tcounts.increment(token._2)
			}
		}

		val mfta = tcounts.toList.sortBy(-1 * _._2).toArray
		val mft = mfta(0)._1
		System.err.println("Most Frequent Tag = " + mft)

    val testReader = new CoNLLReader(testFile)
		for (datum <- testReader) {
			for (token <- datum.forms.zip(datum.postags)) {
				val word = token._1.toLowerCase
				if (cpt.contains(word)) {
					println(cpt.mostProbable(word)._1)
				}
				else {
					println(mft)					
				}
			}
			println
		}
	}
}




/*
		val tcounts = new HashCounter[String]
		val wtcounts = new HashCounter[(String, String)]
		for (datum <- CoNLLReader.iterator(trainFile); token <- datum.forms.zip(datum.postags)) {
			tcounts.increment(token._2)
			wtcounts.increment(token)
		}
		println(tcounts.mkString("\n"))
		val mfta = tcounts.toList.sortBy(-1 * _._2).toArray
		val mft = mfta(0)._1
		println("MFT = " + mft)
	
		for (datum <- CoNLLReader.iterator(testFile)) {
			for (token <- datum.forms.zip(datum.postags)) {
	//			println(wtcounts.getOrElse("", mft))
			}
			println
		}
	}	
}
*/