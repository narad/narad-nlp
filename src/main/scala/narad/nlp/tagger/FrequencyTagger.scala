package narad.nlp.tagger
import narad.io.conll.CoNLLReader
import narad.util.{ArgParser, HashCounter}
import narad.stats.ConditionalProbabilityTable
import collection.mutable.HashMap
import java.io.FileWriter

object FrequencyTagger {
	
	def main(args: Array[String]) = {
		val options = new ArgParser(args)
		val trainFile = options.getString("--train.file")
		val testFile  = options.getString("--test.file")
//		val tcounts = new HashCounter[String]
    val attributes = Array("CASE", "COARSE", "FINE")
    val cpts = new HashMap[String, ConditionalProbabilityTable[String]]
    val tcounts = new HashMap[String, HashCounter[String]]
    for (attr <- attributes) {
      cpts(attr) = new ConditionalProbabilityTable[String]
      tcounts(attr) = new HashCounter[String]
    }

//		val cpt = new ConditionalProbabilityTable[String]
    val trainReader = new CoNLLReader(trainFile)
		for (datum <- trainReader) {
      for (attr <- attributes) {
        for (i <- 1 to datum.slen) {
          cpts(attr).increment(datum.word(i).toLowerCase.trim, datum.getAttribute(i, attr).trim)
          tcounts(attr).increment(datum.getAttribute(i, attr))
        }
      }
/*
			for (token <- datum.forms.zip(datum.postags)) {
				cpt.increment(token._1.toLowerCase, token._2)
				tcounts.increment(token._2)
			}
*/
    }

//    println(cpts("CASE").toString)

    val mfts = new HashMap[String, String]
    for (attr <- attributes) {
      val mfta = tcounts(attr).toList.sortBy(-1 * _._2).toArray
      mfts(attr) = mfta(0)._1
      System.err.println("Most Frequent Tag(%s) = %s (%f)".format(attr, mfts(attr), mfta(0)._2))
    }

    val outs = new HashMap[String, FileWriter]
    for (attr <- attributes) {
      outs(attr) = new FileWriter("mft." + attr)
    }

    val testReader = new CoNLLReader(testFile)
		for (datum <- testReader) {
      for (attr <- attributes) {
        for (i <- 1 to datum.slen) {
          val word = datum.word(i).toLowerCase.trim
          if (cpts(attr).containsEvent(word)) {
            System.out.println("Found word!" + word)
            outs(attr).write(cpts(attr).mostProbable(word)._1 + "\n")
          }
          else {
            System.out.println("Didn't find word! !%s!".format(word))
            outs(attr).write(mfts(attr) + "\n")
          }
        }
        outs(attr).write("\n")
      }
    }

    for (attr <- attributes) {
      outs(attr).close()
    }
	}
}




/*
			for (token <- datum.forms.zip(datum.postags)) {
				val word = token._1.toLowerCase
				if (cpt.contains(word)) {
					println(cpt.mostProbable(word)._1)
				}
				else {
					println(mft)
				}
			}
			println()
		}


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