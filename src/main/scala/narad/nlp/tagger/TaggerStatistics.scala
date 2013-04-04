package narad.nlp.tagger
import narad.io.conll.{CoNLLDatum, CoNLLReader}
import narad.util.{ArgParser, HashCounter}
import narad.util.visualize.HeatMap
import narad.stats.ConditionalProbabilityTable
import collection.mutable.HashSet
import java.io.File

object TaggerStatistics {
	
	def main(args: Array[String]) = {
		val options = new ArgParser(args)
		val conllFile = options.getString("--input.file")
		val rFile = options.getString("--output.file")
    val attribute = options.getString("--attribute", "FINE")
    heatmaps(conllFile, attribute)
	}

  def heatmaps(filename: String, attribute: String) {
    val multidict = MultiTagDictionary.construct(new CoNLLReader(filename), Array(attribute))
    val adjCPT = new ConditionalProbabilityTable[String]
    val syntaxCPT = new ConditionalProbabilityTable[String]
    var dtcount = 0
    var tocount = 0
    var mdcount = 0

    for (datum <- new CoNLLReader(filename)) {
      datum.cpostags.foreach{t =>
        if (t == "DT") dtcount += 1
        if (t == "TO") tocount += 1
        if (t == "MD") mdcount += 1
      }
      for (i <- 2 to datum.slen) {
        adjCPT.increment(tag(datum, i, attribute), tag(datum, i-1, attribute))
      }
      for (i <- 1 to datum.slen; j <- 1 to datum.slen if i != j) {
        if (datum.head(j) == i) {
          syntaxCPT.increment(tag(datum, j, attribute), tag(datum, i, attribute))
        }
      }
    }
    println("DT = " + dtcount)
    println("TO = " + tocount)
    println("MD = " + mdcount)

    printCPT(adjCPT, multidict, attribute, "BIGRAM")
    printCPT(syntaxCPT, multidict, attribute, "SYNTAX")
  }

  def printCPT(cpt: ConditionalProbabilityTable[String], multidict: MultiTagDictionary, attribute: String, label: String) {
    println("%s PROBABILITIES".format(label))
    val tags = multidict.tagsOfAttribute(attribute).map(t => (t, cpt.probability(t))).sortBy(_._2 * -1).toArray
    tags.foreach{case(t,p) => println(t + " " + p)}
    println("%s CONDITIONAL PROBABILITIES".format(label))
    for (tag1 <- multidict.tagsOfAttribute(attribute).sortBy(_.toString); tag2 <- multidict.tagsOfAttribute(attribute).sortBy(_.toString)) {
      println("P(%s|%s) = %f".format(tag2, tag1, cpt.conditionalProbability(tag2, tag1)))
    }
    println
    val hm1 = HeatMap.constructFromCPT(cpt, tags.map(_._1).toArray, tags.map(_._1).toArray)

  //  val etags = Array("NN", "VB", "IN", "DT", "JJ", "CC", "TO", "MD") //cpt.contexts
  //  val hm1 = HeatMap.constructFromCPT(cpt, etags, etags)
    hm1.writeToFile(new File("%s.%s.R".format(label, attribute)), "%s.%s.heatmap".format(label, attribute))
  }

  def tag(datum: CoNLLDatum, i: Int, attribute: String): String = {
    attribute match {
      case "COARSE" => datum.cpostag(i)
      case "FINE"   => datum.postag(i)
      case "CASE"   => datum.mcase(i)
      case "PERSON" => datum.mperson(i)
      case "GENDER" => datum.mgender(i)
      case "NUMBER" => datum.mnumber(i)
    }
  }
}







/*


    println("SYNTAX CONDITIONAL PROBABILITIES")
    for (tag1 <- multidict.tagsOfAttribute(attribute); tag2 <- multidict.tagsOfAttribute(attribute)) {
      println("P(%s|%s) = %f".format(tag2, tag1, syntaxCPT.conditionalProbability(tag2, tag1)))
    }


    val hm2 = HeatMap.constructFromCPT(syntaxCPT, tags.toArray, tags.toArray)
    hm2.writeToFile(new File("syntaxCPT.R"), "syntax.heatmap")
  }
 */
















/*
package narad.nlp.tagger
import java.io.File
import narad.io.conll._
import narad.stats.ConditionalProbabilityTable
import narad.util.ArgParser
import narad.util.visualize.HeatMap

object TaggerVisualization {

	def main(args: Array[String]) = {
		val options = new ArgParser(args)
		val cfile = options.getString("--conll.file")
		val mode  = options.getString("--mode", "BIGRAM")
		val thresh = options.getInt("--threshold", 0)
		val verbose = options.getBoolean("--verbose", false)
		val data = new CoNLLReader(cfile).iterator.toArray
		val hm = if (mode == "SYNTAX") {
			constructHead(data, thresh, verbose)
		}
		else {
			constructMarkov(data, thresh, verbose)
		}
		hm.writeToFile(new File("hrm.r"))


	}

	def constructMarkov(data: Array[CoNLLDatum], threshold: Int, verbose: Boolean): HeatMap = {
		val cpt = new ConditionalProbabilityTable[String]
		val utags = data.map(_.cpostags).flatten.groupBy(_.toString)
		val tags = utags.filter(_._2.size >= threshold).map(_._1).toArray.sortBy(_.toString).filter(_.size > 1)
		println(tags.zipWithIndex.mkString("\n"))

		for (datum <- data) {
			for (i <- 2 to datum.slen) {
				if (tags.contains(datum.cpostag(i)) && tags.contains(datum.cpostag(i-1))) {
					cpt.increment(datum.cpostag(i-1), datum.cpostag(i))
				}
			}
		}
		val size = tags.size
		val grid = Array.ofDim[Double](size, size)
		for (t1 <- 0 until size; t2 <- 0 until size) {
			val p = cpt.conditionalProbability(tags(t2), tags(t1))
			grid(t2)(t1) = truncate(p)
		}
		if (verbose) {
			val tops = cpt.probs.sortBy(_._2 * -1.0).slice(0,20)
			for (t <- tops) {
				println("P(i=%s|i-1=%s) = %f".format(t._1._2, t._1._1, t._2))
			}
		}
		val inted = tags.zipWithIndex.map(_._2.toString)
		new HeatMap(grid, tags, tags)
	}

	def constructHead(data: Array[CoNLLDatum], threshold: Int, verbose: Boolean): HeatMap = {
		val cpt = new ConditionalProbabilityTable[String]
		val utags = data.map(_.cpostags).flatten.groupBy(_.toString)
		val tags = utags.filter(_._2.size >= threshold).map(_._1).toArray.sortBy(_.toString).filter(_.size > 1)
		println(tags.zipWithIndex.mkString("\n"))

		for (datum <- data) {
			for (i <- 2 to datum.slen) {
				val head = datum.head(i)
				if (head > 0 && tags.contains(datum.cpostag(i)) && tags.contains(datum.cpostag(head))) {
					cpt.increment(datum.cpostag(head), datum.cpostag(i))
				}
			}
		}
		val size = tags.size
		val grid = Array.ofDim[Double](size, size)
		for (t1 <- 0 until size; t2 <- 0 until size) {
			val p = cpt.conditionalProbability(tags(t2), tags(t1))
			grid(t2)(t1) = truncate(p)
		}
		if (verbose) {
			val tops = cpt.probs.sortBy(_._2 * -1.0).slice(0,20)
			for (t <- tops) {
				println("P(i=%s|head(i)=%s) = %f".format(t._1._2, t._1._1, t._2))
			}
		}
		val inted = tags.zipWithIndex.map(_._2.toString)
		new HeatMap(grid, tags, tags)
	}

	def truncate(x: Double) = math.round(x*1000)*0.001


}

*/