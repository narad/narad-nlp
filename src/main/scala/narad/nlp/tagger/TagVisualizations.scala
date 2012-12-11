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