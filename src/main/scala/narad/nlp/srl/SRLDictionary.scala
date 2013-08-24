package narad.nlp.srl
import narad.io.srl.SRLReader
import collection.mutable.{HashMap, HashSet}
import java.io.{File, FileWriter, PrintWriter}
import math._

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 1/4/13
 * Time: 7:36 PM
 * To change this template use File | Settings | File Templates.
 */
class SRLDictionary(argDict: HashMap[String, Int], suffixes: HashMap[String, Int],
                    senseDict: HashMap[String, HashSet[String]], dists: Array[Int]) {
/*
  private val preds  = new HashSet[String]
  private val args   = new HashMap[String, Int]
  private val suffixes = new HashMap[String, Int]
  private val senses = new HashMap[String, HashSet[String]]
*/

  def containsSense(lemma: String) = senseDict.contains(lemma)

  def senses(lemma: String) = {
    if (senseDict.contains(lemma)) {
      senseDict(lemma).toArray
    }
    else {
      Array[String]()
    }
  }

  lazy val roles = argDict.toArray.sortBy(_._2 * -1).map(_._1)

}

object SRLDictionary {

  def construct(filename: String): SRLDictionary = {
    val reader = new SRLReader(filename)
    val args   = new HashMap[String, Int]
    val suffixes = new HashMap[String, Int]
    val senses = new HashMap[String, HashSet[String]]
    val dists  = new Array[Int](1000)
    for (datum <- reader) {
      for (label <- datum.labels) {
        if (args.contains(label)) args(label) += 1 else args(label) = 1
      }
      for (i <- 1 to datum.slen if datum.hasPred(i)) {
        val pred = datum.lemma(i)
        val sense = datum.sense(i)
        val suffix = sense.substring(sense.lastIndexOf(".")+1)
        if (suffixes.contains(suffix)) {
          suffixes(suffix) += 1
        }
        else {
          suffixes(suffix) = 1
        }

        if (senses.contains(pred)) {
          senses(pred) += sense
        }
        else {
          senses(pred) = new HashSet[String]
          senses(pred) += sense
        }
        for (j <- 1 to datum.slen if datum.hasArg(i,j)) {
          dists(abs(i-j)) += 1
        }
      }
    }
    new SRLDictionary(args, suffixes, senses, dists)
  }
}

























/*
def findLabels(filename: String, format: String, threshold: Int = 0): Array[String] = {
val labels = new HashSet[String]
for (chunk <- ChunkReader.read(filename)) {
val datum = SRLDatum.constructFromCoNLL(chunk.split("\n"), format=format)
labels ++= datum.labels
}
System.err.println("Arg labels (%d): %s".format(labels.size, labels.mkString(", ")))
return labels.toArray
}


def findPredTags(filename: String, poffset: Int = 13, threshold: Int = 0): Array[String] = {
val labels = new ArrayBuffer[String]
for (line <- io.Source.fromFile(filename).getLines() if line.contains("\t")) {
val cells = line.split("\t")
if (cells(poffset) != "_") labels += cells(4)
}
val ltokens = labels.toArray
val ltypes  = ltokens.distinct
if (threshold == 0) {
return ltypes
}
else {
return ltypes.filter(l => ltokens.filter(_ == l).size > threshold)
}
}
*/




/*
package narad.nlp.srl
import java.io._
import scala.collection.mutable.{HashMap, HashSet}
import narad.io.util.{ChunkReader, SRLReader}
import narad.util.ArgParser

object SRLStatistics {

	def main(args: Array[String]) = {
		val options = new ArgParser(args)
		val filename  = options.getString("--srl.file")
		val format    = options.getString("--format", "CoNLL09")
		assert(format == "CoNLL09" || format == "CoNLL08", "Invalid SRL format: " + format)
		var argCount = 0
		var count = 0
		var predCount = 0
		var noPredCount = 0

		val tags  = new HashSet[String]
		val preds = new HashSet[String]
		val sargs = new HashSet[String]
		val rargs  = new HashSet[Tuple2[String, String]]
		val roles = new HashMap[String, Int]
		val dups = new HashMap[String, Int]
		val dists = new Array[Int](1000)

		var scount = 0
		var dscount = 0
		var tscount = 0

		for (datum <- SRLReader.iterator(options)) {
			count += 1
			val slen = datum.slen
			var noPred = true
			for (i <- 1 to slen if datum.hasPred(i)) {
					predCount += 1
					noPred = false
					for (j <- 1 to slen if datum.hasArg(i,j)) {
						argCount += 1
						dists(Math.abs(i-j)) += 1
						if (datum.head(j) == i) scount += 1
						var ffound = false
						for (k <- 1 to slen) {
							if (datum.head(j) == k && datum.head(k) == i) ffound = true
						}
						var tfound = false
						for (k <- 1 to slen; l <- 1 to slen) {
							if (datum.head(j) == k && datum.head(k) == l && datum.head(l) == i) tfound = true
						}
						if (ffound) dscount += 1
						if (tfound) tscount += 1
					}
			}


			for (label <- datum.labels) {
				if (roles.contains(label)) {
					roles(label) += 1
				}
				else {
					roles(label) = 1
				}
				/*
				val containsDuplicate = datum.frames.exists{f => f.args.size > f.args.map(_.label).distinct.size}
				if (datum.containsDuplicate(label)) {
					if (dups.contains(label)) {
						dups(label) += 1
					}
					else {
						dups(label) = 1
					}
				}
				*/
			}



			if (noPred) noPredCount += 1
		}
		println("Statistics for %s:".format(filename))
		println("%d sentences, %d of them not having any predicates".format(count, noPredCount))
		println("%d args, an average of %f per predicate, with %d roles:\n  %s".format(argCount, argCount / predCount.toDouble, roles.keys.size, roles.mkString(", ")))
		println("Role statistics:")
		for (role <- roles.toArray.sortBy(_._2 * -1)) {
			if (dups.contains(role._1)) {
				println("  %s: %d (duplicated in %d frames)".format(role._1, role._2, dups.getOrElse(role._1, 0)))
			}
			else {
				println("  %s: %d".format(role._1, role._2))
			}
		}
		var max = 0
		for (i <- 1 until dists.size) if (dists(i) > 0) max = i
		println("Arg distances:")
		for (i <- 1 until max) println("%d: %d".format(i, dists(i)))

		println("% of 1st and 2nd order syntax correspondences with SRL arguments:")
		println("1st Order: (%d/%d) = %f".format(scount, argCount, scount.toDouble / argCount.toDouble))
		println("2nd Order: (%d/%d) = %f".format(dscount, argCount, dscount.toDouble / argCount.toDouble))
		println("3rd Order: (%d/%d) = %f".format(tscount, argCount, tscount.toDouble / argCount.toDouble))
	}
}






/*
for (j <- 1 to slen) {
	if (datum.hasArg(i,j)) {rargs += Tuple(tokens(i-1).pos, tokens(j-1).pos); sargs += tokens(j-1).pos }
}
*/
//		println("%d predicates with %d labels:\n  %s".format(predCount, preds.size, preds.mkString(", ")))
//		println("%d arg tags:\n %s".format(sargs.size, sargs.mkString(", ")))
//		println("%d arg tag pairs:\n %s".format(args.size, rargs.map(t => "%s-%s".format(t._1, t._2)).mkString(", ")))

*/