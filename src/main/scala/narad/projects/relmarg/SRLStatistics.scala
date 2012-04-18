package narad.projects.relmarg
import java.io._
import scala.collection.mutable.{HashMap, HashSet}
import narad.util.{ArgParser, ChunkReader}

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
		val dists = new Array[Int](1000)
		for (datum <- SRLReader.iterator(options)) {
			count += 1
			val slen = datum.size
			var noPred = true
			for (i <- 1 to slen if datum.hasPred(i)) {
					predCount += 1
					noPred = false
					for (j <- 1 to slen if datum.hasArg(i,j)) {
						argCount += 1
						dists(Math.abs(i-j)) += 1
					}
			}
			for (label <- datum.labelArray) {
				if (roles.contains(label)) {
					roles(label) += 1
				}
				else {
					roles(label) = 1
				}
			}
			if (noPred) noPredCount += 1
		}
		println("Statistics for %s:".format(filename))
		println("%d sentences, %d of them not having any predicates".format(count, noPredCount))
		println("%d args, an average of %f per predicate, with %d roles:\n  %s".format(argCount, argCount / predCount.toDouble, roles.keys.size, roles.mkString(", ")))
		println("Role statistics:")
		for (role <- roles.toArray.sortBy(_._2 * -1)) {
			println("  %s: %d".format(role._1, role._2))
		}
		var max = 0
		for (i <- 1 until dists.size) if (dists(i) > 0) max = i
		println("Arg distances:")
		for (i <- 1 until max) println("%d: %d".format(i, dists(i)))
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
