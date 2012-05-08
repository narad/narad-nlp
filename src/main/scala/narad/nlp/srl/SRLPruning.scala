package narad.nlp.srl
import java.io._
import scala.collection.mutable.{HashMap, HashSet}
import narad.util.ChunkReader

object SRLPruning {

	def main(cargs: Array[String]) = {
		val options = new narad.util.ArgParser(cargs)
		val argFile   = options.getString("--arg.file", "srl.args")
		val predFile  = options.getString("--pred.file", "srl.preds")
		val senseFile = options.getString("--sense.file", "srl.senses")
		val suffixFile = options.getString("--suffix.file", "srl.suffixes")
		val distFile  = options.getString("--dist.file", "srl.dist")
		val lthreshold = options.getInt("--label.threshold", 300)
		val dthreshold = options.getInt("--dist.threshold", 100)
		val dpercent   = options.getDouble("--dist.percent", 98)
		val iformat   = options.getString("--input.format", "UTF-8")
		val oformat   = options.getString("--input.format", "UTF-8")
		
		
		val preds  = new HashSet[String]
		val args   = new HashMap[String, Int]
		val suffixes = new HashMap[String, Int]
		val senses = new HashMap[String, HashSet[String]]
		val dists  = new Array[Int](1000)
		System.out.println("about to read")
		for (datum <- SRLReader.read(options)) {
//			args ++= datum.labels
		println("datum found")
			println(datum)
			for (label <- datum.labels) {
				if (args.contains(label)) args(label) += 1 else args(label) = 1
			}
			for (i <- 1 to datum.slen if datum.hasPred(i)) {
				println(i)
				println(i + ":" + datum.postags(i))
				preds += datum.postags(i)
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
					dists(Math.abs(i-j)) += 1
				}
			}
		}
		if (argFile != null) {
			var out = new PrintWriter(new File(argFile), oformat);
			val labs = args.toArray.sortBy(_._2 * -1).filter(_._2 >= lthreshold)
			out.write(labs.map(_._1).mkString("\n"))
			out.close
/*
			var pruned = false
			for (l <- args) {
				if (l._2 >= lthreshold) {
					out.write(l._1 + "\n")
				}
				else {
					pruned = true
				}
			}	
*/ //			if (pruned) out.write("A-DUMMY\n")
		}
		if (predFile != null) {
			var out = new PrintWriter(new File(predFile), oformat);
			for (l <- preds) out.write(l + "\n")
			out.close
		}
		if (senseFile != null) {
			var out = new PrintWriter(new File(senseFile), oformat);
			for (pred <- senses.keys) {
				out.write("%s\t%s\n".format(pred, senses(pred).mkString(" ")))
			}
			out.close
		}
		if (suffixFile != null) {
			var out = new PrintWriter(new File(suffixFile), oformat);
			val max = suffixes.toArray.sortBy(_._2 * -1).first._1
			out.write(max + "\n")
			out.close
		}
		if (distFile != null) {
			val out = new FileWriter(distFile)
			println(dists.mkString("\n"))
			val sum = dists.foldLeft(0.0)(_+_)
			val tally = sum / 100 * dpercent
			var set = false
			var lump = 0.0
			var dmax = 1000
			for (i <- 1 until dists.size) {
				lump += dists(i)
				if (!set && lump > tally) {
					dmax = i
					set = true
				}
			}
			
//			val dmax = dists.zipWithIndex.filter(_._1 > 0).map(_._2).max

//			var dmax = 0
//			for (i <- 1 until dists.size) {
//				if (dists(i) < dthreshold && dmax == 0) dmax = i
//			}
			out.write("%d\n".format(dmax))
			out.close
		}
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