/*
package narad.nlp.srl
import narad.util.ArgParser
import narad.io.reader.SRLReader

object SRLErrorAnalysis {
	
	def main(args: Array[String]) = {
		val options = new ArgParser(args)
		val goldFile = options.getString("--gold.file")
		val hiddenFile = options.getString("--hidden.file")
		val jointFile  = options.getString("--joint.file")
		val min = options.getInt("--min", 0)
		val max = options.getInt("--max", 100)
		val gdatums = SRLReader.iterator(goldFile).toArray
		val hdatums = SRLReader.iterator(hiddenFile).toArray
		val jdatums = SRLReader.iterator(jointFile).toArray
		for (i <- 0 until gdatums.size) {
			val slen = gdatums(i).slen
			if (slen > min && slen < max) {
				val hf1 = F1(gdatums(i), hdatums(i))
				val jf1 = F1(gdatums(i), jdatums(i))
				if (hf1 > jf1) {
					println("Ex: " + i + "   Gain: " + (hf1 - jf1))
					println(gdatums(i))
					println
				}				
			}
		}
	}
	
	def F1(gold: SRLDatum, test: SRLDatum): Double = {
		var gargs = 0.0
		var targs = 0.0
		var correct = 0.0
		for (i <- 1 to gold.slen) {
			if (gold.hasPredicate(i)) {
				for (j <- 1 to gold.slen) {
					if (gold.hasArg(i,j) && test.hasArg(i,j)) {
						correct += 1
						gargs += 1
						targs += 1
					}
					else if (gold.hasArg(i,j)) {
						gargs += 1
					}
					else if (test.hasArg(i,j)) {
						targs += 1
					}
				}
			}
		}
		val prec = if (correct == 0 || targs == 0) 0 else correct / targs
		val rec  = if (correct == 0 || gargs == 0) 0 else correct / gargs
		val f1 = if (prec == 0 || rec == 0) 0 else (2 * (prec * rec ) / (prec + rec))
		return f1
	}
}
*/