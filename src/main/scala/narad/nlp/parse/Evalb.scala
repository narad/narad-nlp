package narad.nlp.parse
import narad.util.{ArgParser, ZippedReader}

object Evalb {
	
	def main(args: Array[String]) = {
		val options = new ArgParser(args)
		val goldFile = options.getString("--gold.file")
		val testFile = options.getString("--test.file")
		val nps = Array("NP", "NNP")
		var ccount = 0.0
		var rcount = 0.0
		var pcount = 0.0
		val gparses = io.Source.fromFile(goldFile).getLines.toArray
		val tparses = io.Source.fromFile(testFile).getLines.toArray
		assert(gparses.size == tparses.size, "Files not equal in length.")
//		for (t <- ZippedReader.read(goldFile, testFile)) {
		for (i <- 0 until gparses.size) {
			try {
				val goldTree = TreebankReader.parseExpression(gparses(i), options)
				val testTree = TreebankReader.parseExpression(tparses(i), options)
				goldTree.annotateWithIndices(0)
				testTree.annotateWithIndices(0)
				val gspans = goldTree.getSpans.filter{s => nps.contains(s.label)}
				val tspans = testTree.getSpans.filter{s => nps.contains(s.label)}
				for (gspan <- gspans) {
					var found = false
					for (tspan <- tspans) {
						if (gspan.start == tspan.start && gspan.end == tspan.end && gspan.label == tspan.label) {
							ccount += 1
						}					
					}
				}
				rcount += gspans.size
				pcount += tspans.size				
			}
		}
		val p  = if (pcount == 0) 0 else ccount / pcount
		val r  = if (rcount == 0) 0 else ccount / rcount
		val f1 = if ((p + r) == 0) 0 else 2 * ((p * r) / (p + r))
		println("NP Precision: (%f / %f) = %f".format(ccount, pcount, p))
		println("NP Recall: (%f / %f) = %f".format(ccount, rcount, r))
		println("NP F1: %f".format(f1))
	}
	
}