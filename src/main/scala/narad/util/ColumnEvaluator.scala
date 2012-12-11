package narad.util

object ColumnEvaluator {
	
	def main(args: Array[String]) {
		val options = new ArgParser(args)
		val goldFile = options.getString("--gold.file")
		val testFile = options.getString("--test.file")
		val offset   = options.getInt("--offset")
		val delim    = options.getString("--delim", "\t")
		val verbose  = options.getBoolean("--verbose", false)
		val glines = io.Source.fromFile(goldFile).getLines.toArray
		val tlines = io.Source.fromFile(testFile).getLines.toArray
		assert(glines.size == tlines.size, "Number of lines do not match: %d (gold) vs %d (test)".format(glines.size, tlines.size))

		val gden = glines.map(_.trim).zipWithIndex.filter(_._1.isEmpty)
		val tden = tlines.map(_.trim).zipWithIndex.filter(_._1.isEmpty)
		assert(gden.mkString == tden.mkString, "Incorrect empty line offsets between gold (%d) and test file (%d).".format(gden.size, tden.size))
		
		var correct = 0
		for (i <- 0 until glines.size if !glines(i).isEmpty && !tlines(i).isEmpty) {
			val gcols = glines(i).split("\t")
			val tcols = tlines(i).split("\t")
			if (tcols(0).trim == gcols(offset).trim) {
				correct += 1 
			}
			else if (verbose) {
				System.err.println("ERROR line %d: GOLD[%s] vs TEST[%s]".format(i, gcols(offset), tcols(0)))
			}	
		}
		
		val denom = glines.size - gden.size
		System.err.println("Tagging Accuracy: [%d / %d] = %f".format(correct, denom, correct / denom.toDouble))			
	}
}
		
		


/*
object ColumnEvaluator {
	
	def main(args: Array[String]) {
		val options = new ArgParser(args)
		val goldFile = options.getString("--gold.file")
		val testFile = options.getString("--test.file")
		val gcols    = options.getString("--gcols")
		val tcols    = options.getString("--tcols")
		val delim    = options.getString("--delim", "\t")
		val numcols  = options.getInt("--num.cols", 1)
		val offset   = options.getInt("--offset")
		val simple = options.getBoolean("--simple", true)
//		val osign = offsetstr.substring(0,1)
//		val ovalue = offsetstr.substring(1).toInt
//		assert(osign == "+" || osign == "-")
		val reader = LineReader.read(goldFile, options).zip(LineReader.read(testFile, options))
		val corrects = new Array[Int](numcols)
		val comps = new Array[Int](numcols)
	
		for (pair <- reader if (!pair._1.isEmpty && !pair._2.isEmpty)) {
			val goldCols = pair._1.split(delim)
			val testCols = pair._2.split(delim)
//			println(testCols())
			for (i <- 0 until testCols.size) {
				if (testCols(i) == goldCols(i+offset)) corrects(i) += 1 
				comps(i) += 1
			}
		}
		for (i <- 0 until numcols) {
			System.err.println("Tagging Accuracy Column %d: %f".format(i, corrects(i).toDouble / comps(i).toDouble))			
		}
		System.err.println("---------------------------------")
//		System.err.println("Tagging Accuracy All Columns: %f".format(correct / comps(0)))			
	}
}
*/


/*
if (goldCols.size > 4) {
	val gp = goldCols(3) + "^" + goldCols(4)
	val tp = testCols(0)
	if (gp == tp) correct += 1
	comps += 1
}
*/