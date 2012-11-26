/*
package narad.nlp.srl
import java.io.File
import narad.util.ArgParser
import narad.util.visualize.HeatMap
import narad.io.reader.{ChunkReader, SRLReader}
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

object SRLHeatMapper {
	val linkPattern  = """un\(([0-9]+),([0-9]+)\)(.+)""".r
	val argPattern   = """hasArg\(([0-9]+),([0-9]+)\)(.+)""".r

	def main(args: Array[String]) = {
		val options = new ArgParser(args)
		val outdir = options.getString("--output.dir", "heatmap")
		val label  = options.getBoolean("--label", false)
		var i = 0
		if (options.contains("--bpdp.file")) {
			for (chunk <- ChunkReader.read(options.getString("--bpdp.file"))) {
				val xlabels = if (label) getLabels(chunk) else Array[String]()
				val ylabels = xlabels

				val grid = heatgridFromPotentials(chunk, linkPattern)
				val hm = new HeatMap(grid, xlabels, ylabels)
				hm.writeToFile(new File("%s/%d_test_heatmap.R".format(outdir, i+1)), "%d_test_heatmap".format(i+1))
//				val code = constructCode(grid, labels, "%d_test_heatmap".format(i+1))
//				var out = new java.io.PrintWriter(new File("%s/%d_test_heatmap.R".format(outdir, i+1)))
//				out.write(code)
//				out.close			

				val sgrid = heatgridFromPotentials(chunk, argPattern)
				val shm = new HeatMap(sgrid, xlabels, ylabels)
				shm.writeToFile(new File("%s/%d_srltest_heatmap.R".format(outdir, i+1)), "%d_srltest_heatmap".format(i+1))

//				val scode = constructCode(sgrid, labels, "%d_srltest_heatmap".format(i+1))
//				var sout = new java.io.PrintWriter(new File("%s/%d_srltest_heatmap.R".format(outdir, i+1)))
//				sout.write(scode)
//				sout.close			


				i += 1
			}			
		}
		
		i = 0
		if (options.contains("--srl.file")) {
			for (datum <- SRLReader.read(options)) {
				val grid = heatgridFromDatum(datum)
				val xlabels = if (label) Array("<root>") ++ datum.tokens.map(_.word) else Array[String]()
				val ylabels = xlabels
				val hm = new HeatMap(grid, xlabels, ylabels)
				hm.writeToFile(new File("%s/%d_gold_heatmap.R".format(outdir, i+1)), "%d_gold_heatmap".format(i+1))

//				val code = constructCode(grid, labels, "%d_gold_heatmap".format(i+1))
//				var out = new java.io.PrintWriter(new File("%s/%d_gold_heatmap.R".format(outdir, i+1)))
//				out.write(code)
//				out.close	
				
				val sgrid = heatgridFromDatum(datum, syntax=false, srl=true)
				val shm = new HeatMap(sgrid, xlabels, ylabels)
				shm.writeToFile(new File("%s/%d_srlgold_heatmap.R".format(outdir, i+1)), "%d_srlgold_heatmap".format(i+1))
				
//				val scode = constructCode(sgrid, labels, "%d_srlgold_heatmap".format(i+1))
//				var sout = new java.io.PrintWriter(new File("%s/%d_srlgold_heatmap.R".format(outdir, i+1)))
//				sout.write(scode)
//				sout.close
				i += 1		
			}			
		}
	}
	
	def getLabels(chunk: String): Array[String] = {
		val lines = chunk.split("\n")
		for (line <- lines) {
			if (line.startsWith("@words")) {
				return Array("<root>") ++ line.split("\t")(1).split(" ")
			}
		}
		return Array()
	}

	def heatgridFromPotentials(chunk: String, pattern: Regex): Array[Array[Double]] = {
		val lines = chunk.split("\n")
		val slen = findLength(lines)
		val grid = Array.ofDim[Double](slen+1, slen+1)
		for (i <- 0 until grid.size; j <- 0 until grid(i).size) grid(i)(j) = 0.0
		for (line <- lines) {
			line match {
				case pattern(s, e, w) => {
					val start  = s.toInt
					val end    = e.toInt
					val weight = w.trim.toDouble
						grid(start)(end) = weight							
				}					
				case _ => 
			}
		}
		grid
	}
	
	def heatgridFromDatum(datum: SRLDatum, syntax: Boolean = true, srl: Boolean = false): Array[Array[Double]] = {
		println(datum)
		val slen = datum.slen
		val grid = Array.ofDim[Double](slen+1, slen+1)
		for (i <- 0 to slen; j <- 1 to slen) {
			if (syntax) {
				grid(i)(j) = if (datum.head(j) == i) 1.0 else 0.0				
			}
			else if (srl) {
				grid(i)(j) = if (datum.hasArg(i,j)) 1.0 else 0.0				
			}
		}
		return grid
	}

//
//> A = matrix( 
//+   c(2, 4, 3, 1, 5, 7), # the data elements 
//+   nrow=2,              # number of rows 
//+   ncol=3,              # number of columns 
//+   byrow = TRUE)        # fill ma


	def findLength(lines: Array[String]): Int = {
		val slenPattern = """@slen\t *(.+)""".r
		for (line <- lines) {
			line match {
				case slenPattern(l) => return l.trim.toInt
				case _ =>
			}
		}
		return 0
	}
}


/*
var out = new java.io.PrintWriter(new File("heatmap/%d.hm".format(i)))
out.println("HEAD," + (0 to slen).toArray.mkString(","))
for (i <- 0 to slen) {
out.println(i + "," + grid(i).mkString(","))
}
out.close

/*
f <- "heatmap/2.gold"
pdf(sprintf("%s.%s", f, "pdf"))
hm <- read.csv(f, sep=",")
row.names(hm) <- hm$HEAD
hm <- hm[,2:17]
hm_matrix <- data.matrix(hm)
hm_hm <- heatmap(hm_matrix, Rowv=NA, Colv=NA, col = cm.colors(256), scale="column", margins=c(5,10))
dev.off()
*/

val heads = Array(-1) ++ datums(i).heads
for (i <- 1 to slen) {
val h = heads(i)
tgrid(h)(i) = 1.0
}
var tout = new java.io.PrintWriter(new File("heatmap/%d.gold".format(i)))
tout.println("HEAD," + (0 to slen).toArray.mkString(","))
for (i <- 0 to slen) {
tout.println(i + "," + tgrid(i).mkString(","))
}
tout.close

}
}
*/



*/

