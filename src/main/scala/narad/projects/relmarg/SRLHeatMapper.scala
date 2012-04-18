package narad.projects.relmarg
import java.io.File
import narad.util.{ArgParser, ChunkReader}

object SRLHeatMapper {
	val predPattern  = """pred\(([0-9]+)\)(.+)""".r
	val sensePattern = """sense\(([0-9]+),(.+)\)(.+)""".r
	val argPattern   = """hasArg\(([0-9]+),([0-9]+)\)(.+)""".r
	val labelPattern = """hasLabel\(([0-9]+),([0-9]+),(.+)\)(.*)""".r

	val linkPattern  = """un\(([0-9]+),([0-9]+)\)(.+)""".r

	def main(args: Array[String]) = {
		val options = new ArgParser(args)
		val heatmap  = options.getString("--heatmap", "syntax")
		val chunks = ChunkReader.read(options.getString("--bpdp.file")).toArray
		val outdir = options.getString("--output", "heatmap")
		val datums = SRLReader.read(options).toArray
		assert(chunks.size == datums.size, "Files not identically sized.")
		for (i <- 0 until chunks.size) {
			val chunk = chunks(i)
			val lines = chunk.split("\n")
			val slen = findLength(lines)
			val grid = Array.ofDim[Double](slen+1, slen+1)
			val tgrid = Array.ofDim[Double](slen+1, slen+1)
			for (line <- lines) {
				line match {
					case linkPattern(s, e, w) => {
						val start  = s.toInt
						val end    = e.toInt
						val weight = w.trim.toDouble
						if (start > 0 && end > 0) {
							grid(start)(end) = weight							
						}
					}					
					case _ => 
				}
			}			
			var out = new java.io.PrintWriter(new File("heatmap/%d.hm".format(i)))
			out.println("HEAD," + (0 to slen).toArray.mkString(","))
			for (i <- 0 to slen) {
				out.println(i + "," + grid(i).mkString(","))
			}
			out.close
			
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