package narad.util

object TagalogFixer {
	
	def main(args: Array[String]) = {
		for (line <- io.Source.fromFile(args(0)).getLines()) {
			if (!line.isEmpty) {
				val cols = line.split("\t")
				cols(4) = cols(3) + "-" + cols(4)
				println(cols.mkString("\t"))
			}
			else {
				println()
			}
		}
	}
}