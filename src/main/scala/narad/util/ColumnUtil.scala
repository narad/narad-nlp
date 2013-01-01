package narad.util

object ColumnUtil {
	
	def main(args: Array[String]) {
		for (line <- io.Source.stdin.getLines()) {
			val cols = line.split("\t")
			println(cols(0))
    }
	}
}