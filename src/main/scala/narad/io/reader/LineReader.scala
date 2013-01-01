package narad.io.reader
import narad.util.ArgParser

object LineReader {	

	def read(filename: String): Iterator[String] = {
			scala.io.Source.fromFile(filename).getLines()
	}
	
	def read(filename: String, options: ArgParser): Iterator[String] = {
		val encoding = options.getString("--input.encoding")
		if (encoding != null) {
			scala.io.Source.fromFile(filename, encoding).getLines()
		}
		else {
			scala.io.Source.fromFile(filename).getLines()
		}
	}
}



/*package narad.io.reader
import narad.util.ArgParser

object LineReader {	

	def read(filename: String): Iterator[String] = {
		val src = scala.io.Source.fromFile(filename) 
		try {
		  for (line <- src.getLines) yield line
		}
		finally src match { case b: scala.io.BufferedSource => b.close }
	}
	
	def read(filename: String, options: ArgParser): Iterator[String] = {
		val encoding = options.getString("--input.encoding")
		val src = if (encoding != null) {
			scala.io.Source.fromFile(filename, encoding) 			
		}
		else {
			scala.io.Source.fromFile(filename) 
		}
		try {
		  src.getLines
		}
		finally src match { case b: scala.io.BufferedSource => b.close }
	}
}
*/