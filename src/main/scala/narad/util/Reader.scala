package narad.util
import scala.collection.mutable.ArrayBuffer

object ChunkReader {	
	private var processed = 0
	private val delim = "^[ \t]*$"

	/** An iterator for reading blank-line delimited files.
	* 
	* Can be used with most treebanks, CoNLL-formatted files to
	* process each chunk in a memory-efficient manner.  Though 
	* not totally efficient at the moment...*/
	def read(filename: String, iencoding: String="UTF-8"): Iterator[String] = {
		var lines = Array[String]()
		try {
			lines = scala.io.Source.fromFile(filename, iencoding).getLines.toArray			
		}
		catch {
			case e: Exception => System.err.println("Error reading file <%s> in ChunkReader.read (encoding:%s)".format(filename, iencoding))
		}
		Iterator.continually(readNext(lines)).takeWhile(_ != null)
	}

	def read[T](filename: String, parseString: String => T): Iterator[T] = {
		val lines = scala.io.Source.fromFile(filename).getLines.toArray
		Iterator.continually(parseString(readNext(lines))).takeWhile(_ != null)
	}

	def readNext(lines: Array[String]): String = {
		var start = processed
		while (processed < lines.size) {
			val line = lines(processed)
			processed += 1
			if (line.matches(delim)) {
				return lines.slice(start, processed-1).mkString("\n")
			}
		}
		if (start == processed) {
			reset
			return null
		}
		else {
			return lines.slice(start, processed).mkString("\n")
		}
	}

	def slurp(filename: String): Array[String] = {
		val buffer = new ArrayBuffer[String]
		for (str <- read(filename)) { buffer += str }
		buffer.toArray
	}

	def reset = processed = 0
	
	def main(args: Array[String]) {
		val options = new ArgParser(args)
		val input = options.getString("--input")
		val start = options.getInt("--start", 1)
		var end   = options.getInt("--end", 999999999)
		val grab  = options.getInt("--grab", -1)
		val scount = options.getBoolean("--count", false)
		val verbose = options.getBoolean("--verbose", true) 
		if (grab > 0) end = start+grab-1 
		var count = 1
		for (chunk <- read(input)) {
			if ((count >= start) && (count <= end) && verbose){
				println(chunk)
				println
			}
			count += 1
		}
		if (scount) println(count)
	}
}

object SexpReader {
	private var processed = 0
	val ldelim = "("
	val rdelim = ")"

	def read(filename: String): Iterator[String] = {
		val text = scala.io.Source.fromFile(filename).getLines.mkString(" ")
		Iterator.continually(readNext(text)).takeWhile(_ != None)
	}

	def read[T](filename: String, parseString: String => T): Iterator[T] = {
		val text = scala.io.Source.fromFile(filename).getLines.mkString(" ")
		Iterator.continually(parseString(readNext(text))).takeWhile(_ != null)
	}

	def readNext(text: String): String = {
		var count = 0;
		val start = processed
		if (start >= text.size)
		return null
		var letter = text.substring(processed, processed+1)
		while (processed < text.size) {
			processed += 1
			letter = text.substring(processed-1, processed)
			if (letter == ldelim) {
				count += 1
			}
			else if (letter == rdelim) {
				count -= 1
				if (count == 0) {
					return text.substring(start, processed).trim				
				}
			}
		}
		return null				
	}
}

object ZippedReader {
	private var processed1 = 0
	private var processed2 = 0

	def read(filename1: String, filename2: String): Iterator[(String, String)] = {
		val lines1 = scala.io.Source.fromFile(filename1).getLines.toArray
		val lines2 = scala.io.Source.fromFile(filename2).getLines.toArray
		Iterator.continually(Tuple(readNext1(lines1), readNext2(lines2))).takeWhile(t => t._1 != null && t._2 != null)
	}

	def readNext1(lines: Array[String]): String = {
		var start = processed1
		while (processed1 < lines.size) {
			val line = lines(processed1)
			processed1 += 1
			if (line == "") {
				return lines.slice(start, processed1-1).mkString("\n")
			}
		}
		if (start == processed1) {
			reset
			return null
		}
		else {
			return lines.slice(start, processed1).mkString("\n")
		}
	}

	def readNext2(lines: Array[String]): String = {
		var start = processed2
		while (processed2 < lines.size) {
			val line = lines(processed2)
			processed2 += 1
			if (line == "") {
				return lines.slice(start, processed2-1).mkString("\n")
			}
		}
		if (start == processed2) {
			reset
			return null
		}
		else {
			return lines.slice(start, processed2).mkString("\n")
		}
	}
	
	def reset = { processed1 = 0; processed2 = 0 }
}


object ChunkReaderTester {
	def main(args: Array[String]) {
		var i = 1
		for (chunk <- ChunkReader.read(args(0))) {
			println("CHUNK #%d:\n%s\n\n".format(i, chunk))
			i += 1
		}
	}
}
