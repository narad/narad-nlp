package narad.io.util
import narad.util.ArgParser
import scala.collection.mutable.ArrayBuffer

class ChunkReader(filename: String, iencoding: String="UTF-8") extends Iterable[String] {
	private var processed = 0
	private val delim = "^[ \t]*$"

	/** An iterator for reading blank-line delimited files.
	* 
	* Can be used with most treebanks, CoNLL-formatted files to
	* process each chunk in a memory-efficient manner.  Though 
	* not totally efficient at the moment...*/
  def iterator: Iterator[String] = {
    var lines = Array[String]()
    try {
      val src = scala.io.Source.fromFile(filename, iencoding)
      lines = src.getLines().toArray
      src.close()
    }
    catch {
      case e: Exception => System.err.println("Error reading file <%s> in ChunkReader.read (encoding:%s)".format(filename, iencoding))
    }
    Iterator.continually(readNext(lines)).takeWhile(_ != null)
  }

  /*
  def iterator: Iterator[String] = {
		var lines = Array[String]()
		try {
			lines = scala.io.Source.fromFile(filename, iencoding).getLines().toArray
		}
		catch {
			case e: Exception => System.err.println("Error reading file <%s> in ChunkReader.read (encoding:%s)".format(filename, iencoding))
		}
		Iterator.continually(readNext(lines)).takeWhile(_ != null)
	}
	*/

  /*
	def read[T](filename: String, parseString: String => T): Iterator[T] = {
		val lines = scala.io.Source.fromFile(filename).getLines.toArray
		Iterator.continually(parseString(readNext(lines))).takeWhile(_ != null)
	}
  */

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

  /*
	def slurp(filename: String): Array[String] = {
		val buffer = new ArrayBuffer[String]
		for (str <- read(filename)) { buffer += str }
		buffer.toArray
	}
  */

	def reset = processed = 0
}

object ChunkReader {

  def read(filename: String): Iterator[String] = {
    val reader = new ChunkReader(filename)
    reader.iterator
  }
}


/*
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
    */
/*
object ChunkReaderTester {
	def main(args: Array[String]) {
		var i = 1
		for (chunk <- ChunkReader.read(args(0))) {
//			println("CHUNK #%d:\n%s\n\n".format(i, chunk))
			i += 1
		}
	}
}
*/