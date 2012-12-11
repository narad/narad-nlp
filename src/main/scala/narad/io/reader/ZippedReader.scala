package narad.io.reader

object ZippedReader {
	private var processed1 = 0
	private var processed2 = 0

	def read(filename1: String, filename2: String): Iterator[(String, String)] = {
		val lines1 = scala.io.Source.fromFile(filename1).getLines.toArray
		val lines2 = scala.io.Source.fromFile(filename2).getLines.toArray
		Iterator.continually(Tuple(readNext1(lines1), readNext2(lines2))).takeWhile(t => t._1 != null && t._2 != null)
	}

/*
	def read[T1,T2](reader1: Reader, reader2: Reader): Iterator[(T1, T2)] = {
		while (reader1.hasNext && reader2.hasNext) {
			yield Tuple(reader1.next, reader2.next)
		}
//		Iterator.continually(Tuple(readNext1(lines1), readNext2(lines2))).takeWhile(t => t._1 != null && t._2 != null)
	}
*/

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