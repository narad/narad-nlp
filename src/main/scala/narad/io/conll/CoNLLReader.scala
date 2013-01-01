package narad.io.conll

import narad.io.reader.ChunkReader
import narad.util.ArgParser
import scala.collection.mutable.ArrayBuffer

//abstract class CoNLLReader(filename: String) extends ChunkReader(filename) {
//}

class CoNLLReader(filename: String) extends Iterable[CoNLLDatum]{
	
	def iterator: Iterator[CoNLLDatum] = {
    val reader = new ChunkReader(filename)
    reader.iterator.map(s => CoNLLDatum.constructFromCoNLL(s.split("\n")))
  }
}

object CoNLLReader {

  def main(args: Array[String]) {
    val options = new ArgParser(args)
    val conllfile = options.getString("--conll.file")
    val mode = options.getString("--mode")
    System.err.println(conllfile)
    val reader = new CoNLLReader(conllfile)
    for (datum <- reader; i <- 1 to datum.slen) { System.out.println(extract(datum, i, mode)); if (i == datum.slen) println() }
  }

  def extract(datum: CoNLLDatum, i: Int, mode: String): String = {
    mode match {
      case "COARSE" => datum.cpostag(i)
      case "FINE"   => datum.postag(i)
      case "CASE"   => datum.mcase(i)
      case "GENDER" => datum.mgender(i)
      case "NUMBER" => datum.mnumber(i)
      case "CASE+GENDER+NUMBER" => datum.mcase(i) + "|" + datum.mgender(i) + "|" + datum.mnumber(i)
      case _ => ""
    }
  }
}













  /*
    Iterator.continually(readNext(lines)).takeWhile(_ != null)


    for (chunk <- reader) yield {
//			System.err.println("READER CHUNK:")
//			System.err.println(chunk)
			try {
		 		val datum = CoNLLDatum.constructFromCoNLL(chunk.split("\n"))
//				System.err.println("CONLL CHUNK:")
//				System.err.println(datum)
				datum
			}
			catch {
				case e: Exception => {
					System.err.println("Error trying to create datum from string:\n%s\n".format(chunk))
					System.err.println("\n" + e.getStackTrace + "\n")
					System.exit(1)
			 		null.asInstanceOf[CoNLLDatum]
				}
			}
		}
	}
}
               */
  /*
	def read(options: ArgParser): Array[CoNLLDatum] = {
		val filename  = options.getString("--input.file")
		val iformat   = options.getString("--input.format", "UTF-8")
		val oformat   = options.getString("--input.format", "UTF-8")
		val filter    = options.getBoolean("--filter.examples", false)
		val print 		= options.getBoolean("--print", false)
		
		var out = new java.io.PrintStream(System.out, true, oformat)
		val data = new ArrayBuffer[CoNLLDatum]
		for (chunk <- ChunkReader.read(filename, iformat)) {
			val datum = CoNLLDatum.constructFromCoNLL(chunk.split("\n"))
			data += datum
		}
		out.close
		return data.toArray
	}

	
	def main(args: Array[String]) = {
		val datums = read(new ArgParser(args))
	}
	*/

