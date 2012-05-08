package narad.nlp.re
import java.io._
import scala.collection.mutable.ArrayBuffer
import narad.util.{ArgParser, ChunkReader}


object REReader {

	def iterator(filename: String): Iterator[REDatum] = {
		for (chunk <- ChunkReader.read(filename, "UTF-8")) yield {
			try {
				REDatum.construct(chunk.split("\n"))
			}
			catch {
				case e: Exception => {
					System.err.println("Error trying to create datum from string:\n%s\n".format(chunk))
					System.err.println("\n" + e.getStackTrace.mkString("\n") + "\n")
					System.exit(1)
			 		null.asInstanceOf[REDatum]
				}
			}
		}
	}
	
	def read(filename: String) = iterator(filename).toArray
	
	def main(args: Array[String]) = {
		val options  = new ArgParser(args)
		val filename = options.getString("--relation.file")
		val datums = read(filename)
		datums.foreach(println(_) + "\n")
	}
}
