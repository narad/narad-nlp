package narad.nlp.re
import java.io.FileWriter
import narad.util.{ArgParser, ChunkReader}
import scala.collection.mutable.HashSet

object REStatistics {
	
	def main(args: Array[String]) {
		val options = new ArgParser(args)
		val filename = options.getString("--re.file")
		for (datum <- REReader.read(filename)) {
			println(datum)
		}
	}
	
}