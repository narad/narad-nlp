package narad.nlp.bio
import scala.xml.parsing._

object GeniaReader {
	
	def main(args: Array[String]) {
		read(args(0))
	}
	
	def read(filename: String) = {
		val xml = ConstructingParser.fromSource(scala.io.Source.fromFile(filename), false).document
		val setnences = xml \\ "_" foreach { node =>
			println(node)
		}
	}
}