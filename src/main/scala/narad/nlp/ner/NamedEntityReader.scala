package narad.nlp.ner

import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex
import narad.util.ArgParser

object NamedEntityReader {
	val ontoSingle    = new Regex("""<ENAMEX_TYPE=\"(.*?)\">([^<>]+)</ENAMEX>""")
	val ontoStart     = new Regex("""<ENAMEX_TYPE=\"(.*?)\">([^<]+)""")
	val ontoEnd   = new Regex("""([^<>]+)</ENAMEX>""")

	def read(filename: String, options: ArgParser): Array[NamedEntity] = {
		val entities = new ArrayBuffer[NamedEntity]
		var count = 0
		var label = null.asInstanceOf[String]
		var tokens = new ArrayBuffer[String]
		val min = options.getString("-min", "0").toInt
		val max = options.getString("-max", "99999").toInt
		for (line <- io.Source.fromFile(filename).getLines) {
			var words = line.replaceAll("ENAMEX TYPE", "ENAMEX_TYPE").split(" ")
			if (words.size >= min && words.size <= max) {
				if (options.getBoolean("-print", false)) { println(line) }
				words.zipWithIndex.foreach { case(token, index) =>
					token match {
						case ontoSingle(entityLabel, entityString) => {
							entities += new NamedEntity(Array(entityString), entityLabel, count, index, index+1)						
						}
						case ontoStart(entityLabel, entityString) => {
							tokens += entityString
							label = entityLabel
						}
						case ontoEnd(entityString) => {
							tokens += entityString
							entities += new NamedEntity(tokens.toArray, label, count, index+1-tokens.size, index+1)
							tokens = new ArrayBuffer[String]
						}
						case default => {}
					}
				}
			}
			count += 1
		}
		entities.toArray
	}
	
	def main(args: Array[String]) {
		val options = new ArgParser(args)
		val entities = read(options.getString("-nerFile"), options)
	}
	
}