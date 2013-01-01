package narad.io.onto
import narad.nlp.ner._
import narad.util.ArgParser
import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.util.matching.Regex
import collection.mutable

class NamedEntityReader(filename: String, coarsen: Boolean = true) extends mutable.Iterable[NamedEntityDatum] {
	val ontoSingle = new Regex("""<ENAMEX_TYPE=\"(.*?)\">([^<>]+)</ENAMEX>""")
	val ontoStart  = new Regex("""<ENAMEX_TYPE=\"(.*?)\">([^<]+)""")
	val ontoEnd    = new Regex("""([^<>]+)</ENAMEX>""")
	val labelmap   = constructLabelMap
	val normalmap  = constructNormalMap
	
	def constructNormalMap: HashMap[String, String] = {
		val map = new HashMap[String, String]
		map += "PER" -> "PERSON"
		map
	}
	
	def constructLabelMap: HashMap[String, String] = {
		val map = new HashMap[String, String]
		map += "PERSON" -> "PERSON"
		map += "NORP" -> "ORG"
		map += "FAC" -> "LOC"
		map += "ORG" -> "ORG"
		map += "GPE" -> "LOC"
		map += "LOC" -> "LOC"
		map += "MISC" -> "MISC"
		map += "PRODUCT" -> "MISC"
		map += "EVENT" -> "MISC"
		map += "WORK_OF_ART" -> "MISC"
		map += "LAW" -> "MISC"
		map += "LANGUAGE" -> "MISC"
		map += "DATE" -> "MISC"
		map += "TIME" -> "MISC"
		map += "PERCENT" -> "MISC"
		map += "MONEY" -> "MISC"
		map += "QUANTITY" -> "MISC"
		map += "ORDINAL" -> "MISC"
		map += "CARDINAL" -> "MISC"
		map
	}

//	def read(filename: String, options: ArgParser): Iterator[NamedEntityDatum] = {
//		val coarsen = options.getString("--ner.label.type", "FINE") == "COARSE"
	def iterator: Iterator[NamedEntityDatum] = {
  for (line <- io.Source.fromFile(filename).getLines() if !line.isEmpty) yield entities(line, coarsen)
  //			for (line <- LineReader.read(filename) if !line.isEmpty) yield entities(line, coarsen)
	}

	def entities(line: String, coarsen: Boolean = false): NamedEntityDatum = {
//		System.err.println(line.size + ": " + line)
		val entities = new ArrayBuffer[NamedEntity]
		var count = 0
		var label = null.asInstanceOf[String]
		var words = line.replaceAll("ENAMEX TYPE", "ENAMEX_TYPE").split(" ")
		var tokens = new ArrayBuffer[String]		
		words.zipWithIndex.foreach { case(token, index) =>
			token match {
				case ontoSingle(entityLabel, entityString) => {
					entities += new NamedEntity(Array(entityString), refine(entityLabel, coarsen), count, index, index+1)						
				}
				case ontoStart(entityLabel, entityString) => {
					tokens += entityString
					label = refine(entityLabel, coarsen)
				}
				case ontoEnd(entityString) => {
					tokens += entityString
					entities += new NamedEntity(tokens.toArray, refine(label, coarsen), count, index+1-tokens.size, index+1)
					tokens.clear()
				}
				case default => {
					if (tokens.size > 0) tokens += token
				}
			}
			count += 1
		}
//		System.err.println("# entities = " + entities.size)
		new NamedEntityDatum(words.size, entities.toArray)
	}
	
	def refine(label: String, coarsen: Boolean): String = {
		val rlabel = normalmap.getOrElse(label, label)
		assert(labelmap.contains(rlabel), "Label %s not found in NER label map.".format(rlabel))
		if (coarsen) {
			return labelmap.getOrElse(rlabel, rlabel)
		}
		else {
			return rlabel
		}
	}
}

case class NamedEntityDatum(slen: Int, entities: Array[NamedEntity]) {
	def containsSpan(start: Int, end: Int): Boolean = entities.exists(e => e.start == start && e.end == end)
	def containsSpanLabel(start: Int, end: Int, label: String): Boolean = entities.exists(e => e.start == start && e.end == end && e.label == label)
	def coversSpan(start: Int, end: Int): Boolean = entities.exists(e => e.start <= start && e.end >= end && e.width > 1)
	def labels = entities.map(_.label)
	def size = slen
}



/*
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

*/

