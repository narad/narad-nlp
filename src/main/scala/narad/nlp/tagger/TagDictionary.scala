package narad.nlp.tagger
import java.io.{File, FileWriter}
import scala.collection.mutable.{HashMap, HashSet}
import narad.io.conll.CoNLLReader

class TagDictionary extends HashMap[String, HashMap[String, Int]]{
//	val hash = new HashMap[String, HashMap[String, Int]]
	private val set = new HashSet[String]

	def add(w: String, tag: String) {
		val word = w.toLowerCase
		set += tag
		if (this.contains(word)) {
			if (this(word).contains(tag)) {
				this(word)(tag) += 1
			}
			else {
				this(word)(tag) = 1
			}
		}
		else {
			this(word) = new HashMap[String, Int]
			this(word)(tag) = 1
		}
	}

	def all: Array[String] = { //HashSet[String] = {
		set.toList.sortBy(_.toString).toArray
	}

	override def contains(w: String): Boolean = {
		val word = w.toLowerCase
		super.contains(word)
	}
	
//	def getOrElse(word: String, e: T) = hash.getOrElse(word, e)
	
	def tags(w: String): Iterator[String] = {
		val word = w.toLowerCase
		this.getOrElse(word, new HashMap[String, Int]()).keys.toList.sortBy(_.toString).iterator
	}

	def tags(w: String, threshold: Int): Iterator[String] = {
		val word = w.toLowerCase
		this.getOrElse(word, new HashMap[String, Int]()).filter(_._2 >= threshold).keys.toList.sortBy(_.toString).iterator
	}
	
	def toFile(filename: String) = {
		val out = new FileWriter(filename)
//		out.write(dict.all.mkString("\t"))
		for (word <- words.toList.sortBy(_.toString)) {
			out.write(word + "\t" + tags(word).mkString("\t") + "\n")
		}
		out.close
	}
	
	def words = keys
}


object TagDictionary {

	def construct(filename: String, mode: String = "FINE"): TagDictionary = {
		val dictionary = new TagDictionary
    val reader = new CoNLLReader(filename)
    for (datum <- reader) {
      for (i <- 1 to datum.slen) {
        mode match {
          case "COARSE" => dictionary.add(datum.word(i), datum.cpostag(i))
          case "FINE"   => dictionary.add(datum.word(i), datum.postag(i))
          case "CASE"   => dictionary.add(datum.word(i), datum.mcase(i))
//          case "PERSON" => dictionary.add(datum.word(i), datum.person(i))
          case "GENDER" => dictionary.add(datum.word(i), datum.mgender(i))
          case "NUMBER" => dictionary.add(datum.word(i), datum.mnumber(i))
          case "CASE+GENDER+NUMBER" => dictionary.add(datum.word(i),
            datum.mcase(i) + "|" + datum.mgender(i) + "|" + datum.mnumber(i))
        }
      }
    }

		for (line <- io.Source.fromFile(filename).getLines) {
			val cols = line.split("\t")
			if (cols.size > 3 && mode == "COARSE") {
				val word = cols(1)
				dictionary.add(word, cols(3))
			}
			if (cols.size > 4 && mode == "FINE") {
				val word = cols(1)
				dictionary.add(word, cols(4))
			}
			if (cols.size > 4 && mode == "CONCAT") {
				val word = cols(1)
				assert(cols(3) != "" && cols(4) != "", "Line has a POS error: %s".format(line))
				dictionary.add(word, cols(3) + "^" + cols(4))
			}
		}
		return dictionary
	}

	def fromFile(filename: String): TagDictionary = {
		val dictionary = new TagDictionary
		for (line <- io.Source.fromFile(filename).getLines) {
			val cols = line.split("\t")
			for (i <- 1 until cols.size) {
				dictionary.add(cols(0), cols(i))
			}
		}
		return dictionary
	}
}





/*
		val dictionary = new TagDictionary
		for (line <- io.Source.fromFile(filename).getLines) {
			val cols = line.split("\t")
			if (cols.size > 3 && mode == "COARSE") {
				val word = cols(1)
				dictionary.add(word, cols(3))
			}
			if (cols.size > 4 && mode == "FINE") {
				val word = cols(1)
				dictionary.add(word, cols(4))
			}
			if (cols.size > 4 && mode == "CONCAT") {
				val word = cols(1)
				assert(cols(3) != "" && cols(4) != "", "Line has a POS error: %s".format(line))
				dictionary.add(word, cols(3) + "^" + cols(4))
			}
		}
		return dictionary
	}

 */

/*
			.zipWithIndex { case(line, i) =>
				if (i == 0) {
					
				}
				else {
					
				}
			}
		}
*/		





/*
class TagDictionary {
	val hash = new HashMap[String, HashSet[String]]
	val set = new HashSet[String]
	
	def add(word: String, tag: String) = {
		set += tag
		if (hash.contains(word)) {
			hash(word) += tag
		}
		else {
		  hash(word) = new HashSet
		  hash(word) += tag
		}
	}
	
	def all: HashSet[String] = {
//		val tags = new HashSet[String]
//		for (k <- hash.keys; t <- hash(k)) {
//			tags += t
//		}
//		return tags
		set
	}
	
	def contains(word: String): Boolean = {
		hash.contains(word)
	}
	
	def tags(word: String): Iterator[String] = {
		hash.getOrElse(word, new HashSet()).iterator
	}
}
*/