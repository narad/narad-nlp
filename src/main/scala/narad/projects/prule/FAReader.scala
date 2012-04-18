package narad.projects.prule
import narad.util.ArgParser
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

// A Reader class for the force-aligned Providence corpus data
object FAReader {
	
	def main(args: Array[String]) = {
		val options = new ArgParser(args)
		val filename = options.getString("--fa.file")
		val words = readWords(filename)
		for (w <- words) println(w)
		
		val sb = new ArrayBuffer[String]
		words.foreach(_.iterator.foreach(sb += _))
		val sounds = sb.toArray.distinct
		println("Read %d phonemes".format(sounds.size))

		
//		val stems = new HashSet[String]
//		val suffixes
		
	}
	
	def readWords(filename: String): Array[PhoneticWord] = {
		val words = new ArrayBuffer[PhoneticWord]
		for (line <- io.Source.fromFile(filename).getLines) {
			words ++= line.split("\t").tail.map(w => new PhoneticWord(w.split(" ")))
		}
		val wa = words.toArray
		val wt = wa.distinct
		println("Read %d types from %d tokens.".format(wt.size, wa.size))
		return wa
	}

/*
	def init(words: Array[Word], maxMorphs: Int = 4): Array[Analysis] = {
		val random = new Random(1)

		return words.map { word => 
// generate maxMorphs random latent morphs
// each morpheme 
			val morphs = new ArrayBuffer
			var i = 0
			while (i < word.size && )
		}
	return null.asInstanceOf[Array[Analysis]]
	}
	*/
	
}



class Analysis(word: PhoneticWord, morphs: Array[String]) {
	
}

class PhoneticWord(phones: Array[String]) {
	
	def iterator = phones.iterator
	
	def size = phones.size
	
	override def equals(that: Any) = that match { 
   case other: PhoneticWord => other.toString == this.toString 
   case _ => false 
 }	

  override def toString = phones.mkString(" ")
}