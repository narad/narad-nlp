package narad.nlp.phonology
import scala.collection.mutable.{ArrayBuffer, HashMap}

class PhoneticWord (val phones: Array[Phone]) extends Iterator[Phone]{
		var idx = 0

		def hasNext: Boolean = {
			val nextCheck = idx < phones.size
			if (nextCheck == false) {
				idx = 0
			}
			nextCheck
		}

		def next: Phone = { 
			val phone = phones(idx); 
			idx += 1; 
			phone
		}
		
		def toArray = phones
		
		def project: Array[HashMap[String, Boolean]] = phones.map(_.features).toArray

		def projectAsArray: Array[Array[(String, Boolean)]] = phones.map(_.features.toArray).toArray

		def equals(word: PhoneticWord) = word.map(_.form).mkString("_") == this.map(_.form).mkString("_")
		
		def get(i: Int): Phone = phones(i)
		
		def toVerboseString: String = {
			val buffer = new ArrayBuffer[String]
			buffer += toString
			for (phone <- phones) {
				buffer += "%s:\t%s".format(phone.toString, phone.features.toString)
			}
			buffer.mkString("\n")
		}

				
		override def toString: String = phones.mkString(" ")
		
		override def hashCode = Tuple(toString, toString).hashCode

		override def equals(other: Any) = other match { 
			case other: PhoneticWord => other.toString == toString
			case _ => false 
		}
		
}









	
/*	
	val wordform = wordform1
	val phones = wordform.split("").tail.map(new Phone(_))

/*	
	def phones: Iterable[Phone] = new Iterable[Phone]{
		def iterator = wordform.split("").tail.map(new Phone(_)).toList.iterator
	}
*/
	
	def project: Array[OTFeat] = {
		val feats = new ArrayBuffer[OTFeat]
		for (i <- 0 until size-1) {
			for (f1 <- new Phone(wordform.substring(i, i+1)).features) {
				for (f2 <- new Phone(wordform.substring(i+1, i+2)).features) {
					feats += new OTFeat("*%sx%s".format(f1._2, f2._2), f1._2, f2._2)
//					feats += new OTFeat("*%s=%sx%s=%s".format(f1._1, f1._2, f2._1, f2._2))
				}
			}
		}
		feats.toArray
	}
	
	override def toString = wordform
	
	var idx = 0
	
	def hasNext: Boolean = {
		val nextCheck = idx < phones.size
		if (nextCheck == false) {
			idx = 0
		}
		nextCheck
	}
	
	def next: Phone = {
		if (idx+1 < wordform.size && wordform.substring(idx+1, idx+2) == ":") {
			val phone = new Phone(wordform.substring(idx, idx+2)) 
			idx += 2
			return phone
		}
		else {
			val phone = new Phone(wordform.substring(idx, idx+1))
			idx +=1 
			return phone
		}
//		val phone = phones(idx); idx += 1; phone 
	}
	
	def reset = idx = 0
}

*/

