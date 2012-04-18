package narad.nlp.phonology
import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, StringBuilder}

case class ArticulatoryFeature(name: String, value: Boolean) {
	override def toString = "[%s: %s]".format(name, value)
}

class SoundInventory extends HashMap[String, HashSet[ArticulatoryFeature]] {
	val featureNames = new HashSet[String]
	
	def add(tuple: (String, HashSet[ArticulatoryFeature])) = {
		featureNames ++= tuple._2.map(_.name)
		this += tuple
	}
	
	def createPhone(str: String): Phone = {
		assert(keySet contains str, "Cannot create phone from string '%s' - it is not in the inventory.".format(str))
		val fmap = new HashMap[String, Boolean]
		this(str).foreach(p => fmap += p.name -> p.value)
		return new Phone(str, fmap)
	}
	
	def createWord(str: String): PhoneticWord = createWord(str.split(" "))
	
	def createWord(strs: Array[String]): PhoneticWord = {
		return new PhoneticWord(strs.map(s => createPhone(s)))
	}
	
	def createWord(phone: Phone): PhoneticWord = {
		return new PhoneticWord(Array(phone))
	}
	
	def createWord(phones: Array[Phone]): PhoneticWord = {
		return new PhoneticWord(phones)
	}
	
	def getFeature(sound: String, fname: String): Option[ArticulatoryFeature] = {
		for (feat <- this(sound)) {
			if (feat.name == fname) {
				return Some(feat)
			}
		}
		return None
	}
	
	def sounds: Array[Phone] = {
		val buffer = new ArrayBuffer[Phone]
		for (sound <- keySet) {
			buffer += createPhone(sound)
		}
		buffer.toArray
	}
		
	override def toString: String = {
		println(this.get("P").mkString(","))
		println(this.get("B").mkString(","))
		val sb = new StringBuilder
		val fnames = featureNames.toArray
		sb.append("      %s\n".format(fnames.mkString("\t")))
		for (sound <- keys) {			
			val feats = this.get(sound)			
			sb.append("%s    %s\n".format(sound, fnames.map{ name => 
				val feat = getFeature(sound, name)
				feat match {
					case Some(x) => if (x.value) "+" else "-"
					case None => ""
				}
			}.mkString("\t")))
		}
		sb.toString
	}
	
}

object SoundInventory {
	
	def read(filename: String): SoundInventory = {
		val lines = io.Source.fromFile(filename).getLines.toArray
		val labels = lines.first.split("\t").tail
		val sounds = new SoundInventory
		for (line <- lines.tail) {
			val cells = line.split("\t")
			val feats = new HashSet[ArticulatoryFeature]
			cells.tail.zipWithIndex.foreach { case(value, index) =>
				if (value == "+" || value == "-") {
					feats += ArticulatoryFeature(labels(index), value == "+")
				}
			}
			sounds.add(cells.first -> feats)
		}
		sounds
	}
	
}