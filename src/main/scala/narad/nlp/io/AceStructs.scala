package narad.nlp.io
import narad.nlp.re._
import scala.collection.mutable.ArrayBuffer

case class ACEArg(start: Int, end: Int, text: String) {
	def overlaps(other: FullMention): Boolean = {
		(start <= other.start && end >= other.start) || (start <= other.end && end >= other.end) || (start <= other.start && end >= other.end)
	}
	
	def encompasses(other: ACEArg): Boolean = {
		(overlaps(other) && width > other.width)
	}
		
	def overlaps(other: ACEArg): Boolean = {
		(start <= other.start && end >= other.start) || (start <= other.end && end >= other.end) || (start <= other.start && end >= other.end)
	}
	
	def width = end - start
}

case class ACEEntity(label: String, sublabel: String, mentions: Array[ACEEntityMention]) {
	override def toString = "Entity [%s, %s]:\n%s".format(label, sublabel, mentions.mkString("\n"))
}

case class ACEEntityMention(label: String, start: Int, end: Int, text: String) {}

case class ACERelation(label: String, sublabel: String, arg1: ACEArg, arg2: ACEArg) {
	def matches(str: String): Boolean = {
//		return str.contains(arg1.text) && str.contains(arg2.text)
		return (str.contains(" " + arg1.text + " ") || str.startsWith(arg1.text + " ") || str.endsWith(" " + arg1.text)) &&
					 (str.contains(" " + arg2.text + " ") || str.startsWith(arg2.text + " ") || str.endsWith(" " + arg2.text))

	}

	def overlaps =  (arg1.start <= arg2.start && arg1.end >= arg2.start) || 
	(arg2.start <= arg1.start && arg2.end >= arg1.start)

	def encompasses(other: ACERelation): Boolean = {
		arg1.encompasses(other.arg1) || arg1.encompasses(other.arg2) ||
		arg2.encompasses(other.arg1) || arg2.encompasses(other.arg2)
	}


	def matchIndices(str: String): ((Int, Int), (Int, Int)) = {
		val a1 = str.indexOf(arg1.text)
		val a2 = str.indexOf(arg2.text)
		val s1 = arg1.text.split(" ").size
		val s2 = arg2.text.split(" ").size
		val i1 = countCharacter(0, a1, str, " ")
		val i2 = countCharacter(0, a2, str, " ")
		val answer =  new Tuple2(new Tuple2(i1, i1 + s1 - 1), new Tuple2(i2, i2 + s2 - 1))
		return answer
	}

	def sharesMention(other: ACERelation): Boolean = {
		return this.arg1 == other.arg1 || this.arg2 == other.arg1 || this.arg1 == other.arg2 || this.arg2 == other.arg2
	}

	def countCharacter(start: Int, end: Int, str: String, delim: String): Int = {
		var count = 0
		for (i <- start to end-delim.size) {
			if (str.substring(i, i+delim.size) == delim) count += 1
		}
		return count
	}
}

case class FullRelation(label: String, sublabel: String, arg1: FullMention, arg2: FullMention) {	
}

case class FullMention(label: String, elabel: String, esublabel: String, start: Int, end: Int, text: String) {
	def matches(str: String): Boolean = {
		return str.contains(" " + text + " ") || str.startsWith(text + " ") || str.endsWith(" " + text)
	}	
	
	def countCharacter(start: Int, end: Int, str: String, delim: String): Int = {
		var count = 0
		for (i <- start to end-delim.size) {
			if (str.substring(i, i+delim.size) == delim) count += 1
		}
		return count
	}
	
	def encompasses(other: FullMention): Boolean = {
		(overlaps(other) && width > other.width)
	}
	
	def matchIndices(str: String): (Int, Int) = {
		val a1 = str.indexOf(text + " ")
		val s1 = text.split(" ").size
		val i1 = countCharacter(0, a1, str, " ")
		val answer =  new Tuple2(i1, i1 + s1 - 1)
		return answer
	}
		
	def overlaps(other: FullMention): Boolean = {
		(start <= other.start && end >= other.start) || (start <= other.end && end >= other.end) || (start <= other.start && end >= other.end)
	}
	
	override def equals(that: Any) = that match { 
		case other: EntityMention => other.text == text 
		case _ => false 
	}
	
	override def toString = "[%d,%d]: %s".format(start, end, text)

	def width = end - start
}