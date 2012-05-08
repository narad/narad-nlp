package narad.nlp.re

case class Relation(label: String, sublabel: String = "None", arg1: EntityMention, arg2: EntityMention) {
	
	def encompasses(other: Relation): Boolean = {
		arg1.encompasses(other.arg1) || arg1.encompasses(other.arg2) ||
		arg2.encompasses(other.arg1) || arg2.encompasses(other.arg2)
	}
	
	def matches(str: String): Boolean = {
		return str.contains(arg1.text) && str.contains(arg2.text)
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
	
	def sharesMention(other: Relation): Boolean = {
		return this.arg1 == other.arg1 || this.arg2 == other.arg1 || this.arg1 == other.arg2 || this.arg2 == other.arg2
	}
	
	def countCharacter(start: Int, end: Int, str: String, delim: String): Int = {
		var count = 0
		for (i <- start to end-delim.size) {
			if (str.substring(i, i+delim.size) == delim) count += 1
		}
		return count
	}
	
	def overlaps =  (arg1.start <= arg2.start && arg1.end >= arg2.start) || 
	 							 	(arg2.start <= arg1.start && arg2.end >= arg1.start)
	
	override def toString = "RELATION[%s-%s]:\nARG-1: %s\nARG-2:%s".format(label, sublabel, arg1, arg2)
}

case class EntityMention(text: String, start: Int = -1, end: Int = -1, label: String = "N/A", sublabel: String = "N/A") {
	
	def countCharacter(start: Int, end: Int, str: String, delim: String): Int = {
		var count = 0
		for (i <- start to end-delim.size) {
			if (str.substring(i, i+delim.size) == delim) count += 1
		}
		return count
	}
	
	def encompasses(other: EntityMention): Boolean = {
		(overlaps(other) && width > other.width)
	}
	
	def matchIndices(str: String): (Int, Int) = {
		val a1 = str.indexOf(text)
		val s1 = text.split(" ").size
		val i1 = countCharacter(0, a1, str, " ")
		val answer =  new Tuple2(i1, i1 + s1 - 1)
		return answer
	}
	
	def matches(str: String): Boolean = {
		return str.contains(text)
	}
	
	def overlaps(other: EntityMention): Boolean = {
		(start <= other.start && end >= other.start) || (start <= other.end && end >= other.end) || (start <= other.start && end >= other.end)
	}
	
	override def equals(that: Any) = that match { 
		case other: EntityMention => other.text == text 
		case _ => false 
	}
	
	override def toString = "[%d,%d]: %s".format(start, end, text)

	def width = end - start
}