package narad.nlp.ner

case class NamedEntity(tokens: Array[String], label: String, sentID: Int, start: Int, end: Int) {
	
	override def toString: String = "%s [%d, [%d,%d]]: %s".format(label, sentID, start, end, tokens.mkString(", "))
	
	override def equals(that: Any) = that match { 
	   case other: NamedEntity => {
//			other.tokens.mkString(" ") == tokens.mkString(" ") && 
			other.start == start &&
			other.end == end &&
			other.label == label
		} 
	   case _ => false 
	 }
	
	def width = end - start
}