package narad.nlp.ner

case class NamedEntity(tokens: Array[String], label: String, sentID: Int, start: Int, end: Int) {
	
	override def toString: String = "%s [%d, [%d,%d]]: %s".format(label, sentID, start, end, tokens.mkString(", "))
	
}