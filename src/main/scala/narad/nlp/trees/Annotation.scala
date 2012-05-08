package narad.nlp.trees


case class Annotation() extends scala.collection.mutable.HashMap[String, String] {
	def label(): String = this("label")
	def label(l: String) = this += "label" -> l
	override def toString: String = "[%s,%s] %s: %s".format(this("start"), this("end"), this("label"), this("entity"))

}