package narad.nlp.trees

case class GrammarRule(parent: String, children: Array[String]) {
	
//	override def equals(that: Any) = that match { 
//		case other: Rule => parent == other.parent && children == other.children 
//		case _ => false 
//	}
	
	override def toString = "%s => %s".format(parent, children.mkString(" "))
	
//	override def hashCode = toString.hashCode
}

class BinarizedRule(parent: String, leftChild: String, rightChild: String) extends GrammarRule (parent, Array(leftChild, rightChild)){}

class UnaryRule(parent: String, child: String) extends GrammarRule(parent, Array(child)) {}