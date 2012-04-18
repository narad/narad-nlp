package narad.nlp.parse


	
	case class Span(left: Int, right: Int, label: String, unary: Boolean=true) {
		def width(): Int = right - left	
		def covers(otherSpan: Span): Boolean = left <= otherSpan.left && right >= otherSpan.right && !equals(otherSpan)
		def equals(otherSpan: Span): Boolean = left == otherSpan.left && right == otherSpan.right && otherSpan.label == label
		override def toString(): String = "%s(%s,%s)".format(label, left, right)
		def start = left
		def end = right
		def isUnary = unary
				
	}