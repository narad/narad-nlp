package narad.nlp.trees


	
	case class Span(left: Int, right: Int, label: String, unary: Boolean=true) {
		
		def width: Int = right - left	
		
		def covers(other: Span): Boolean = {
			return left <= other.left && 
			       right >= other.right && 
			       !equals(other)
		}
		
		def crosses(other: Span): Boolean = {
			return (start < other.start && end > other.start   && end < other.end) || 
			       (start > other.start && start < other.end && end > other.end)
		}
				
		override def equals(that: Any): Boolean = that match {
			case other: Span => {
				left == other.left && right == other.right && other.label == label				
			}
			case _=> false
		}
		
		def isUnary = unary
				
		def start = left				

		def end = right

		override def toString(): String = "%s(%s,%s)".format(label, left, right)	
}