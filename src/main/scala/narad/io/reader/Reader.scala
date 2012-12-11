package narad.io.reader

abstract class Reader[T] {
	
	def hasNext: Boolean
	
	def next
	
}




