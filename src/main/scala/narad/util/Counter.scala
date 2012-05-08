package narad.util
import scala.collection.mutable.HashMap

class HashCounter extends HashMap[String, Double] {
	
	override def clone: HashCounter = {
		val e = new HashCounter
		for (k <- e.keys) {
			e(k) = this(k)
		}
		return e
	}
		
	def count(str: String): Double = {
		this.getOrElse(str, 0)
	}
	
	def increment(str: String, amount: Double = 1) = {
		if (this.contains(str)) {
			this(str) += amount
		}
		else {
			this(str) = amount
		}
	}
}