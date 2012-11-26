package narad.util
import scala.collection.mutable.HashMap

class HashCounter[T] extends HashMap[T, Double] {
	
	override def clone: HashCounter[T] = {
		val e = new HashCounter[T]
		for (k <- e.keys) {
			e(k) = this(k)
		}
		return e
	}
		
	def count(t: T): Double = {
		this.getOrElse(t, 0)
	}
	
	def increment(t: T, amount: Double = 1) = {
		if (this.contains(t)) {
			this(t) += amount
		}
		else {
			this(t) = amount
		}
	}
}

class NestedHashCounter[T] extends HashMap[T, HashCounter[T]] {

	def count(t: T): Double = {
		if (this.contains(t)) {
			this(t).foldLeft(0.0)(_+_._2)
		}
		else {
			0
		}
	}
	
	def count(t1: T, t2: T): Double = {
		if (this.contains(t1) && this(t1).contains(t2)) {
			this(t1).count(t2)
		}
		else {
			0
		}
	}
	
	def increment(t1: T, t2: T, amount: Double = 1) = {
		if (!this.contains(t1)) {
			this(t1) = new HashCounter[T]
		}
		this(t1).increment(t2, amount)
	}
}

