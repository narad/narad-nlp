package narad.util

object Functions { 
	
	def argmax[T](f:T => Double, x:Seq[T]): T = {
		assert (x.size > 0, "Cannot do argmax on empty sequence.")
		var best = null.asInstanceOf[T]
		var bestscore: Double = null.asInstanceOf[Double]
		for (a <- x) {
			val score = f(a)
			if (best == null || score > bestscore) {
				best = a
				bestscore = score
			}
		}
		best
	}
	
	def argmin[T](f:T => Double, x:Seq[T]): T = {
		assert (x.size > 0, "Cannot do argmin on empty sequence.")
		var best = null.asInstanceOf[T]
		var bestscore: Double = null.asInstanceOf[Double]
		for (a <- x) {
			val score = f(a)
			if (best == null || score < bestscore) {
				best = a
				bestscore = score
			}
		}
		best
	}
	
	def max(a: Int, b: Int) = { if (a > b) a else b }

	def min(a: Int, b: Int) = { if (a > b) b else a }
	
	def max(a: Double, b: Double) = { if (a > b) a else b }

	def min(a: Double, b: Double) = { if (a > b) b else a }
	
}
