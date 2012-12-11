package narad.stats
import narad.util.NestedHashCounter

class ConditionalProbabilityTable[T] extends NestedHashCounter[T]{
	
	override def increment(event: T, context: T, amount: Double = 1) = {
		super.increment(event, context)
	}
		
	def probability(event: T): Double = {
		if (contains(event)) {
			val z = keys.foldLeft(0.0)(_+count(_))
			count(event) / z
		}
		else {
			0.0
		}
	}
	
	// P(t2|t1)
	def conditionalProbability(event: T, context: T): Double = {
		if (count(event) == 0) 0
		else count(event, context) / count(event)
	}
/*
		if (contains(event)) {
			count(event, context) / count(event)
		}
		else {
			0
		}
	}
*/
	
	def mostProbable: (T, Double) = {
		var maxProb = 0.0
		var maxElem = null.asInstanceOf[T]
		for (k <- keys) {
			val prob = probability(k)
			if (prob > maxProb) {
				maxProb = prob
				maxElem = k
			}
		}
		(maxElem, maxProb)
	}
	
	// ArgMax P(x|t1)
	def mostProbable(t1: T): (T, Double) = {
		var maxProb = 0.0
		var maxElem = null.asInstanceOf[T]
		for (t2 <- this(t1).keys) {
			val prob = conditionalProbability(t1, t2)
			if (prob > maxProb) {
				maxProb = prob
				maxElem = t2
			}
		}
		(maxElem, maxProb)
	}
	
	def probs: Array[((T, T), Double)] = {
		val probs = (for (k1 <- keys; k2 <- this(k1).keys) yield ((k1, k2), conditionalProbability(k1, k2)))
		return probs.toArray
	}
}

