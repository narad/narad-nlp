package narad.stats
import narad.util.NestedHashCounter
import collection.immutable.HashSet
import collection.mutable.ArrayBuffer

class ConditionalProbabilityTable[T] extends NestedHashCounter[T]{
//  val events = new NestedHashCounter[T]
//  val contexts = new NestedHashCounter[T]

  def containsEvent(e: T): Boolean = this.exists(_._2.keys.exists(_ == e))  //this.filter(_._2 == e).size > 0

  def containsContext(c: T): Boolean = this.keys.exists(_ == c)  //this.keys.filter(_ == c).size > 0         //

  def contexts: List[T] = {
    keys.toList
  }

	override def increment(event: T, context: T, amount: Double = 1) = {
//    events += event
//    contexts += context
		super.increment(context, event)
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
		if (count(context) == 0) 0
		else {
//      val z = contexts.foldLeft(0.0)(_+count(_))
      count(context, event) / count(context)

    }
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
		for (t2 <- this.keys) {
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

  override def toString: String = {
    val ab = new StringBuilder()
    for (p <- this) {
      ab.append(p._1 + "\n")
      for (pp <- p._2) {
        ab.append("    " + pp._1 + "," + pp._2 + "\n")
      }
    }
    ab.toString
  }
}

object ConditionalProbabilityTable {

  def main(args: Array[String]) {
    val cpt = new ConditionalProbabilityTable[String]
    cpt.increment("cat", "the")
    cpt.increment("dog", "the")
    cpt.increment("rat", "the")
    cpt.increment("clawed", "cat")
    cpt.increment("clawed", "rat")
    System.err.println("CONTEXTS: " + cpt.contexts.mkString(" "))
    System.err.println("P(cat) = " + cpt.probability("cat"))
    System.err.println("P(clawed) = " + cpt.probability("clawed"))
    System.err.println("P(cat|the) = " + cpt.conditionalProbability("cat", "the"))
    System.err.println("P(clawed|cat) = " + cpt.conditionalProbability("clawed", "cat"))
  }
}
