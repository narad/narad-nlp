package narad.stats
import scala.collection.mutable.ArrayBuffer

class Urn[T](var pebbles: Array[(T, Double)] = Array[(T, Double)]()) {
	val random = new util.Random

	def add(pebble: T, weight: Double) = {
		pebbles = pebbles :+ (pebble, weight)
	}
	
	def draw: T = {
		assert(!pebbles.isEmpty, "Cannot sample from an empty Urn.")
		val sum = pebbles.foldLeft(0.0)(_+_._2)
		val portion = random.nextDouble() * sum
		var total = 0.0
		for (p <- pebbles) {
			total += p._2
			if (total >= portion) {
				return p._1
			}
		}
		pebbles.last._1
	}

	def drawMany(n: Int=1, replace: Boolean=true)(implicit m: ClassManifest[T]): Array[T] = {
		assert(replace || n <= pebbles.size, "Cannot draw %d unique samples from %d elements".format(n, pebbles.size))
		val drawn = new ArrayBuffer[T]
		while (drawn.size < n) {
			drawn += draw
		}
		drawn.toArray
	}
	
	def size = pebbles.size

}










/*
while (total < portion) {
	e = pebbles.next
	total += e._2
}

class Urn[T] { //extends collection.mutable.ListBuffer[(T, Double)] {  // (e: Array[T] = Array(), w: Array[Double] = Array()) { // 
	val random = new util.Random
	
	
	
	
	
	
	
	
	
	
	
	
//	val elems = new ArrayBuffer[T]()
//	val weights = new ArrayBuffer[Double]
//	weights ++= w
	//	def add(elem: T, weight: Double) = elems += Tuple2(elem, weight)

	val elems = new collection.mutable.ArrayBuffer[(T, Double)]
	
	def size = elems.size

	def add(e: Array[(T, Double)]) = { 
		elems ++= e 
	}
	
	def add(elem: T, weight: Double) = { 
		elems += Tuple2(elem, weight)
	}
	
	def addAll(l: Array[(T, Double)]) = elems ++= l

	def sample: T = {
//		println(elems.size)
		val sum = elems.foldLeft(0.0)(_+_._2)
		val portion = random.nextDouble() * sum
		var total = 0.0
		for (e <- elems) {
			total += e._2
			if (total >= portion) {
				return e._1
			}
		}
	return elems.last._1
	}


  	
	def sample(n: Int=1)(implicit m: ClassManifest[T]): Array[T] = {
//		println("doing mass sample (%d)".format(n))
		val buffer = new ArrayBuffer[T]
		if (elems.size == 0)
		  return buffer.toArray
		if (n >= elems.size)  // Need to break this apart into a sample with/without replacement where it could be appropriately applied
			return elems.map(_._1).toArray
		var i = 0
		while (i < n) {
			buffer += sample
			i += 1
		}
		buffer.toArray
	}
	
	
	def toArray = elems.toArray

}

object Urn {
	
	def buildUrn[T](e: Array[(T, Double)]): Urn[T] = {
		val urn = new Urn[T]
		e.foreach { t => urn.add(t._1, t._2) }
		return urn
	}
	
/*	def sample[T](l: Array[(T, Double)], maxSamples: Int=1): Array[T] = {
		val urn = new Urn[T]
		l.foreach { t => urn.add(t._1, t._2) }
		val samples: Array[T] = urn.sample(maxSamples)		
		samples
	}
*/
	
}


*/



