package narad.decoding
import scala.collection.mutable.ArrayBuffer

class Lattice[T](var lattice: Array[Array[T]]) {
	
	def rows = lattice(0).size
	def cols = lattice.size
	
	def toArray = lattice
	
	def pathsAsObject[X](wrapper:Seq[T] => X)(implicit m: ClassManifest[T]): Iterator[X] = {
		if (lattice.size == 1) {
			val iterator = for (e <- lattice(0)) yield wrapper(Seq(e))
			return iterator.iterator
		}
		else if (lattice.size == 2) {
			val iterator = for (e <- lattice(0); f <- lattice(1)) yield wrapper(Seq(e, f))
			return iterator.iterator
		}
		else if (lattice.size == 3) {
			val iterator = for (e <- lattice(0); f <- lattice(1); g <- lattice(2)) yield wrapper(Seq(e, f, g))
			return iterator.iterator
		}
		println("Unsupported lattice size in all paths method!")
		return null
	}

	def combinationsAsObject[X](wrapper:Seq[Seq[T]] => X, allPartitions: Boolean, maxCombination:Int=2)(implicit m: ClassManifest[T]): Iterator[X] = {
		var combs = new ArrayBuffer[X]

		if (lattice.size == 1 || allPartitions) {
			for (choose <- 1 to maxCombination; t <- 0 until lattice.size) {
			 	for (e <- lattice(t).combinations(choose)) {
					val comb = wrapper(Seq(e))
					println("... %s".format(comb))
					combs += comb
				}
//				iterator = iterator ++ it.iterator
//				return iterator.iterator
			}
		}
		
		if (lattice.size == 2) {
			val iterator = for (choose1 <- 1 to maxCombination; 
													e1 <- lattice(0).combinations(choose1); 
													choose2 <- 1 to maxCombination; 
													e2 <- lattice(1).combinations(choose2)) combs += wrapper(Seq(e1,e2))
//			return iterator.iterator
		}
		return combs.iterator
	}
	
	override def toString: String = {
		val sb = new StringBuilder
		for (j <- 0 until rows) {
			for (i <- 0 until cols) {
				sb.append(lattice(i)(j) + "\t")
			}
			sb.append("\n")
		}
		return sb.toString
	}
}


/*	
	def paths()(implicit m: ClassManifest[T]): Array[Array[T]] = {
		if (lattice.size == 1) {
			return 
		}
	}
	
	def combinationPaths()(implicit m: ClassManifest[T]): Array[Array[T]] = {
		return null
	}
	
}


	def allPaths()(implicit m: ClassManifest[T]): Array[Array[T]] = {
//		println("Finding paths with lattice of size = %d".format(lattice.size))
		if (lattice.size == 1) {
			return lattice(0).map(Array(_)).toArray
		}
		var tally = null.asInstanceOf[Array[Array[T]]]
		for (j <- lattice.size-1 to 0 by -1) {
			if (j == lattice.size-1) {
				tally = lattice(j).map(Array[T](_)).toArray
			}
			else {
				tally = crossConcat(lattice(j), tally)
			}
		}
		tally.toArray
	}
	
	def combinations()(implicit m: ClassManifest[T]) = {
		expand.allPaths
	}
	
	def expand()(implicit m: ClassManifest[T]): Lattice[Array[T]] = { //Array[Array[Array[T]]] = {
		val l2 = new Array[Array[Array[T]]](lattice.size)
		for (i <- 0 until l2.size) {
			l2(i) = subsets(lattice(i))
		}
		return new Lattice[Array[T]](l2)
	}

	
	def subsets(l: Array[T])(implicit m: ClassManifest[T]): Array[Array[T]] = {
		val buffer = new ArrayBuffer[Array[T]]()
		for (start <- 0 until l.size; window <- 0 to l.size - 1 - start) {
			val innerBuffer = new ArrayBuffer[T]
			for (i <- start to start+window) {
				innerBuffer += l(i)
			}
			buffer += innerBuffer.toArray
		}
		buffer.toArray
	}
	
	def crossConcat(elems: Array[T], lists: Array[Array[T]])(implicit m: ClassManifest[T]): Array[Array[T]] = {
		val buffer = new ArrayBuffer[Array[T]]
		for (e <- elems; l <- lists) {
			buffer += Array(e) ++ l
		}
		buffer.toArray
	}	
*/
	
