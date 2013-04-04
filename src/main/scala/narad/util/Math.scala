package narad.util

object Average {
	def main(args: Array[String]) {
		val avg = args.foldLeft(0.0)(_+_.toDouble) / args.size
		System.out.println(avg)
	}
}

object HarmonicMean {
	def main(args: Array[String]) {
		assert(args.size == 2, "Harmonic Mean requires 2 arguments, found %d".format(args.size))
		val v1 = args(0).toDouble
		val v2 = args(1).toDouble
		val hm = 2 * (v1 * v2) / (v1 + v2)
		System.out.println(hm)
	}
}

object Math {

//  def combinations(elems: Array[T], k: Int): Array[Array[T]] = {
//  }

/*
  def groupings(elems: Seq[T], k: Int): Array[Array[T]] = {

  }

*/

  def harmonicMean(x: Double, y: Double): Double = {
    2 * (x * y) / (x + y)
  }
}