package narad.stats

object Correlation {
	
		def pearson(l1: Array[Double], l2: Array[Double]): Double = {
			assert(l1.size == l2.size, "Lists must be equal size for Correlation.")
			val size = l1.size
			val xsum = l1.foldLeft(0.0)(_+_)
			val ysum = l2.foldLeft(0.0)(_+_)
			val xsquare = l1.toList.foldLeft(0.0)(_+square(_))
			val ysquare = l2.toList.foldLeft(0.0)(_+square(_))
			var xy = 0.0
			for (i <- 0 until l1.size) { xy += l1(i) * l2(i) }
			val c1 = xsquare - square(xsum) / size
			val c2 = ysquare - square(ysum) / size
			val c3 = xy - (xsum * ysum) / size
			return c3 / Math.sqrt(c1 * c2)
		}

		def square(n: Double) = Math.pow(n, 2)



		// Spearman's rank correlation coefficient is a nonparametric measure expressing the correlation
		// between two variables.  The closer to +1 or -1, the higher the association.  A negative value
		// indicates an inversely correlated relationship.
		def spearman(l1: Array[Double], l2: Array[Double]): Double = {
			assert(l1.size == l2.size, "Lists must be equal size for Spearman Correlation.")
			val n = l1.size
			val c1 = rank(l1, tiebreak="average")//l1.zipWithIndex.sortBy(_._1).map(_._2)
			val c2 = rank(l2, tiebreak="average")//l2.zipWithIndex.sortBy(_._1).map(_._2)
			val sumSquaredDiff = (c1 zip c2).map{
				x => square((x._1 - x._2))
			}.foldLeft(0.0)(_+_)
			val r = 1.0 - (6 * sumSquaredDiff) / (n * (square(n)-1)) 
			val f = r * Math.sqrt(((n-2.0)/(1.0-square(r)))) // n may have to be number of pairwise comps instead??
//		println("spearman confidence: %f".format(f))  Should be .686 under t-distribution as above
			return r
		}

		def testSpearman() = {
			val iq = Array[Double](86, 97, 99, 100, 101, 103, 106, 110, 112, 113)
			val tv = Array[Double](0, 20, 28, 27, 50, 29, 7, 17, 6, 12)
			val s1 = spearman(iq, tv)
			println(s1)
			assert(-0.17575757575757578 == s1, "Spearman results not replicated in the case of test case 1.")			
			
			val x = Array[Double](22, 24, 30, 40, 45, 50, 50, 52, 64, 64, 64, 72, 78, 78, 84, 90)
			val y = Array[Double](36, 24, 25, 20, 48, 44, 40, 56, 62, 68, 56, 32, 78, 68, 68, 58)
			val s2 = spearman(x, y)
			println(s2)
			assert(0.774264705882353 == s2, "Spearman results not replicated in the case of test case 2.")			

			val c1 = Array[Double](106, 86, 97, 113, 120, 110)
			val c2 = Array[Double](7, 0, 20, 12, 12, 17)
			val s3 = spearman(c1, c2)
			println(s3)
			assert(.24285714285714288 == s3, "Spearman results not replicated in the case of test case 3.")			
		}

		// Assumes l is a sorted ranking
		def rank(l: Array[Double], tiebreak:String="average"): Array[Double] = {
	//		val counts = l.distinct.map(x => Tuple(x, l.count(_ == x))) //.filter(_._2 > 1)
	//		println(counts.mkString(","))
			val sl = l.zipWithIndex.sortBy(_._1)
			val elems = sl.map(_._1)
			val ranks = sl.map(_._2)
//			println(elems.mkString(","))
//			println(ranks.mkString(","))
			l.map { e =>
				val first = elems.indexOf(e) + 1
				val last  = elems.lastIndexOf(e) + 1
	//			if (first == last) {
	//				first
	//			}
	//			else{
					(first to last).toList.foldLeft(0.0)(_+_) / (1+last-first)
	//			}
			}
		}
	//
}