package narad.util
import java.io.File

object Averager {
	
	def main(args: Array[String]) = {
		val options = new ArgParser(args)
		val input = options.getString("--input")
		val range = options.getInt("--range")
		val params = new Array[Array[Double]](range)
		var maxpv = 0
		for (i <- 1 to range) {
			val file = new File(input + "." + i)
			if (file.exists) {
				val pv = io.Source.fromFile(file).getLines().map(_.toDouble).toArray
				if (pv.size > maxpv) maxpv = pv.size
				params(i-1) = pv
			}
		}
		for (i <- 0 until maxpv) {
			var sum = 0.0
			var c = 0
			for (j <- 1 to range if (params(j-1) != null)) {
				sum += params(j-1)(i)
				c += 1
			}
			println(sum / c.toDouble)			
		}
	}
}