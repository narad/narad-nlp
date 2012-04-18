package narad.learn
import scala.collection.mutable.{ArrayBuffer, HashMap}
import narad.nlp.phonology._
import narad.projects.otlearn._

class Feature(val label: String, var weight: Double = 1.0) {}


/*

object FeatureFactory {
	val slicePattern = """(\[([^\]]+)\])|( [A-Z]+ )""".r
	val	binPattern   = """\[?(\^?)(\+|\-)(.+)\]?""".r
	val charPattern  = """ ([A-Z]+) """.r
	
	def parseFeature(fconstraints: Array[HashMap[String, Boolean]]): Feature = {
		val label = fconstraints.map { slice =>
			"[%s%s]".format(if (slice.toArray.contains(("negate", true))) "^" else "", slice.filter(_._1 != "negate").map { pair =>
				if (pair._1 == "any" && pair._2 == true) {
					"_"
				}
				else {
					"%s%s".format(if (pair._2) "+" else "-", pair._1)					
				}					
			}.mkString(","))
		}.mkString("")
		new Feature(label, 0.0, fconstraints)
	}
	
	def parseFeature(str: String): Feature = {
		val buffer = new ArrayBuffer[HashMap[String, Boolean]]
		slicePattern findAllIn str foreach { slice => 
			val hm = new HashMap[String, Boolean]
			for (bin <- slice.replaceAll("\\[|\\]", "").split(",")) {
				if (bin == " ") {
					hm += "any" -> true
				}
				else if (bin.matches(" [A-Z]+ ")) {
					val charPattern(sound) = bin
					hm += sound -> true
				}
				else {
					val binPattern(reverse, sign, name) = bin
					if (reverse == "^") hm += "negate" -> true
					hm += Tuple2(name, sign == "+")					
				}
			}
			buffer += hm
		}
		new Feature(str, 0.0, buffer.toArray)
	}
}

case class BinaryFeature(label: String, value: Boolean) {}

//case class Feature(label: String, var weight: Double, violations: ()

class Feature(l: String, var w: Double = 0.0, val fconstraints: Array[HashMap[String, Boolean]]) {
	val wPattern = """\[([^\]]+)\]""".r
	val dPattern = """ """.r
	val bpPattern = """\[?(\+|\-)(.*)\]?""".r
	
	def constraints = fconstraints
	
	def label = l

	def weight = w
	
	def width = fconstraints.size
	
	def window = fconstraints.size
	
	def update(w2: Double) = w = w2

	def apply(word: PhoneticWord) = violations(word)
	
	def parseFeature(str: String): Feature = {
//		println("Parsing feature")
		val buffer = new ArrayBuffer[HashMap[String, Boolean]]
		wPattern findAllIn str foreach { chunk => 
			buffer += chunk.split(",").map(parseBF(_)).foldLeft(new HashMap[String, Boolean])(_+=_)
		}
		new Feature(str, 0.0, buffer.toArray)
	}
	
	def parseBF(str: String):Tuple2[String, Boolean] = {
		str match {
			case bpPattern(sign, label) => {
				return (label, sign == "+")
			}
			case default => {
				throw new Exception("String %s is not a valid format".format(str))
			}
		}
	}

	def violations(word: PhoneticWord): Int = {
		if (word.size < fconstraints.size)
			return 0
//		println(toVerboseString)
//		println("feats size = %d".format(fconstraints.size))
//		println(toString + ":  " + fconstraints.mkString(", "))
		val window = fconstraints.size
		val features = word.project
		var vcount = 0
//		try {
//		println("word size is %d: %s".format(word.size, word))
//		println("feat size is %d".format(fconstraints.size))
		var i = 0
		while (i <= (word.size - window)) {
//		for (i <- 0 to (word.size - window)) {
			var allViolate = true
			for (j <- 0 until window) {
				val pattern = fconstraints(j)
				val fill = features(i+j)
//				println("COMP: " + word.get(i+j) + " => " + fill + "  VS  " + pattern + "\t " + violates(word.get(i+j), fill, pattern))
				val violate = violates(word.get(i+j), fill, pattern) 
				if (!violate)  // || (fill.getOrElse("reverse", false) && violate))
					allViolate = false
			}
			if (allViolate) { vcount += 1 }
//			if (fconstraints(0).contains("init"))
//				return vcount
	   i += 1
		}
//		println(vcount)
		vcount
	}
/*
		println(word)
		println(word.toArray.map(_.features.mkString(", ")).mkString("\n"))
		println(vcount)
		println(toVerboseString)
		println
*/
//		}
/*
		catch {
			case error => {  
				println("Error counting violations on word %s with feature %s with function %s".format(word, toString, fconstraints.mkString(",")))
				println(error.getStackTraceString)
				System.exit(-1)
			}
		}
*/


	def violates(phone: Phone, fill: HashMap[String, Boolean], pattern: HashMap[String, Boolean]): Boolean = {
//		println(phone)
//		println(label)
//		println(phone.features.mkString(","))
//		println(fconstraints.mkString(","))
		if (pattern.contains("any"))
			return true
		var all = true
		for (key <- pattern.keys if key != "negate") {
			val matches = ((key == "all") ||
										 (key == phone.toString.trim) ||
										 (fill.contains(key) && fill(key) == pattern(key)))
			if (!matches){
				all = false
			}
		}
		if (pattern.contains("negate")) {
			return !all
		}
		else {
			return all
		}
	}
	
//			if (key == phone.toString.trim)
//				matchAll = false
//				if (fill.contains(key) && fill(key) == pattern(key)   ||   key == phone.toString.trim) {

//				println("		Found contradiction %s,%s to constraint with %s,%s".format(key, fill.getOrElse(key, "NOT FOUND"), key, pattern.getOrElse(key, "NOT FOUND")))
//				println
//				return true				
//			}
//		}
//		println("		No contradition found")
//		println
//		return false			
	

	def fires(word: PhoneticWord) = violations(word) > 0
	
	def toVerboseString = {
		val buffer = new collection.mutable.StringBuilder
		buffer.append("Feature: %s\n".format(label))
		for (i <- 0 until fconstraints.size) {
			buffer.append("  slice %d:\t%s\n".format(i, fconstraints(i).mkString(", ")))
		}
		buffer.toString
	}
	
	def negate(idx: Int): Feature = {
		val f = fconstraints.map(_.clone)
		f(idx) += "negate" -> true
		return FeatureFactory.parseFeature(f)
	}
	
	def negate(idx1: Int, idx2: Int): Feature = {
		val f = fconstraints.map(_.clone)
		f(idx1) += "negate" -> true
		f(idx2) += "negate" -> true
		return FeatureFactory.parseFeature(f)
	}
	
	override def toString: String = "%f\t%s".format(w, label)

	override def hashCode = Tuple(label, label).hashCode

	override def equals(other: Any): Boolean = other match { 
		case other: Feature => {
			label == other.label
/*
			if (constraints.size != other.constraints.size)
				return false
			for (i <- 0 until constraints.size) {
				if (constraints(i) != other.constraints(i))
					return false
			}
			return true
*/
		}
		case _ => false 
	}
}



/*
//			other.label == label 
if (other.label.size != label.size)
	return false
val aSlices = label.split("\\]\\[").map(_.replaceAll("\\[|\\]", ""))
val bSlices = label.split("\\]\\[").map(_.replaceAll("\\[|\\]", ""))
println(aSlices.mkString(";"))
println(bSlices.mkString(";"))
for (i <- 0 until aSlices.size) {
	val cells1 = aSlices(i).split(",").map(_.trim)
	val cells2 = bSlices(i).split(",").map(_.trim)
	if (cells1.size != cells2.size || cells1.filter(x => !cells2.contains(x)).size > 0)
		return false
}

*/







/*			
for (i <- 0 until word.size-window) {
var allTrue = true
for (slice <- v(i)) {
for (k <- slice.keys) {
if (word.phones(i))
}
}

for (j <- 0 until v.size) {
for (k <- 0 until v(j).size) {
val cfeat = features(i+j).filter(v(j)(k).label == _.label)
assert (cfeat.size == 0, "There should be one corresponding feature in the global set.")
if (v(j)(k).value != cfeat(0).value) {  
allTrue = false
}						
}				
}
if (allTrue) {
vcount += 1
}
}
vcount 
}
*/


// //features(i+j).filter(v(j).label)) {

*/