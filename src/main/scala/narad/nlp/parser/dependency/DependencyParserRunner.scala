/*
package narad.nlp.parse

import narad.bp.structure._
import narad.bp.train._
import narad.bp.util._
import narad.util._
import scala.collection.mutable.{ArrayBuffer, HashMap}
import narad.nlp.parse._
import scala.util.matching.Regex

object DependencyParserRunner {
	val indicesPattern = new Regex(".*\\(([0-9]+)\\,([0-9]+)\\).*")

	def main(args: Array[String]) = {
		val options  = new ArgParser(args)
		val fidxFile = options.getString("--fidx.file", "null")
		val initFile = options.getString("--init.file", "null")
		val outFile  = options.getString("--out.file", "dp.parser.model")
		val pvsize   = options.getInt("--pv.size", -1)
		val maxEx    = options.getInt("--max.examples", -1)
		val iterations = options.getInt("--iterations", 10)
		val rate     = options.getDouble("--rate", 0.3)
		val initRate = options.getDouble("--init.rate", 0.01)
		var btime = System.currentTimeMillis()
		
		if (options.getBoolean("--train", false)) {
			train(fidxFile, initFile, outFile, pvsize, iterations, maxEx)			
		}
		else if (options.getBoolean("--test", false)) {
			test(fidxFile, initFile)
		}
		println("TOTAL TIME = " + (System.currentTimeMillis() - btime))
		
	}


	def train(fidxFile: String, initFile: String, outFile: String, pvsize: Int, iterations: Int = 10, maxEx: Int = -1) = {
		var params = init(initFile, pvsize)
		val nrExamples = if (maxEx == -1) countExamples(fidxFile) else maxEx
		for (i <- 0 until iterations) {
			var nparams = sgdtrain(params, fidxFile, maxExamples = nrExamples, iteration = i)
			if (i+1 == iterations) {
				writeToFile(nparams.tail, outFile + ".pv")
			}
			else{
				writeToFile(nparams.tail, outFile + "." + (i+1) + ".pv")				
			}
			params = nparams
		}
	}

	def test(fidxFile: String, initFile: String) = {
		var params = init(initFile)
/*
		for (ex <- PotentialReader.read(fidxFile)) {
			val words  = ex.attributes("@words").split(" ")
			val nonterms = ex.attributes.getOrElse("@grammar", "").split(" ").distinct
			var btime = System.currentTimeMillis()

			val pots = ex.potentials
			val names   = pots.map(_.name)
			val correct = pots.map(_.isCorrect)
			val feats   = pots.map(_.features)
			println("timed extracted example in " +  (System.currentTimeMillis() - btime))
			btime = System.currentTimeMillis()
			
			val pscores  = computePotentials(params, feats)
			println("timed potentials in " +  (System.currentTimeMillis() - btime))
		 	btime = System.currentTimeMillis()

			val scores = pscores.map(Math.exp(_))
			println("timed constructed node arry in " +  (System.currentTimeMillis() - btime))
			btime = System.currentTimeMillis()

			for (i <- 0 until pscores.size) {
				println("O = %f;\tE = %f".format(pscores(i), scores(i)))
			}
			val model = DependencyParser.construct(names, scores, words.size)
			println("timed parser creation in " +  (System.currentTimeMillis() - btime))
			btime = System.currentTimeMillis()

			var conv = runBP(model.graph, 5, 1, 0.1)
			println("timed BP in " +  (System.currentTimeMillis() - btime))
			btime = System.currentTimeMillis()
			model.decode(words)
			println
		}
		*/
	}

	def sgdtrain(params: Array[Double], fidxFile: String, maxExamples: Int = Int.MaxValue, rate: Double = 0.1, iteration: Int = 0): Array[Double] = { //pots: Array[Potential], scores: Array[Double], maxExamples: Int = 1): Array[Double] = {
		var count = 0
		var pvv = params.clone
		val damp = 0.099
		/*
		for (ex <- PotentialReader.read(fidxFile) if count < maxExamples) {
//			val rate = -1.0 * (0.01 / (1.0 + ((count+1.0) + (iteration * maxExamples))/(maxExamples*1.0)))
			println("\nPVV:")
				for (i <- 0 until pvv.size) {
					println("[%d]\t%f".format(i, pvv(i)))
				}				
			val rate = -0.01
			val pots = ex.potentials
			val names   = pots.map(_.name)
			val correct = pots.map(_.isCorrect)
			val feats   = pots.map(_.features)
			val scores  = computePotentials(pvv, feats).map(Math.exp(_))
			println("\nSCORES:")
				for (i <- 0 until scores.size) {
					println("[%d]\t%s\t%f".format(i, names(i), scores(i)))
				}
				
				
			val words  = ex.attributes("@words").split(" ")
			val tagset = ex.attributes.getOrElse("@grammar", "").split(" ").distinct
			
			val model = DependencyParser.construct(names, scores, words.size)
			println("Model")
			println(model.toString)
			var conv = runBP(model.graph, 5, damp, 0.1)
			
//			for (e <- model.graph.edges) {
//				println()
//			}
			val beliefs = model.graph.potentialBeliefs.sortBy { b =>
					val indicesPattern(start, end) = b._1
					start.toInt * 10000 + end.toInt
			}
//			println("VELIEFS:")
//			for (b <- model.variableBeliefs) { println(b._1 + "\t" + b._2) }
//			var bcount = 0
//			for (ob <- beliefs1) { println(bcount + "\t" + ob); bcount += 1 }
//			println("---------")
//			println(beliefs1.size)
//			println(names.size)

/*
			var beliefs = beliefs1.zipWithIndex.map { case(v, idx) => 				
				Tuple(names(idx), v._2)
			}
*/
//			beliefs(beliefs.size-1) = Tuple("brack(0,16)", 1.0)

//			for (i <- 0 until beliefs.size) {
//				println("%s vs %s".format(names(i), beliefs1(i)._1))
//			}

			val margs = beliefs.map { belief =>
				val idx = names.indexOf(belief._1)
				assert(idx != -1, "Potential %s from marg function does not match any name from the potential file.")
				println("  name = " + belief._1 + "\t" + idx + "\t" + belief._2)
				if (correct(idx))
					belief._2 - 1.0
				else
					belief._2
			}
			println("\nBELIEFS")
			for (i <- 0 until beliefs.size) {
				println("%s\t%f\t%f".format(beliefs(i)._1, beliefs(i)._2, margs(i)))
			}
/*			
			println("Pots not used?")
			val n2 = beliefs.map(_._1)
			for (pot <- names.filter(!n2.contains(_))) {
				println("NOT FOUND = " + pot)
			}
*/
			pvv = margupdate(pvv, feats, margs, rate=rate)
			println("\n\nPV:")
			for (i <- 0 until pvv.size) {
				println("[%d]\t%f".format(i, pvv(i)))
			}
			count += 1
		}
		*/
		return pvv
	}	




	def margupdate(oldpv: Array[Double], feats: Array[Array[Feature]], margs: Array[Double], rate: Double = 1.0): Array[Double] = {
		println("rate = " + rate)
		var newpv = oldpv
		assert(feats.size == margs.size, "feats and marg arguments to margupdate were not identically sized (%d to %d).".format(feats.size, margs.size))
		for (i <- 0 until margs.size) {
			val grad = margs(i) * rate
			for (j <- 0 until feats(i).size) {
				val feat = feats(i)(j)
				newpv(feat.idx) += grad * feat.value
			}
		}
		println("\nMarg Update:")
		for (i <- 0 until 20) {
			//			println(newpv(i) + " : " + margs(i) + "\t" + feats(i).mkString(","))
		}		

		return newpv
	}


	def runBP(graph: FactorGraph, bpiters: Int = 5, drate: Double = 0.99, dinit: Double, threshold: Double = .001): Boolean = {
		val model = graph  // Should pass in a model and not need to do this, but tagger does not subclass FG yet
		var maxDiff = -1.0
		var damp = dinit
		val mqueue = scala.collection.mutable.Queue[MessageNode]() // queue for everything not in uqueue
		val uqueue = scala.collection.mutable.Queue[MessageNode]() // queue for unary factors, will probably rearrange and make this obsolete
		// Load the queue, could add support for user pre-loading of the queue
		for (fac <- model.factors) {
			if (model.edgesFrom(fac).size == 1) {
				uqueue += fac
			}
			else {
				mqueue += fac
			}
		}
		for (v <- model.variables) {
			mqueue += v
		}
		
//		mqueue ++= mqueue.reverse
		println("M QUEUE: ")
		for (m <- mqueue) {
			println("\t" + m.toString)
		}
		
		// Do the unary facs first with no damping
		for (u <- uqueue) {
			u.computeMessages(model, damp=1)
		}
		// Do the others with successive damping
		for (i <- 0 until bpiters) {
			println("\n\nBP ITER %d".format(i))
			for (v <- mqueue) {
				maxDiff = -1.0
				var diff = v.computeMessages(model, 1) //damp=damp)
				if (diff > maxDiff) maxDiff = diff
			}
			println(maxDiff + " vs threshold of " + threshold)
			if (i > 0 && maxDiff < threshold) {  // Converged??
				return true
			}
			if (i > 1) damp *= drate
		}
		return false			
	}

	def init(initFile: String, pvsize: Int = 0): Array[Double] = {
		assert(initFile != "null" || pvsize > 0, "Both init.file and pv.size are not specified correctly.")
		if (initFile == "null") {
			return Array.fill(pvsize+1)(0.0)
		}
		else {
			val params1 = io.Source.fromFile(initFile).getLines().map(_.toDouble).toArray
			val params = Array[Double](0.0) ++ params1
			if (pvsize > params.size) {
				return params ++ Array.fill(pvsize - (params.size+1))(0.0)
			}
			else {
				return params
			}
		}
	}

	def computePotentials(params: Array[Double], feats: Array[Array[Feature]]): Array[Double] = {
		println("Params = %d".format(params.size))
		feats.map(x => x.foldLeft(0.0)((sum, feat) => sum + params(feat.idx) * feat.value))
	}

	def writeToFile(array: Array[Double], filename: String) {
		val fw = new java.io.FileWriter(filename)
		array.foreach{ p =>
			fw.write(p + "\n")
		}
		fw.close
	}

	def countExamples(filename: String): Int = {
		print("Determining number of examples in fidx file...")
		val endPattern   = """[ \n\t]*""".r
		var count = 0
		for (line <- scala.io.Source.fromFile(filename).getLines if line == "") count += 1
		println(count + ".")
		return count
	}
}















/*


def train(fidxFile: String, initFile: String, outFile: String, pvsize: Int, iterations: Int = 10) = {
var params = init(initFile, pvsize)
val nrExamples = countExamples(fidxFile)
for (i <- 0 until iterations) {
var nparams = sgdtrain(params, fidxFile, maxExamples = nrExamples, iteration = i)
if (i+1 == iterations) {
writeToFile(nparams.tail, outFile + ".pv")
}
else{
writeToFile(nparams.tail, outFile + "." + (i+1) + ".pv")				
}
params = nparams
}
}

def test(fidxFile: String, initFile: String) = {
var params = init(initFile)
for (ex <- PotentialReader.read(fidxFile)) {
val words  = ex.attributes("@words").split(" ")
//			val tags = ex.attributes("@tagset").split(" ")
var pots = ex.potentials
val scoredpots = computePotentials(params, pots)	
val names = scoredpots.map(_._1.name)
val ppots = scoredpots.map(_._2)
val parser = DependencyParser.construct(names, ppots, words.size)
var conv = runBP(parser.graph, 5, 1, 0.1)
parser.decode(words)
println
}
}

def sgdtrain(params: Array[Double], fidxFile: String, maxExamples: Int = Int.MaxValue, rate: Double = 0.1, iteration: Int = 0): Array[Double] = { //pots: Array[Potential], scores: Array[Double], maxExamples: Int = 1): Array[Double] = {
var count = 0
var pvv = params.clone
for (ex <- PotentialReader.read(fidxFile) if count < maxExamples) {
val rate = -1.0 * (0.01 / (1.0 + ((count+1.0) + (iteration * maxExamples))/(maxExamples*1.0)))
var pots = ex.potentials
println("computing potentials")
val scoredpots = computePotentials(pvv, pots) //.map(_._2)
val words  = ex.attributes("@words").split(" ")
val corrects = pots.filter(_.isCorrect).map(_.name)
println("getting marges")
val marg = margfun(scoredpots, damp=0.099, words).map(p => if (corrects.contains(p._1)) Tuple(p._1, p._2 - 1.0) else p  )
println("marg update")
pvv = margupdate(pvv, pots, marg, rate=rate)
println("done")
count += 1
}
return pvv
}

def margfun(scoredpots: Array[(Potential, Double)], damp: Double = 0.99, words: Array[String]): Array[(String, Double)] = {
val names = scoredpots.map(_._1.name)
val ppots = scoredpots.map(_._2)
println("Constructing parser")
val model = DependencyParser.construct(names, ppots, words.size)
println(model.toString)
println("Running BP")
var conv = runBP(model.graph, 5, damp, 0.1)
println("Getting beliefs")
val beliefs = model.potentialBeliefs
return beliefs
}

def margupdate(oldpv: Array[Double], pots: Array[Potential], margs: Array[(String, Double)], rate: Double = 1.0): Array[Double] = {
var newpv = oldpv
pots.zip(margs.map(_._2)).foreach { case(pot,gradient) =>
val grad = gradient * rate
for (feat <- pot.features) {
newpv(feat.idx) += grad * feat.value
}	
}
return newpv
}

def runBP(graph: FactorGraph, bpiters: Int = 5, drate: Double = 0.99, dinit: Double, threshold: Double = 0): Boolean = {
val model = graph  // Should pass in a model and not need to do this, but tagger does not subclass FG yet
var maxDiff = -1.0
var damp = dinit
val mqueue = scala.collection.mutable.Queue[MessageNode]() // queue for everything not in uqueue
val uqueue = scala.collection.mutable.Queue[MessageNode]() // queue for unary factors, will probably rearrange and make this obsolete
// Load the queue, could add support for user pre-loading of the queue
for (fac <- model.factors) {
if (model.edgesFrom(fac).size == 1) {
uqueue += fac
}
else {
mqueue += fac
}
}
for (v <- model.variables) {
mqueue += v
}
// Do the unary facs first with no damping
for (u <- uqueue) {
u.computeMessages(model, damp=1)
}
// Do the others with successive damping
for (i <- 0 until bpiters) {
for (v <- mqueue) {
maxDiff = -1.0
var diff = v.computeMessages(model, damp=damp)
if (diff > maxDiff) maxDiff = diff
}
if (i > 0 && maxDiff < threshold) {  // Converged??
return true
}
if (i > 1) damp *= drate
}
return false			
}

def init(initFile: String, pvsize: Int = 0): Array[Double] = {
assert(initFile != "null" || pvsize > 0, "Both init.file and pv.size are not specified correctly.")
if (initFile == "null") {
return Array.fill(pvsize+1)(0.0)
}
else {
val params1 = io.Source.fromFile(initFile).getLines().map(_.toDouble).toArray
val params = Array[Double](0.0) ++ params1
if (pvsize > params.size) {
return params ++ Array.fill(pvsize - (params.size+1))(0.0)
}
else {
return params
}
}
}

def computePotentials(params: Array[Double], pots: Array[Potential]): Array[(Potential, Double)] = {
var correctScore = 0.0
val scores = new ArrayBuffer[(Potential, Double)]
for (pot <- pots) {
var score = 0.0
for (feat <- pot.features) {
score += params(feat.idx) * feat.value
}
if (pot.isCorrect) correctScore += score
scores += Tuple(pot, score)
}
scores.toArray
}

def writeToFile(array: Array[Double], filename: String) {
val fw = new java.io.FileWriter(filename)
array.foreach{ p =>
fw.write(p + "")
fw.write("\n")
}
fw.close
}

def countExamples(filename: String): Int = {
print("Determining number of examples in fidx file...")
val endPattern   = """[ \n\t]*""".r
var count = 0
for (line <- scala.io.Source.fromFile(filename).getLines if line == "") count += 1
println(count + ".")
return count
}
}	
*/
*/