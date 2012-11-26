/*

package narad.nlp.srl
import narad.util.ArgParser
import narad.bp.structure._
import narad.bp.train._
import narad.bp.util._
import scala.collection.mutable.ArrayBuffer

object SRLRunner {

	def main(args: Array[String]) = {
		val options   = new ArgParser(args)
		val fidxFile   = options.getString("--fidx.file", "null")
		val initFile   = options.getString("--init.file", "null")
		val outFile    = options.getString("--out.file", "srl.model")
		val labelFile  = options.getString("--label.file", "srl.args")
		val pvsize     = options.getInt("--pv.size", -1)
		val maxEx      = options.getInt("--max.examples", -1)
		val iterations = options.getInt("--iterations", 10)
		val bpIters 	 = options.getInt("--bp.iters", 10)
		val rate       = options.getDouble("--rate", 0.3)
		val initRate   = options.getDouble("--init.rate", 0.01)
		val variance   = options.getDouble("--var", 1.0)
		val verbose 	 = options.getBoolean("--verbose", false)
		val maxdist = 1000 //readLines(options.getString("--dist.file", "srl.dist"))(0).toInt
		
		val labels = if (labelFile == null) Array[String]() 
  		else io.Source.fromFile(labelFile).getLines.toArray.map(_.trim)

		if (options.getBoolean("--train", false)) {
			var btime = System.currentTimeMillis()
			train(fidxFile, initFile, outFile, pvsize, iterations, bpIters, maxEx, maxDist=maxdist, rate=rate, variance=variance, verbose)		
			println("TOTAL TIME = " + (System.currentTimeMillis() - btime))	
		}
		else if (options.getBoolean("--test", false)) {
			test(fidxFile, initFile, bpIters, maxDist=maxdist, verbose)
		}
		val i = 0
	}

	def train(fidxFile: String, initFile: String, outFile: String, pvsize: Int, iterations: Int = 10,
						bpIters: Int = 10, maxEx: Int = -1, maxDist: Int, rate: Double=0.1, variance: Double=1.0, verbose: Boolean = false) = {
		var params = init(initFile, pvsize)
		val nrExamples = if (maxEx == -1) countExamples(fidxFile) else maxEx
		for (i <- 0 until iterations) {
			var nparams = SGDTrainer.train(params, fidxFile, construct, maxExamples = nrExamples, bpIters = 10, rate=rate, variance=variance, verbose = verbose)
			if (i+1 == iterations) {
				writeToFile(nparams.tail, outFile + ".pv")
			}
			else{
				writeToFile(nparams.tail, outFile + "." + (i+1) + ".pv")				
			}
			params = nparams
		}
	}


	def test(fidxFile: String, initFile: String, bpIters: Int = 10, maxDist: Int, verbose: Boolean = false) = {
		var params = init(initFile)
		for (ex <- PotentialReader.read(fidxFile)) {
			val feats   = ex.getFeatures				
			val pots    = ex.getPotentials
			pots.foreach { pot => pot.value = feats(pot.name).foldLeft(0.0)((sum, feat) => sum + params(feat.idx) * feat.value)}
			pots.foreach { pot => pot.value = Math.exp(pot.value) }
			val model = construct(ex, pots)
			
			SGDTrainer.runBP(model, bpIters, dinit = 1, threshold = 0.1, verbose = verbose)
			val words  = ex.attributes("@words").split(" ")
			val tags  = ex.attributes("@tags").split(" ")
			val attrs = Array("@slen", "@words", "@tags", "@lemmas", "@roles", "@gpreds")
			for (attr <- attrs) {
				println(attr + "\t" + ex.attributes(attr))
			}
			model.decode(words, tags)
		}
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

	def construct(ex: PotentialExample, pots: Array[Potential]): SRLModel = {
		val words  = ex.attributes("@words").split(" ")
		val slen   = words.size
		SRLModel.construct(pots, slen)
	}
	
	def countExamples(filename: String): Int = {
//		print("Determining number of examples in fidx file...")
		val endPattern   = """[ \n\t]*""".r
		var count = 0
		for (line <- scala.io.Source.fromFile(filename).getLines if line == "") count += 1
		println(count + ".")
		return count
	}
	
	def writeToFile(array: Array[Double], filename: String) {
		val fw = new java.io.FileWriter(filename)
		array.foreach{ p =>
			fw.write(p + "\n")
		}
		fw.close
	}
}



























/*	
			val words  = ex.attributes("@words").split(" ")
			val pots = ex.potentials
			val names   = pots.map(_.name)
			val feats   = pots.map(_.features)
			val scores  = computePotentials(params, feats).map(Math.exp(_))

			val model = SRLModel.construct(names, scores, labels, words.size)
			println("GRAPH:")
			println(model.graph.toString())
			var conv = runBP(model.graph, 5, 1, 0.1)
			model.decode(words)
			println
		}
	}
*/


/*
	def sgdtrain(params: Array[Double], fidxFile: String, maxExamples: Int = Int.MaxValue, rate: Double = 0.1, iteration: Int = 0, labels: Array[String]): Array[Double] = { //pots: Array[Potential], scores: Array[Double], maxExamples: Int = 1): Array[Double] = {
		var count = 0
		var pvv = params.clone
		val damp = 0.099
		for (ex <- PotentialReader.read(fidxFile) if count < maxExamples) {
			//			val rate = -1.0 * (0.01 / (1.0 + ((count+1.0) + (iteration * maxExamples))/(maxExamples*1.0)))
			val pots = ex.potentials
			val names   = pots.map(_.name)
			val correct = pots.map(_.isCorrect)
			val feats   = pots.map(_.features)
			val scores  = computePotentials(pvv, feats).map(Math.exp(_))

			val words  = ex.attributes("@words").split(" ")
			val model = SRLModel.construct(names, scores, labels, words.size)
			println("GRAPH:")
			println(model.graph.toString())
			var conv = runBP(model.graph, 5, damp, 0.1)
			val beliefs = model.graph.potentialBeliefs

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

			pvv = margupdate(pvv, feats, margs, rate=0.03 * -1.0) // note the explicit rate here
			println("PVV:")
			pvv.zipWithIndex.foreach(println(_))
			println

			count += 1
		}
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

		println("U QUEUE: ")
		for (m <- uqueue) {
			println("\t" + m.toString)
		}
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



	def computePotentials(params: Array[Double], feats: Array[Array[Feature]]): Array[Double] = {
		println("Params = %d".format(params.size))
		feats.map(x => x.foldLeft(0.0)((sum, feat) => sum + params(feat.idx) * feat.value))
	}




}
*/


















//			var pots = ex.potentials
//			println("computing potentials")
//			val corrects = pots.filter(_.isCorrect).map(_.name)
//			corrects.foreach(c => println("CORRECT: %s".format(c)))
//			println("Features:")
//			pots.map{p => p.features.foreach{f => print(f)}; println}
//			println("getting marges")
//			val scoredpots = computePotentials(pvv, pots) //.map(_._2)
//			val marg = margfun(scoredpots, damp=0.099, words).map(p => if (corrects.contains(p._1)) Tuple(p._1, p._2 - 1.0) else p  )
//			println("marg update")
//			pvv = margupdate(pvv, pots, marg, rate=rate)
//			println("done")
//			count += 1
//		}
//		return pvv
//	}

/*
def margfun(scoredpots: Array[(Potential, Double)], damp: Double = 0.099, words: Array[String]): Array[(String, Double)] = {
val names = scoredpots.map(_._1.name)
val ppots = scoredpots.map(_._2)
println("Constructing parser")
val model = SRLModel.construct(names, ppots, words.size)
println(model.toString)
println("Running BP")
var conv = runBP(model.graph, 5, damp, 0.1)
println("Getting beliefs")
val beliefs = model.potentialBeliefs
return beliefs
}
*/

/*		
feats.zip(margs).foreach { case(farray, gradient) =>
val grad = gradient * rate
farray.foreach {f => newpv(f.idx) += grad * f.value}
}
*//*		
pots.zip(margs).foreach { case(pot,gradient) =>
val grad = gradient * rate
for (feat <- pot.features) {
newpv(feat.idx) += grad * feat.value
}	
}
return newpv
}
*/




/*		
var correctScore = 0.0
val scores = new ArrayBuffer[(Potential, Double)]
for (pot <- pots) {
var score = 0.0
for (feat <- pot.features) {
score += params(feat.idx) * feat.value
}
if (pot.isCorrect) correctScore += score
scores += score
}
scores.toArray
}
*/









/*
object SRLRunner {

def main(args: Array[String]) = {
val options  = new ArgParser(args)
val fidxFile = options.getString("--fidx.file", "null")
val initFile = options.getString("--init.file", "null")
val outFile  = options.getString("--out.file", "srl.model.out")
val pvsize   = options.getInt("--pv.size", -1)
val iterations = options.getInt("--iterations", 10)
val rate     = options.getDouble("--rate", 0.3)
val initRate = options.getDouble("--init.rate", 0.01)
println(pvsize)
if (options.getBoolean("--train", false)) {
train(fidxFile, initFile, outFile, pvsize, iterations)			
}
else if (options.getBoolean("--test", false)) {
test(fidxFile, initFile)
}
}

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
var pots = ex.potentials
val scoredpots = computePotentials(params, pots)	
val model = SRLModel.construct(scoredpots.map(p => Tuple(p._1, Math.exp(p._2))), words.size)
val conv = runBP(model.graph, 5, 1, 0.1)
model.decode(words)
println
}
}

def sgdtrain(params: Array[Double], fidxFile: String, maxExamples: Int = Int.MaxValue, rate: Double = 0.1, iteration: Int = 0): Array[Double] = { //pots: Array[Potential], scores: Array[Double], maxExamples: Int = 1): Array[Double] = {
var count = 0
var pvv = params.clone
for (ex <- PotentialReader.read(fidxFile) if count < maxExamples) {
val rate = -1.0 * (0.01 / (1.0 + ((count+1.0) + (iteration * maxExamples))/(maxExamples*1.0)))
var pots = ex.potentials
val scoredpots = computePotentials(pvv, pots) //.map(_._2)
val words  = ex.attributes("@words").split(" ")
val marg = margfun(scoredpots, damp=0.099, words).map(p => if (p._1.isCorrect) Tuple(p._1, p._2 - 1.0) else p  )
pvv = margupdate(pvv, marg, rate=rate)
count += 1
}
return pvv
}

def margfun(scoredpots: Array[(Potential, Double)], damp: Double = 0.99, words: Array[String]): Array[(Potential, Double)] = {
val model = SRLModel.construct(scoredpots.map(p => Tuple(p._1, Math.exp(p._2))), words.size)
var conv = runBP(model.graph, 5, damp, 0.1)
val beliefs = model.potentialBeliefs
return beliefs
}

def margupdate(oldpv: Array[Double], scoredpots: Array[(Potential, Double)], rate: Double = 1.0): Array[Double] = {
var newpv = oldpv
scoredpots.foreach { case(pot,gradient) =>
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