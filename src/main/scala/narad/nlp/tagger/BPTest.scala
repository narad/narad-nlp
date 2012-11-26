package narad.projects.bpdp

import narad.bp.structure._
//import narad.bp.train._
import narad.bp.util._
import narad.util._
import scala.collection.mutable.{ArrayBuffer, HashMap}

/*

object BPTest {

	def main(args: Array[String]) = {
		if (args.contains("--testYSNP")) {
			testYSNP()
		}
	}

  def testYSNP() = {
		val pots = Array[Potential](new Potential(-0.5, "pred(5)", true),
										 						new Potential(0.78, "argOf(2,5)", true))
		pots.foreach(p => p.value = Math.exp(p.value))
		
		val fg = new FactorGraphBuilder(pots)
/*		
		fg.addVariable("predVar(%d)".format(5), 2)
		fg.addUnaryFactor2("predVar(%d)".format(5), "predFac(%d)".format(5), Array(pots(0)))											
		
		fg.addVariable("argVar(%d,%d)".format(2, 5), 2)
		fg.addUnaryFactor2("argVar(%d,%d)".format(2, 5), "argFac(%d,%d)".format(2, 5), Array(pots(1)))																
		
		fg.addYouShallNotPassFactor("predVar\\(%d\\)".format(5), "argVar\\(%d,%d\\)".format(2, 5), "YSNP(%d,%d)".format(2, 5))			
*/
		val graph = fg.toFactorGraph
		println("GRAPH:\n%s".format(graph.toString))

		SGDTrainer.runBP(graph, 2, 0.01, 0.1)
		
		println("\nBELIEFS:")
		for (b <- graph.potentialBeliefs) {
			println(b)
		}
	}
}

*/


/*
	def main(args: Array[String]) = {
		val options  = new ArgParser(args)
		val fidxFile = options.getString("--fidx.file", "null")
		val initFile = options.getString("--init.file", "null")
		val outFile  = options.getString("--out.file", "smodel.out")
		val pvsize   = options.getInt("--pv.size", -1)
		val iterations = options.getInt("--iterations", 10)
		val rate     = options.getDouble("--rate", 0.3)
		val initRate = options.getDouble("--init.rate", 0.01)
		println(pvsize)
		if (options.getBoolean("--train", false)) {
			train(fidxFile, initFile, outFile, pvsize, iterations)			
		}
		else if (options.getBoolean("--tag", false)) {
			test(fidxFile, initFile)
		}
	}
	

	def train(fidxFile: String, initFile: String, outFile: String, pvsize: Int, iterations: Int = 10) = {
		var params = init(initFile, pvsize)
		val nrExamples = countExamples(fidxFile)
		for (i <- 0 until iterations) {
//			println("BEFORE TRAIN: " + params.mkString(", "))
			var nparams = sgdtrain(params, fidxFile, maxExamples = nrExamples, iteration = i)
//			println("BACK AT TRAIN: " + nparams.mkString(", "))
			writeToFile(nparams.tail, outFile + "." + (i+1) + ".pv")
			params = nparams
		}
	}
	
	def test(fidxFile: String, initFile: String) = {
		var params = init(initFile)
		for (ex <- PotentialReader.read(fidxFile)) {
			val words  = ex.attributes("@words").split(" ")
			val tags = ex.attributes("@tagset").split(" ")
			var pots = ex.potentials
			val scoredpots = computePotentials(params, pots)	
			val tagger = new LinearChainTagger(scoredpots.map(p => Tuple(p._1, Math.exp(p._2))), words, tags)
			var conv = runBP(tagger, 5, 1, 0.1)
			tagger.decode
			println
		}
	}

	// Like facidx.sgd.pass
	def sgdtrain(params: Array[Double], fidxFile: String, maxExamples: Int = Int.MaxValue, rate: Double = 0.1, iteration: Int = 0): Array[Double] = { //pots: Array[Potential], scores: Array[Double], maxExamples: Int = 1): Array[Double] = {
		var count = 0
		var pvv = params.clone // Array.fill(params.size)(0.0)
		for (ex <- PotentialReader.read(fidxFile) if count < maxExamples) {
			val rate = -1.0 * (0.01 / (1.0 + ((count+1.0) + (iteration * maxExamples))/(maxExamples*1.0)))
//			println("RATE = " + rate)
			var pots = ex.potentials
//			println("POTS = " + pots.size)
//			pots.zipWithIndex.foreach { case(pot,i) => println("%d  %s".format(i, pot)) }

			//marg
			val scoredpots = computePotentials(pvv, pots) //.map(_._2)
//			println("SCORES = " + scores.mkString(", "))
			val words  = ex.attributes("@words").split(" ")
			val tags = ex.attributes("@tagset").split(" ")
			val marg = margfun(scoredpots, damp=0.099, words, tags).map(p => if (p._1.isCorrect) Tuple(p._1, p._2 - 1.0) else p  )
//			println("MARG (%d):".format(marg.size))
//			marg.zipWithIndex.foreach { case(pot,i) => println("%d:%f  %s".format(i, pot._2, pot._1.name)) } 

			// update
			pvv = margupdate(pvv, marg, rate=rate)
//			println("NEW PARAMS (%d): [%s] ".format(pvv.size, pvv.mkString(", ")))
			count += 1
		}
		return pvv
	}

	def margfun(scoredpots: Array[(Potential, Double)], damp: Double = 0.99, words: Array[String], tags: Array[String]): Array[(Potential, Double)] = {
		val tagger = new LinearChainTagger(scoredpots.map(p => Tuple(p._1, Math.exp(p._2))), words, tags)
		//, scores.map(Math.exp(_)), words, tags)
		var conv = runBP(tagger, 5, damp, 0.1)
		println("converged? %s".format(conv))
		val beliefs = tagger.potentialBeliefs
//		for (belief <- beliefs) println("%.4f\t%s".format(belief._2, belief._1.name))
		return beliefs
	}

	def margupdate(oldpv: Array[Double], scoredpots: Array[(Potential, Double)], rate: Double = 1.0): Array[Double] = {
		// Add the var > 0 code
//		println("pv size = " + oldpv.size)
//		println("pots size = " + pots.size)
//		println("gradients size = " + gradients.size)

		var newpv = oldpv
//		println("new pv r1 = " + newpv.mkString(", "))
		scoredpots.foreach { case(pot,gradient) =>
//			println(pot.name)
			val grad = gradient * rate
			for (feat <- pot.features) {
				println("...[%d]  +=  %f".format(feat.idx, grad * feat.value))
				newpv(feat.idx) += grad * feat.value
			}	
		}
//		println("new pv r2 = " + newpv.mkString(", "))
		return newpv
	}



	// Returns whether or not converged
	def runBP(tagger: LinearChainTagger, bpiters: Int = 5, drate: Double = 0.99, dinit: Double, threshold: Double = 0): Boolean = {
		val model = tagger.graph  // Should pass in a model and not need to do this, but tagger does not subclass FG yet
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



	//	def margupdate

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
*/














































//		while ({ex = PotentialReader.read(fidxFile)} && count < maxExamples) {  // need to implement sequential reading from the facidx file
	//		while ({ex <- readExample(fidxFile)} && count < maxExamples) {
		//			val exes = PotentialReader.read(fidxFile).toList
		//			val ex = exes(0)
		//			println("examples found = " + exes.size)

		/*
		def margupdate(marg: Array[(Potential, Double)], scale: Double = -0.005, vvar: Double = 1) =  {
		indices.zipWithIndex.map { case(idx,i) =>
		var grad = pgrad(i)
		if (grad == 0) {

}
else {

}
grad = grad * scale
idx.
}

if (var > 0) {
var reg = 1 - (-1.0 * scale / var)
for (i <- 0 until nres) {
res(i) = res(i) * reg
}
}
val pcount = indices.size
for (i <- 0 until pcount) {
var grad = pgrad(i)
if (grad != 0.0) {
grad = grad * scale
for (j <- 0 until idx.size) {
res(idx(j)) 
}
}
}
}
*/

/*

void facidx::propagate_updates(const double* pgrad,
double scale, double var,
int nres, double* res) const {
if ( var > 0 ) {
double reg = 1 - (-scale / var);	// remove NEGATIVE when doing it right
for ( int i = 0; i < nres; ++i ) {
res[i] *= reg;
}
}
int pcount = indices_.size();
for ( int i = 0; i < pcount; ++i ) {
double grad = pgrad[i];
if ( grad == 0.0 ) continue;
grad *= scale;
const vector<int>& idx = indices_[i];
const vector<double>& val = values_[i];
int fcount = idx.size();
for ( int f = 0; f < fcount; ++f ) res[idx[f]] += grad * val[f];
}
}
*/





/*

maxDiff = -1;
for ( std::vector<Vertex>::const_iterator nodep = curq.begin(); nodep != curq.end(); ++nodep ) {
double diff = FG_[*nodep].node->compute_messages(*nodep, FG_, damp);
if ( diff > maxDiff ) maxDiff = diff;
}

int FactorGraph::run_bp(int maxit, double damp_rate, double damp_init,
double threshold) {
int iteration;
double maxDiff = -1;
bool converged = false;

vector<Vertex> curq;
vector<Vertex> unaryq;

for ( std::vector<Vertex>::const_iterator fac = factors_.begin();
fac != factors_.end(); ++fac ) {
if ( !enqueued_.count(*fac) ) {
if ( out_degree(*fac, FG_) == 1 ) {
unaryq.push_back(*fac);
} else {
curq.push_back(*fac);
}
}
}
for ( std::vector<Vertex>::const_iterator var = variables_.begin();
var != variables_.end(); ++var ) {
if ( !enqueued_.count(*var) ) curq.push_back(*var);
}
curq.insert(curq.end(), queue_.begin(), queue_.end());

for ( std::vector<Vertex>::const_iterator nodep = unaryq.begin(); nodep != unaryq.end(); ++nodep ) {
double diff = FG_[*nodep].node->compute_messages(*nodep, FG_, 1);
}

double damp = damp_init;
for ( iteration = 0; iteration < maxit; ++iteration ) {
maxDiff = -1;
for ( std::vector<Vertex>::const_iterator nodep = curq.begin(); nodep != curq.end(); ++nodep ) {
double diff = FG_[*nodep].node->compute_messages(*nodep, FG_, damp);
if ( diff > maxDiff ) maxDiff = diff;
}
// cerr << iteration << ": " << maxDiff << endl;
if ( iteration > 0 && maxDiff < threshold ) {
converged = true;
break;
}
if ( iteration > 1 ) damp *= damp_rate;
}
return converged ? iteration : -1;
}
*/	




/*
def readExample(fidxFile: String): Array[Potential] = {
val pots = new ArrayBuffer[Potential]
for (line <- io.Source.fromFile(fidxFile).getLines()) {
line match {
case attrPattern(name, value) => {
println("attr!")
//					attr(name) = value
}
case potsPattern(name, label, feats) => {
val pfeats =feats.split(" ").filter(_ != "0").map { feat =>
val countPattern(idx, value) = feat
val dval = if (value.size > 0) value.toDouble else 1.0
new Feature(idx.toInt, dval)
}
pots += new Potential(name, label == "+", pfeats)
}
case endPattern => {
println(pots.size)
return pots.toArray
//					pots.clear
}
}
}		
return null.asInstanceOf[Array[Potential]]
}
*/



/*	

def computePotentials(params: Array[Double], indices: Array[Int], 
values: Array[Array[Double]], fnames: Array[String]): Double = {
var correctScore = 0.0
for (i <- indices.size) {
var score = 0.0
for (j <- indices(i).size) {
assert(indices(i)(j) >= params.size, "Out-of-range parameter %d > %d".format(indices(i)(j), params.size))
score += params() * 
}
}
//		for ()
return 1.0
}
}

def parsePotentials(str: String): Array[Potential] = {
str.split("\n").map(Potential.parsePotential(_))
}
}

void facidx::compute_potentials(const double* params) {
correct_score_ = 0;
fvals_.clear();
potentials_.clear();
int pcount = indices_.size();
for ( int i = 0; i < pcount; ++i ) {
double score = 0;
const vector<int>& idx = indices_[i];
const vector<double>& val = values_[i];
int fcount = idx.size();
for ( int f = 0; f < fcount; ++f ) {
int cur = idx[f];
if ( cur >= nparams_ ) error("Out-of-range parameter %d > %d", cur, nparams_);
score += params[cur] * val[f];
}
fvals_.push_back(score);
potentials_[fnames_[i]] = score;
if ( correct_[i] ) correct_score_ += score;
}
}



fnames += name
values += pots.split(" ").filter(_ != "0").map { f =>
val countPattern(idx,count) = f
if (count.size > 0) count.toInt else 1
}
indices += pots.split(" ").fi

*/



//					return new Potential(name, label == "+", pots.split(" ").map(_.toDouble))










/*		
for (pots <- ChunkReader.read[Array[Potential]](fidxFile, parsePotentials(_))) {
if (first) {
//				println(pots)
first = false
for (line <- pots.split("\n")) {

}
}
}
*/



//		val vars  = new ArrayBuffer[Variable[Boolean]]
//		val facs = new ArrayBuffer[Factor]

//		val alist = new ArrayBuffer[(Factor, Array[Variable[Boolean]])]
/*		
val pots = new UnaryPotential(Array(0.1, 0.2))
val var1 = new Variable(0, "cloudy", 		Array(true, false))
val var2 = new Variable(1, "sprinkler", Array(true, false))
val var3 = new Variable(2, "rain", 			Array(true, false))
val var4 = new Variable(3, "wet glass", Array(true, false))
val fac1 = new Factor(4, "fac1", pots)
val fac2 = new Factor(5, "fac2", pots)
val fac3 = new Factor(6, "fac3", pots)
*/		

/*
alist += Tuple(new Factor(3, "fac1"), Array(var1, var2))
alist += Tuple(new Factor(4, "fac2"), Array(var2, var3))
alist += Tuple(new Factor(5, "fac3"), Array(var1, var3))
*/		
/*		
val adjList = Map[MessageNode, Seq[MessageNode]](
(fac1, Seq(var1, var2))
)
//		adjList(fac1) = Seq(var1, var2)
//		adjList(fac2) = Seq(var2, var3)

val fg = FactorGraph.fromAdjacencyList(adjList)    //new FactorGraph
println(fg)
}
}



object Potential {
val potsPattern = """([^\t]+)\t(\+?)\t([0-9 ]+)""".r

def parsePotential(str: String): Potential = {
println("line: %s".format(str))
str match {
case potsPattern(name, label, pots) => return new Potential(name, label == "+", pots.split(" ").map(_.toDouble))
case _ => return None.asInstanceOf[Potential]			
}
}
}

*/