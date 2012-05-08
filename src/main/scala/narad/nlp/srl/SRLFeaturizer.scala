package narad.nlp.srl
import java.io._
import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet}
import narad.util.ArgParser

object SRLFeaturizer {
	val dummyToken = SRLToken("ROOT", "ROOT_LEMMA", "ROOT_POS", "ROOT_CPOS")

	// r = roles, s = syntax, c = connect, capitals mean add "+" to correct examples
	def featurize(datum: SRLDatum, options: narad.util.ArgParser, i: Int) = {
		val prune  = options.getBoolean("--prune", true)
		val feats  = options.getString("--mode", "r11s1c1")
		val lfeats = feats.toLowerCase
		val srl     = feats.contains("r") || feats.contains("R")
		val syntax  = feats.contains("s") || feats.contains("S")
		val connect = feats.contains("c") || feats.contains("C")
		val labelSRL = feats.contains("R")
		val labelSyntax = feats.contains("S")
		val labelConnect = feats.contains("C")
		val srlmode     = if (srl)     lfeats.substring(lfeats.indexOf("r")+1, lfeats.indexOf("r")+2).toInt else 1
		val sensemode   = if (srl)     lfeats.substring(lfeats.indexOf("r")+2, lfeats.indexOf("r")+3).toInt else 1
		val syntaxmode  = if (syntax)  lfeats.substring(lfeats.indexOf("s")+1, lfeats.indexOf("s")+2).toInt else 1
		val connectmode = if (connect) lfeats.substring(lfeats.indexOf("c")+1, lfeats.indexOf("c")+2).toInt else 1

		val roles = readLines(options.getString("--arg.file", "srl.args"))
		val maxdist = readLines(options.getString("--dist.file", "srl.dist"))(0).toInt
		val maxsuffix = readLines(options.getString("--suffix.file", "srl.suffixes"))(0)
		val senseFile = new File(options.getString("--sense.file", "srl.senses"))
		val senseDict = new HashMap[String, Array[String]]
		for (line <- io.Source.fromFile(senseFile).getLines()) {val cols = line.split("\t"); senseDict(cols(0)) = cols(1).split(" ")}

//		val argFile   = new File(options.getString("--arg.file", "srl.args"))
//		val distFile  = new File(options.getString("--dist.file", "srl.dist"))
//		val suffixFile = new File(options.getString("--suffix.file", "srl.suffixes"))
//		val roles   = io.Source.fromFile(argFile).getLines.toArray
//		val distlines = io.Source.fromFile(distFile).getLines().toArray
//		val suffixes = io.Source.fromFile(suffixFile).getLines().toArray

//		argFile.close
//		senseFile.close
//		distFile.close
//		suffixFile.close	
//		val maxdist = distlines(0).toInt
//		val maxsuffix = suffixes(0)

		val skipSyntax    = options.getBoolean("--skip.syntax", false)
		val predictSenses = options.getBoolean("--predict.senses", true)

		val tokens = Array(dummyToken) ++ datum.tokens 
		val validDatum = datum.predicates.size > 0

		val slen = datum.slen
		println("@example\t%d".format(i))
		println("@slen\t%d".format(slen))
		println("@words\t%s".format(tokens.tail.map(_.word).mkString(" ")))
		println("@lemma\t%s".format(datum.lemmas.mkString(" ")))
		println("@tags\t%s".format(tokens.tail.map(_.pos).mkString(" ")))
		println("@roles\t%s".format(roles.mkString(" ") + " A-DUMMY"))
		println("@gpreds\t0 %s".format(datum.predicates.mkString(" "))) 
		println("@maxdist\t%s".format(maxdist))
		println("DUMMY\tXXXXX")

		if (srl && validDatum) 
			extractSRLFeatures(datum, roles, senseDict, labelSRL, prune, maxdist, maxsuffix, srlmode)
		if (syntax && validDatum) 
			extractSyntacticFeatures(datum, labelSyntax, syntaxmode)
		if (connect && validDatum) 
			extractConnectionFeatures(datum, labelConnect, datum.predicates.toArray, maxdist)
		println
	}

	def extractSRLFeatures(datum: SRLDatum, roles: Array[String], 
		senseDict: HashMap[String, Array[String]], labelCorrect: Boolean, prune: Boolean = true, maxdist: Int, maxsuffix: String = "01", srlmode: Int = 1, sensemode: Int = 1, argfeats: Boolean = false) = {
		val slen = datum.slen
		val tokens = Array(dummyToken) ++ datum.tokens 
		for (i <- 1 to slen if datum.hasPred(i)) {

			// Predicate Features
//			println("pred(%d)\t+pred-%S".format(i, datum.words(i-1)))
			val senseFeats = SRLFeatures.senseFeatures(i, tokens, sensemode)
			val senses = senseDict.getOrElse(datum.lemmas(i-1), Array[String](datum.lemmas(i-1) + "." + maxsuffix))
			var senseCount = 0
			if (senseDict.contains(datum.lemmas(i-1))) {
				for (sense <- senses) {
					val senseLabel = if (datum.hasSense(i, sense) && labelCorrect) "+" else ""
					println("sense(%d,%d)\t%spred-%s".format(i, senseCount, senseLabel, senseFeats.map("%s-%s".format(sense, _)).mkString(" ")))						
					senseCount += 1
				}						
			}
			else {
				val sense = senses(0)
				val senseLabel = if (labelCorrect) "+" else ""
				println("sense(%d,0)\t%spred-%s".format(i, senseLabel, senseFeats.map("%s-%s".format(sense, _)).mkString(" ")))												
			}

			// Argument Features
			val abound = if (prune) maxdist else slen
			for (j <- 1 to slen if Math.abs(i-j) <= abound) {
				val afeatures = SRLFeatures.argumentFeatures(j, i, tokens, srlmode)
				val alabel = if (datum.hasArg(i, j) && labelCorrect) "+" else ""
				println("hasArg(%d,%d)\t%s%s".format(i, j, alabel, afeatures.mkString(" ")))
				var found = false
				for (k <- 0 until roles.size) {
					if (datum.hasArgLabel(i, j, roles(k)) && labelCorrect) {
						println("hasLabel(%d,%d,%d)\t+%s".format(i, j, k, afeatures.map("%s-%s".format(roles(k), _)).mkString(" ")))												
						found = true
					}
					else {
						println("hasLabel(%d,%d,%d)\t%s".format(i, j, k, afeatures.map("%s-%s".format(roles(k), _)).mkString(" ")))												
					}
				}
				// Add in a dummy label in case we have pruned away this arg's real label
				if (!found && alabel == "+" && labelCorrect) {
					println("hasLabel(%d,%d,%d)\t+A-DUMMY-F".format(i, j, roles.size))											
				}
				else {
					println("hasLabel(%d,%d,%d)\tA-DUMMY-F".format(i, j, roles.size))											
				}
			}
		}
		if (argfeats) {
			for (i <- 1 to slen) {
				var isArg = false
				for (j <- 1 to slen) {
					if (datum.hasArg(j,i)) isArg = true
				}
				val isArgLabel = if (isArg && labelCorrect) "+" else ""
				println("isArg(%d)\t%s%s".format(i, isArgLabel, SRLFeatures.argFeatures(i, tokens).mkString(" ")))															
			}
		}
	}



		def extractConnectionFeatures(datum: SRLDatum, labelHidden: Boolean = true, gpreds: Array[Int], abound: Int) = {
			val slen = datum.slen
			val words = datum.forms
			val tags  = datum.postags
			val tokens = Array(dummyToken) ++ datum.tokens 
			val heads = Array(-1) ++ datum.heads //Array(-1) ++ lines.map(_.split("\t")(8).toInt)
			for (i <- 0 to slen; j <- 1 to slen if i != j && datum.hasPred(i) && Math.abs(i-j) <= abound) {
				val label = if (datum.hasArg(i, j) && heads(j) == i && labelHidden) "+" else ""
				println("sslink(%d,%d)\t%s%s".format(i, j, label, SRLFeatures.connectFeatures(tokens, i, j).mkString(" ")))
			}
		}

		def extractSyntacticFeatures(datum: SRLDatum, labelHidden: Boolean = true, mode: Int = 1) = {
			val slen = datum.slen
			val tokens = Array(dummyToken) ++ datum.tokens 
			val heads = Array(-1) ++ datum.heads
			val skip = mode == 0
			for (i <- 0 to slen; j <- 1 to slen if i != j) {
				val label = if (heads(j) == i && labelHidden) "+" else ""
				if (skip) {
					println("un(%d,%d)\t%s%s".format(i, j, label, "X_DUMMY"))
				}
				else {
					val feats = SRLFeatures.morphDependency(tokens, i, j).mkString(" ")
					println("un(%d,%d)\t%s%s".format(i, j, label, feats))
				}
			}
		}
		
		def readLines(filename: String): Array[String] = {
			val lines = new ArrayBuffer[String]
			var src = io.Source.fromFile(new File(filename))
			try {
			  src.getLines.foreach(l => lines += l)
			}
			finally src match { case b: scala.io.BufferedSource => b.close }
			return lines.toArray
		}

		def main(args: Array[String]) = {
			val options = new ArgParser(args)
			val filename  = options.getString("--srl.file")
			val format    = options.getString("--format", "CoNLL09")
			val verbose   = options.getBoolean("--verbose", false)
			val printInterval  = options.getInt("--print.interval", 50)
			//		val filterExamples = options.getBoolean("--filter.examples", false)
			var i = 1
			val startTime = System.nanoTime
			for (datum <- SRLReader.iterator(options)) {
				if (i % printInterval == 0) System.err.println("  example %d...".format(i))
				try {
					featurize(datum, options, i)				
				}
				catch {
					case e: Exception => {
						System.err.println("Error trying to featurize sentence %d:\n  %s".format(i, datum.forms.mkString(" ")))
						System.err.println(e.getStackTrace.mkString("\n"))
						System.exit(1)
					}
				}
				i += 1
			}
			val elapsed = (System.nanoTime - startTime)/1000000000.0
			System.err.println("Elapsed time: " + elapsed + " seconds.") 
		}
	}












































/*
for (i <- gpreds; j <- 1 to slen if i != j) {
val label = if (heads(j) == i && datum.hasArg(i, j) && labelHidden) "+" else ""
println("sslink(%d,%d)\t%s%s".format(i, j, label, SRLFeatures.connectFeatures(tokens, i, j).mkString(" ")))
}		
}
*/

/*	
@slen   22
@words  Rolls - Royce Motor Cars Inc. said it expects its U.S. sales to remain steady at about 1,200 cars in 1990 .
@tags   NNP HYPH NNP NNP NNS NNP VBD PRP VBZ PRP$ NNP NNS TO VB JJ IN CD NN NNS IN CD .
@roles  A1 A3 A0 AM-LOC AM-MNR AM-TMP
@gpreds 0 5 7 9 12 14 19
DUMMY   1
pred(5) 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21



println("@slen\t%d".format(slen))
println("@words\t%s".format(tokens.tail.map(_.word).mkString(" ")))
println("@tags\t%s".format(tokens.tail.map(_.pos).mkString(" ")))
println("@roles\t%s".format(args.mkString(" ")))
println("@gpreds\t0 %s".format(gpreds.mkString(" ")))
println("DUMMY\tXXXXX")

if (srl) {
for (i <- 0 until slen) {
if (datum.hasPred(i))
val plabel = if (datum.hasPred(i)) "+" else ""
println("pred(%d)\t%s%s".format(i, plabel, SRLFeatures.predicateFeatures(i, tokens).mkString(" ")))
for (j <- 1 to slen) {
val afeatures = SRLFeatures.argumentFeatures(j, i, tokens, srlmode)
val alabel = if (datum.hasArg(i, j)) "+" else ""
println("hasArg(%d,%d)\t%s%s".format(i, j, alabel, afeatures.mkString(" ")))

for (k <- 0 until args.size) {
val llabel = if (datum.hasLabel(i, j, args(k))) "+" else ""
println("hasLabel(%d,%d,%d)\t%s%s".format(i, j, k, llabel, afeatures.map("%s-%s".format(args(k), _)).mkString(" ")))					
}
}
}
}
if (syntax) extractSyntacticFeatures(datum, !(srl && !labelHidden), skipSyntax)
if (connect) extractConnectionFeatures(datum, labelHidden, gpreds)
println
}

def extractConnectionFeatures(datum: SRLDatum, labelHidden: Boolean = true, gpreds: Array[Int]) = {
val slen = datum.size
val words = datum.words
val tags  = datum.tags
val tokens = Array(dummyToken) ++ datum.tokens 
val heads = Array(-1) ++ datum.heads //Array(-1) ++ lines.map(_.split("\t")(8).toInt)
for (i <- gpreds; j <- 1 to slen if i != j) {
val label = if (heads(j) == i && datum.hasArg(i, j) && labelHidden) "+" else ""
println("sslink(%d,%d)\t%s%s".format(i, j, label, SRLFeatures.connectFeatures(tokens, i, j).mkString(" ")))
}		
}

def extractSyntacticFeatures(datum: SRLDatum, labelHidden: Boolean = true, skip: Boolean) = {
val slen = datum.size
val tokens = Array(dummyToken) ++ datum.tokens 
val heads = Array(-1) ++ datum.heads //lines.map(_.split("\t")(8).toInt)
for (i <- 0 to slen; j <- 1 to slen if i != j) {
val label = if (heads(j) == i && labelHidden) "+" else ""
if (skip) {
println("un(%d,%d)\t%s%s".format(i, j, label, "X_DUMMY")) //dependencyFeatures(tokens, i, j).mkString(" ")))
}
else {
//				println("un(%d,%d)\t%s%s".format(i, j, label, dependencyFeatures(tokens, i, j).mkString(" "))) //dependencyFeatures(tokens, i, j).mkString(" ")))
println("un(%d,%d)\t%s%s".format(i, j, label, narad.nlp.parse.McDonaldFeatures.extract(tokens, i, j).mkString(" "))) //dependencyFeatures(tokens, i, j).mkString(" ")))
}
}
}

def main(args: Array[String]) = {
val options = new narad.util.ArgParser(args)
val filename  = options.getString("--srl.file")
val format    = options.getString("--format", "CoNLL09")
for (datum <- SRLReader.iterator(options)) {
featurize(datum, options)
}
}
}



val feats	    = options.getString("--feats", "srl")
val labelHidden = options.getBoolean("--label.hidden", false)
val prune = options.getBoolean("--prune", false)
val mode   = options.getString("--mode", "train")
val skipSyntax = options.getBoolean("--skip.syntax", false)
assert(format == "CoNLL09" || format == "CoNLL08", "Invalid SRL format: " + format)

val labels = if (mode == "train")
findLabels(filename, format=format)
else
io.Source.fromFile(labelFile).getLines.toArray

val predtags = if (mode == "train")
findPredTags(filename)
else
io.Source.fromFile(tagFile).getLines.toArray

var count = 0
for (chunk <- ChunkReader.read(filename)) {
if (count % 10 == 0) System.err.println("Extracting features for sentence %d".format(count))
featurize(SRLDatum.constructFromCoNLL(chunk.split("\n"), format), options)
count += 1
}
}
}
*/


// The 0 in the gpreds is just to circumvent a problem where if only
// a single index is valid for a sentence, facidx in bpdp does not update
// the list of gpreds for that example.  The dummy potential is used in 
// situations where heavy pruning removes all other potentials and would
// otherwise crash the reader.


/*	
def featurize(lines: Array[String], labels: Array[String], predtags: Array[String], 		
srl: Boolean = false, syntax: Boolean = false, connect: Boolean = false, 
labelHidden: Boolean = true, prune: Boolean = false, bigram: Int = 0, 
format: String, skipSyntax: Boolean = false, mode: Int = 1) = {
*/