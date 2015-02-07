/*
package narad.nlp
import narad.util.ArgParser
import narad.bp.structure._
import narad.bp.train._
import narad.bp.util._
import narad.io.reader._
import narad.nlp.trees._
import scala.collection.mutable.{ArrayBuffer, HashSet}
import narad.nlp.ling._
//import narad.nlp.ner._
import java.io.{BufferedReader, File, FileReader, FileWriter}
import narad.util.Index
//import narad.nlp.parser.constituent._
import scala.util.matching.Regex
import narad.nlp.tagger._
import narad.learn.FeatureIndex


object NLP {

/*
	def main(args: Array[String]) = {
		val params = new NLPParams(args)
		val model = getModel(params.model)
		if (params.extractFeatures) {
			model.extractFeatures(params)
		}
		if (params.integerize) {
			val feats = FeatureIndex.construct(trainOutputFile, pruneFeats)
			feats.writeIndex(params.featsFile)
			feats.writeFeatures(new File(trainOutputFile), new FileWriter(trainOutputFile + ".fidx"))			
			feats.writeFeatures(new File(testOutputFile), new FileWriter(testOutputFile + ".fidx"))				
		}
		if (params.train) {
			model.train(params)
		}
		if (params.test) {
			model.test(params)
		}
	}

*/


	def main(args: Array[String]) = {
		val params   = new NLPParams(args)
		val options = new ArgParser(args)
		val model = params.model
		val nerTrainFile = params.nerTrainFile
		val nerTestFile  = params.nerTestFile
		val syntaxTrainFile = params.syntaxTrainFile
		val syntaxTestFile  = params.syntaxTestFile
		val taggerTrainFile = params.taggerTrainFile
		val taggerTestFile  = params.taggerTestFile
		val trainOutputFile = params.trainOutputFile
		val testOutputFile  = params.testOutputFile
		val modelOutFile    = params.modelOutputFile
		val testFidxFile = params.testFidxFile
		val featsFile  = params.featsFile
		val pruneFeats = params.pruneFeats
		val maxEx      = params.maxEx
		val iterations = params.iterations
		val rate       = params.rate
		val initRate   = params.initRate
		val prune      = params.prune
		val verbose    = params.verbose
		val integerize = params.integerize
		val outFile = params.outputFile
		val min = params.min
		val max = params.max
		val forder = params.featureOrder		
		val format = params.format
		val tagType = params.tagType
		val window = params.window
		val printInterval = params.printInterval
		val labeltype = "ONTO_FINE"

		if (params.extractFeatures) {
			if (model == "ner" || model == "nerjoint") {
/*				val labels = labeltype match {
					case "ONTO_FINE" => {
						Array("CARDINAL", "DATE", "EVENT", "FAC", "GPE", "LANGUAGE", "LAW", "LOC", "MONEY", "NORP", "ORDINAL", "ORG", "PERCENT", "PERSON", "PRODUCT", "QUANTITY", "TIME", "WORK_OF_ART")
					}
					case "ONTO_COARSE" => {
						Array("LOC", "MISC", "ORG", "PERSON")
					}
					case _ => {
						val labelset = new HashSet[String]
						NamedEntityReader.read(nerTrainFile, options).foreach(labelset ++= _.labels)
						labelset.toArray						
					}
				}
				System.err.println("Using label set: %s".format(labels.mkString(" ")))
				NamedEntityFeatureExtraction.featureExtractionLoop(nerTrainFile, syntaxTrainFile, labels, trainOutputFile, options)
				NamedEntityFeatureExtraction.featureExtractionLoop(nerTestFile, syntaxTestFile, labels, testOutputFile, options)							
			}
			else if (model == "syntax") {
				System.err.println("Using files: %s; %s".format(syntaxTrainFile, syntaxTestFile))
				val reader = TreebankReader.read(syntaxTrainFile, options)
				val stats = PruneStatistics.construct(reader, 25)
				featureExtractionLoop(syntaxTrainFile, stats, trainOutputFile, options)
				featureExtractionLoop(syntaxTestFile, stats, testOutputFile, options)							
*/			}
			else if (model == "tagger") {
				val params = new TaggerParams(args)
					val dictionary    = TagDictionary.construct(params.TrainFile, mode=tagType)
					dictionary.toFile("tags.dict")
					val bigramFeats   = forder == "bigram"
					val labels = dictionary.all
//					val useMorph = params.taggerMorphFeats
//					val useSyntax = params.taggerSyntaxFeats
//					System.err.println("Using label set (%d labels):\n%s\n".format(labels.size, labels.mkString("\n")))
//					Tagger.featureLoop(taggerTrainFile, trainOutputFile, dictionary, tagType, bigramFeats, useMorph, useSyntax, window, printInterval)
//					Tagger.featureLoop(taggerTestFile,  testOutputFile,  dictionary, tagType, bigramFeats, useMorph, useSyntax, window, printInterval)

					val tagger = if (options.getString("--feature.order") == "bigram") {
						System.err.println("Extracting features for bigram tagger...")
						new BigramTagger(params, null.asInstanceOf[FactorGraph])
					}
					else {
						System.err.println("Extracting features for unigram tagger...")
						new UnigramTagger(params)
					}
					val dict   = TagDictionary.construct(params.TrainFile, mode="FINE")
					println(dict.all.mkString("\n"))
					tagger.extractFeatures(params.TrainFile, params.TrainFeatureFile, dict, params)
					tagger.extractFeatures(params.TestFile, params.TestFeatureFile, dict, params)

			}
		}
		if (params.integerize) {
			System.err.println("Integerizing...")
			System.err.println("..constructing index...")
			var startTime = System.currentTimeMillis()
			val feats = FeatureIndex.construct(trainOutputFile, pruneFeats) //featureIndex(trainOutputFile)
			System.err.println("Feature index time: " + (System.currentTimeMillis() - startTime) / 1000.0 + "s.")		
			startTime = System.currentTimeMillis()
			System.err.println("..%d features...".format(feats.elements.size))
			feats.writeIndex(new File(featsFile))
			System.err.println("..mapping features to integers...")
			feats.indexFile(new File(trainOutputFile), new FileWriter(trainOutputFile + ".fidx"))			
			feats.indexFile(new File(testOutputFile), new FileWriter(testOutputFile + ".fidx"))			
			System.err.println("Indexed writing time: " + (System.currentTimeMillis() - startTime) / 1000.0 + "s.")		
		}
		if (options.getBoolean("--train")) {
			System.err.println("Training...")
			if (integerize) {
				val fidxFile = params.trainOutputFile + ".fidx"
				val initFile = params.initFile //options.getString("--init.file", "null")
				val pvsize = io.Source.fromFile(featsFile).getLines.size // else params.pvsize
				System.err.println("PV size = " + pvsize)
				model match {
/*					case "syntax" => {
						train(fidxFile, initFile, modelOutFile, pvsize, iterations, maxEx, verbose=verbose, Parser.construct)							
					}
					case "nerjoint" => {
						train(fidxFile, initFile, modelOutFile, pvsize, iterations, maxEx, verbose=verbose, JointNamedEntityModel.construct)													
					}
					case "ner" => {
						train(fidxFile, initFile, modelOutFile, pvsize, iterations, maxEx, verbose=verbose, NamedEntityModel.construct)							
					}
*/					case "tagger" => {
//						train(fidxFile, initFile, modelOutFile, pvsize, iterations, maxEx, verbose=verbose, Tagger.construct)													
					}
					case _=> 
				}
			}
			else {
				val pvsize   = params.pvsize
				val fidxFile = params.trainFidxFile
				val initFile = params.initFile
				model match {
/*					case "syntax" => {
						train(fidxFile, initFile, modelOutFile, pvsize, iterations, maxEx, verbose=verbose, Parser.construct)							
					}
					case "nerjoint" => {
						train(fidxFile, initFile, modelOutFile, pvsize, iterations, maxEx, verbose=verbose, JointNamedEntityModel.construct)												
					}
					case "ner" => {
						train(fidxFile, initFile, modelOutFile, pvsize, iterations, maxEx, verbose=verbose, NamedEntityModel.construct)							
					}
*/					case "tagger" => {
		//				train(fidxFile, initFile, modelOutFile, pvsize, iterations, maxEx, verbose=verbose, Tagger.construct)													
					}
					case _=> 
				}
			}
		}
		if (options.getBoolean("--test")) {
			System.err.println("Testing...")
			val fidxFile = if (integerize) params.testOutputFile + ".fidx" else testFidxFile
			val initFile = if (integerize) modelOutFile + ".pv" else params.initFile //options.getString("--init.file", "null")
			model match {
/*				case "syntax" => {
					test(fidxFile, initFile, maxEx, verbose, outFile, Parser.construct)									
				}
				case "nerjoint" => {
					val gdatums = OntoReader.read(nerTestFile, syntaxTestFile, options).toArray.map(_.ner)
					val tdatums = testNERJoint(fidxFile, initFile, verbose=verbose, constructor=JointNamedEntityModel.construct)
					val fw = new java.io.FileWriter(outFile)
					tdatums.foreach(t => fw.write(t.entities.mkString(", ") + "\n"))
					fw.close
					//				val tdatums = testNER(fidxFile, initFile, verbose=verbose, constructor=nerConstructor)
					println("F1 = " + NamedEntityEval.eval(gdatums, tdatums))				
					println(tdatums.size + " vs " + gdatums.size)
					for (g <- gdatums; e <- g.entities) println("e = " + e)
					println("-----------------")
					for (g <- tdatums; e <- g.entities) println("e = " + e)										
				}
				case "ner" => {
//					val gdatums = OntoReader.read(nerTestFile, syntaxTestFile, options).toArray.map(_.ner)
					val tdatums = "blah"
					val tstring =  testNER(fidxFile, initFile, verbose=verbose, constructor=NamedEntityModel.construct)
					val fw = new java.io.FileWriter(outFile)
					fw.write(tstring)
//					tdatums.foreach(t => fw.write(t.entities.mkString(", ") + "\n"))
					fw.close					

					//				val tdatums = testNER(fidxFile, initFile, verbose=verbose, constructor=nerConstructor)
/*
					println("F1 = " + NamedEntityEval.eval(gdatums, tdatums))				
					println(tdatums.size + " vs " + gdatums.size)
					for (g <- gdatums; e <- g.entities) println("e = " + e)
					println("-----------------")
					for (g <- tdatums; e <- g.entities) println("e = " + e)					
*/
				}
*/				case "tagger" => {
//					testTagger(fidxFile, initFile, maxEx, verbose, outFile, Tagger.construct)														
				}
			}
		}
	}
/*
	def train(fidxFile: String, initFile: String, outFile: String, pvsize: Int, iterations: Int = 10, 
		maxEx: Int = -1, verbose: Boolean = false, constructor: (PotentialExample, Array[Potential]) => FactorGraphModel) = {
		var params = init(initFile, pvsize)
		val nrExamples = if (maxEx == -1) countExamples(fidxFile) else maxEx
		for (i <- 0 until iterations) {
			val nparams = SGDTrainer.train(params, fidxFile, constructor, maxExamples = nrExamples, bpIters = 10, verbose = verbose)
			if (i+1 == iterations) {
				writeToFile(nparams.tail, outFile + ".pv")
			}
			else{
				writeToFile(nparams.tail, outFile + "." + (i+1) + ".pv")				
			}
			params = nparams
		}
	}
*/	
/*	def testNER(fidxFile: String, initFile: String, maxEx: Int = -1, verbose: Boolean = false, constructor: (PotentialExample, Array[Potential]) => FactorGraphModel): String = { //Array[NamedEntityDatum] = { 
		var params = init(initFile)
		val maxExamples = if (maxEx == -1) countExamples(fidxFile) else maxEx
		var count = 0
//		val ents = new ArrayBuffer[NamedEntityDatum]
		val ents = new ArrayBuffer[String]
		for (ex <- PotentialReader.read(fidxFile) if count < maxExamples) {			
//			val words     = ex.attributes("@words").split(" ")
//			val tags      = ex.attributes("@tags").split(" ")
//			val slen      = words.size
			val slen = ex.attributes("@slen").trim.toInt
			val model = SGDTrainer.test(params, constructor, ex, bpIters = 10, verbose = verbose).asInstanceOf[NamedEntityModel]
			//ents += 
			val spans = model.decode(slen)///(words, tags)
			for (s <- spans) ents += "%d\t%d\t%s".format(s._1, s._2, s._3) //s.mkString("\t")
//			ents += spans
			ents += ""
			count += 1
		}
//		ents.toArray
		ents.mkString("\n")
	}

	def testNERJoint(fidxFile: String, initFile: String, maxEx: Int = -1, verbose: Boolean = false, constructor: (PotentialExample, Array[Potential]) => FactorGraphModel): Array[NamedEntityDatum] = { 
		var params = init(initFile)
		val maxExamples = if (maxEx == -1) countExamples(fidxFile) else maxEx
		var count = 0
		val ents = new ArrayBuffer[NamedEntityDatum]
		for (ex <- PotentialReader.read(fidxFile) if count < maxExamples) {			
			val words     = ex.attributes("@words").split(" ")
			val tags      = ex.attributes("@tags").split(" ")
			val slen      = words.size
			val model = SGDTrainer.test(params, constructor, ex, bpIters = 10, verbose = verbose).asInstanceOf[JointNamedEntityModel]
			ents += model.decode(words, tags)
			count += 1
		}
		ents.toArray
	}

	def testTagger(fidxFile: String, initFile: String, maxEx: Int = -1, verbose: Boolean = false, outFile: String, constructor: (PotentialExample, Array[Potential]) => FactorGraphModel) = { 
		var params = init(initFile)
		val maxExamples = if (maxEx == -1) countExamples(fidxFile) else maxEx
		var count = 0
		val tags = new ArrayBuffer[String]
		for (ex <- PotentialReader.read(fidxFile) if count < maxExamples) {			
//			val words   = ex.attributes("@words").split(" ")
//			val labels  = ex.attributes("@tagset").split(" ")
//			val tagType = ex.attributes("@tagtype")
//			val slen      = words.size
			val model = SGDTrainer.test(params, constructor, ex, bpIters = 10, verbose = verbose).asInstanceOf[TaggerModel]
			tags ++= model.decode
//			tags ++= model.decode(words, labels, tagType)
			tags += ""
			count += 1
		}
		val fw = new java.io.FileWriter(outFile)
		tags.foreach(t => fw.write(t.toString + "\n"))
		fw.close
	}
*/	/*		
	def test(fidxFile: String, initFile: String, maxEx: Int = -1, verbose: Boolean = false, outFile: String, constructor: (PotentialExample, Array[Potential]) => FactorGraphModel) = { 
		var params = init(initFile)
		val maxExamples = if (maxEx == -1) countExamples(fidxFile) else maxEx
		var count = 0
		val trees = new ArrayBuffer[Tree]
		for (ex <- PotentialReader.read(fidxFile) if count < maxExamples) {			
			val words     = ex.attributes("@words").split(" ")
			val tags      = ex.attributes("@tags").split(" ")
			val slen      = words.size
			val model = SGDTrainer.test(params, constructor, ex, bpIters = 10, verbose = verbose).asInstanceOf[Parser]
			trees += model.decode(words, tags)
			count += 1
		}
		val fw = new java.io.FileWriter(outFile)
		trees.foreach(t => fw.write(t.toString + "\n"))
		fw.close
	}


	def featureExtractionLoop(syntaxFile: String, stats: ParserStatistics, outputFile: String, options: ArgParser) = {
		val startTime = System.currentTimeMillis()
		val printInterval   = options.getInt("--print.interval", 100)
		val reader = TreebankReader.read(syntaxFile, options)
		val min = options.getInt("--min", 0)
		var i = 1
		val out = new FileWriter(outputFile)
		System.err.print("Processing.")
		val bpdp = options.getBoolean("--bpdp")
		for (datum <- reader if datum.slen >= min) {
			if (i % printInterval == 0) System.err.print("...%d".format(i))
			Parser.featurizeSyntax(datum, stats, false, out, bpdp, options)				
			i += 1
		}
		System.err.println
		System.err.println("Feature extraction time: " + (System.currentTimeMillis() - startTime) / 1000.0 + "s.")		
		out.close
	}	

	def init(initFile: String, pvsize: Int = 0): Array[Double] = {
		assert(initFile != "null" || pvsize > 0, "No init.file or pv.size specified.")
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
*/
	def writeToFile(array: Array[Double], filename: String) {
		val fw = new java.io.FileWriter(filename)
		array.foreach(v => fw.write(v + "\n")) 
		fw.close
	}

	def countExamples(filename: String): Int = {
		System.err.print("Determining number of examples in fidx file <%s>\n...".format(filename))
		val endPattern   = """[ \n\t]*""".r
		var count = 0
		for (line <- scala.io.Source.fromFile(filename).getLines) {
//			System.err.println(line.size + " " + line)
			if (line.size == 0) count += 1
		}
		System.err.println(count + ".")
		return count
	}
	
}




//	val activeFeats = new HashSet[String]
/*
	def fires(f: String): Boolean = activeFeats.contains(f)
*/



	


class NLPParams(args: Array[String]) extends ArgParser(args){
	

	// General Options
	def train = getBoolean("--train")
	def test = getBoolean("--test")
	def model = getString("--model")
	def verbose       = getBoolean("--verbose", false)	

  // Feature Extraction Options	
	def extractFeatures = getBoolean("--extract.features")
	def trainOutputFile = getString("--train.output.file", "train.feats")
	def testOutputFile  = getString("--test.output.file", "test.feats")	
	def featsFile  = getString("--feats.file")
	def pruneFeats = getBoolean("--prune.feats")
	def integerize = getBoolean("--integerize")
	
	// Training Options
	def modelOutputFile  = getString("--model.out.file", "model")
	def maxEx         = getInt("--max.examples", -1)
	def iterations    = getInt("--iterations", 10)
	def rate          = getDouble("--rate", 0.3)
	def initRate      = getDouble("--init.rate", 1)
	def prune         = getBoolean("--prune", false)
	def pvsize        = getInt("--pv.size", -1) 
	def trainFidxFile = getString("--train.fidx.file") //trainOutputFile + ".fidx"
	
	// Testing Options
	def testFidxFile = getString("--test.fidx.file") //testOutputFile + ".fidx"
	def outputFile   = getString("--output.file", "test.out")
	def initFile     = getString("--init.file", "null")
	
	// Parser Options
	def syntaxTrainFile = getString("--syntax.train.file")
	def syntaxTestFile  = getString("--syntax.test.file")
	def topLabel = getString("--top.label", "TOP")
	
	// Named Entity Options
	def nerTrainFile    = getString("--ner.train.file")
	def nerTestFile     = getString("--ner.test.file")		
	
	// Tagger options
	def taggerTrainFile = getString("--tagger.train.file")
	def taggerTestFile  = getString("--tagger.test.file")
		
	def format = getString("--input.format")
	def tagType = getString("--tag.type", "FINE")
	def window  = getInt("--feature.window", 0)
	def featureOrder = getString("--feature.order", "unigram")
	
	def taggerSyntaxFeats = getBoolean("--tagger.syntax.feats")
	def taggerMorphFeats  = getBoolean("--tagger.morph.feats")
	
	
	def printInterval = getInt("--print.interval", 100)	
	
	def min = getInt("--min", 0)
	def max = getInt("--max", 999999999)
}








/*
			val reader = model match {
				case "ner" => {
					new NamedEntityReader()
				}
				case _=> {
					System.err.println("Error: --model.fun %s is an unsupported model type.".format(model))
				}
			}
		}
	}
	
}

*/


//import scala.collection.mutable.ArrayBuffer

/*
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
	
	*/
	
	*/
	
	
