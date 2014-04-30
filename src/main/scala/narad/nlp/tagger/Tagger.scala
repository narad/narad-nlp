package narad.nlp.tagger

import narad.bp.optimize._
import narad.bp.util._
import narad.bp.util.index._
import narad.io.conll.CoNLLReader
import java.io.File
import narad.nlp.tagger.dependency._

object Tagger {

  def main(args: Array[String]) {
    val params = new MorphTaggerParams(args)
    run(params)
  }

  def run(params: MorphTaggerParams) {
    System.err.println("Constructing Dictionary...")
    val dict   = if (params.TRAIN_AND_TEST_DICT) {
      MultiTagDictionary.construct(new CoNLLReader(params.TRAIN_FILE) ++ new CoNLLReader(params.TEST_FILE), params.ATTRIBUTES)
    }
    else {
      MultiTagDictionary.construct(new CoNLLReader(params.TRAIN_FILE), params.ATTRIBUTES)
    }
    dict.toFile("tags.dict")

    if (params.MODEL == "DEPENDENCY") {
      val tagger = new DependencyTaggerModel(params, dict)
      if (params.EXTRACT_FEATURES) {
        System.err.println("Extracting Features...")
        if (params.HASH) {
          val index = new HashIndex(params.PV_SIZE)
          tagger.extractFeatures2(params.TRAIN_FILE, params.TRAIN_FEATURE_FILE, index, params)
          tagger.extractFeatures2(params.TEST_FILE, params.TEST_FEATURE_FILE, index, params)
        }
        else {
          val index = new ArrayIndex[String]()
          tagger.extractFeatures2(params.TRAIN_FILE, params.TRAIN_FEATURE_FILE, index, params)
          index.writeToFile("feats")
          index.freeze
          tagger.extractFeatures2(params.TEST_FILE, params.TEST_FEATURE_FILE, index, params)
        }
      }
    }
    else {
      val tagger = new MorphTaggerModel(params, dict)
      if (params.EXTRACT_FEATURES) {
        System.err.println("Extracting Features...")
        if (params.HASH) {
          val index = new HashIndex(params.PV_SIZE)
          tagger.extractFeatures(params.TRAIN_FILE, params.TRAIN_FEATURE_FILE, index, params)
          tagger.extractFeatures(params.TEST_FILE, params.TEST_FEATURE_FILE, index, params)
        }
        else {
          val index = new ArrayIndex[String]()
          tagger.extractFeatures(params.TRAIN_FILE, params.TRAIN_FEATURE_FILE, index, params)
          index.writeToFile("feats")
          index.freeze
          tagger.extractFeatures(params.TEST_FILE, params.TEST_FEATURE_FILE, index, params)
        }
      }
    }


    val tagger = if (params.MODEL == "DEPENDENCY") {
      new DependencyTaggerModel(params, dict)
    }
    else {
      new MorphTaggerModel(params, dict)
    }

      if (params.TRAIN) {
      val optimizer = new Optimizer(tagger, params) // with L2Regularizer
      val data = new PotentialReader(params.TRAIN_FIDX_FILE)
      optimizer.train(data)
    }

    for (attr <- params.ATTRIBUTES) {
      val f = new File("test.tagged." + attr)
      f.delete()
    }

    if (params.TEST) {
      val optimizer = new Optimizer(tagger, params)
      val data = new PotentialReader(params.TEST_FIDX_FILE)
      optimizer.test(data)
    }
  }
}


















































//	var dict = new TagDictionary
//  var index = new ArrayIndex[String]()


//      val optimizer = if (params.GROUP_REGULARIZER) {
//        new Optimizer(tagger, params) //with GroupRegularizer
//      }
//      else {
//      }

 /*

	def main(args: Array[String]) {
    val params = new TaggerParams(args)
    if (params.COARSE_TO_FINE) {
      CoarseToFineTagger.run(params)
    }
    else if (params.MODEL.contains("MORPH")) {
      runMorph(new MorphTaggerParams(args))
    }
    else {
      run(params)
    }
  }

  def run(params: TaggerParams) {
		val tagger = params.MODEL match {
			case "BIGRAM" => new BigramTaggerModel(params)
			case "DEPENDENCY" => new DependencyTagger(params)
			case _ => new UnigramTaggerModel(params)
		}
		if (params.getBoolean("--extract.features")) {
      val data = new CoNLLReader(params.TRAIN_FILE) ++ new CoNLLReader(params.TEST_FILE)
			val dict   = TagDictionary.construct(data, mode=params.MODE)
			dict.toFile("tags.dict")
      val tags = dict.all
      System.err.println("PV SIZE + " + params.PV_SIZE)
      val index = new HashIndex(params.PV_SIZE) //ArrayIndex[String]()
//			println(dict.all.mkString("\n"))
			tagger.extractFeatures(params.TRAIN_FILE, params.TRAIN_FEATURE_FILE, dict, index, params)
			tagger.extractFeatures(params.TEST_FILE, params.TEST_FEATURE_FILE, dict, index, params)
		}
    if (params.getBoolean("--integerize")) {

    }
		else if (params.getBoolean("--train")) {
			val optimizer = new Optimizer(tagger) with L2Regularizer
      val data = new PotentialReader(params.TRAIN_FIDX_FILE)
      optimizer.train(data, params)
		}
		else if (params.getBoolean("--test")) {
			val optimizer = new Optimizer(tagger)
      val data = new PotentialReader(params.TEST_FIDX_FILE)
      optimizer.test(data, params)
		}
	}

  def runMorph(params: MorphTaggerParams) {
    System.err.println("Running Morph Model")
    System.err.println("TRAIN? " + params.getBoolean("--train"))
    System.err.println("TEST? " + params.getBoolean("--test"))
    val dict   = MultiTagDictionary.construct(new CoNLLReader(params.TRAIN_FILE) ++ new CoNLLReader(params.TEST_FILE))
    dict.toFile("tags.dict")
    val tagger = if (params.MODEL == "MORPHSYNTAX" && (params.OBSERVE_SYNTAX || params.OBSERVE_BIGRAM)) {
      System.err.println("Oracle Model")
      new OracleSyntaxTagger(params, dict)
    }
    else if (params.MODEL == "MORPHSYNTAX") {
      System.err.println("Syntax Model")
      new SyntaxMorphTaggerModel(params, dict)
    }
    else {
      System.err.println("Default Model")
      new OracleSyntaxTagger(params, dict) // new MorphTaggerModel(params, dict)
    }

    if (params.getBoolean("--extract.features")) {
      if (params.HASH) {
        val index = new HashIndex(params.PV_SIZE)
        tagger.extractFeatures(params.TRAIN_FILE, params.TRAIN_FEATURE_FILE, index, params)
        tagger.extractFeatures(params.TEST_FILE, params.TEST_FEATURE_FILE, index, params)
      }
      else {
        val index = new ArrayIndex[String]()
        tagger.extractFeatures(params.TRAIN_FILE, params.TRAIN_FEATURE_FILE, index, params)
        index.writeToFile("feats")
        index.freeze
        tagger.extractFeatures(params.TEST_FILE, params.TEST_FEATURE_FILE, index, params)
      }
    }

    if (params.getBoolean("--train")) {
      println("IN TRAIN!")
      val optimizer = new Optimizer(tagger) with L2Regularizer
      val data = new PotentialReader(params.TRAIN_FIDX_FILE)
      optimizer.train(data, params)
    }

    if (params.getBoolean("--test")) {
      println("IN TEST!")
      val optimizer = new Optimizer(tagger)
      val data = new PotentialReader(params.TEST_FIDX_FILE)
      println("about to test")
      optimizer.test(data, params)
      println("done!")
    }
  }
}


*/















//class TaggerClassifier(pv: Array[Double], tags: Array[String], dict: TagDictionary, index: Index[String], params: TaggerParams) extends TaggerFeatures {






















                              /*
                              		def constructFromExample(ex: PotentialExample, pv: Array[Double]): ModelInstance = { //(pots: Array[Potential], slen: Int, useBigrams: Boolean = false, useSyntax: Boolean = false): TaggerModel = {
			val useBigrams = ex.attributes.getOrElse("bigram", "false").toLowerCase == "true"
      System.err.println("bigram = " + useBigrams)
 //     useBigrams = true
//			for (i <- 0 until pv.size) {
//				println("PRV[%d] = ".format(i) + pv(i))
//			}
			System.err.println("Constructing (%s) Tagger...".format(if (useBigrams) "bigram" else "unigram"))
			val pots = ex.exponentiated(pv)
			System.err.println("%d pots found.".format(pots.size))
			val fg = new FactorGraphBuilder(pots)
			val pidxs = new HashSet[Int]
			val groups = pots.groupBy{pot =>
				val glabelPattern(widx) = pot.name
				widx.toInt
			}//.toList.sortBy(_._1)

			System.err.println(ex.attributes)
			val slen = ex.attributes.getOrElse("slen", "-1").toInt
			System.err.println("%d groups found vs slen of %d".format(groups.size, slen))
			var prevArity = 0
			var prevName = new String

			for (idx <- 1 to slen) {
				val upots = groups(idx).filter(_.name.startsWith("ulabel"))
				val varName = "labelVar(%d)".format(idx)
				fg.addVariable(varName, arity=upots.size)
				fg.addNamed1Factor(varName, "labelFac(%d)".format(idx), upots)

				if (prevArity > 0 && useBigrams) {
					val bpots = groups(idx).filter(_.name.startsWith("blabel"))
					val arity1 = prevArity
					val arity2 = upots.size
					val varName1 = prevName
					val varName2 = varName
 //         System.err.println("arity 1 = " + arity1 + "; arity2 = " + arity2)
//					if (arity1 > 1 && arity2 > 1) {
						fg.addTable2Factor(varName1, varName2, arity1, arity2, "bigramFac(%d)".format(idx), bpots)
//					}
				}
				prevArity = upots.size
				prevName = varName
			}
			return new TaggerModelInstance(fg.toFactorGraph, ex)
		}
                               */




//new TaggerOptions
/*
		def decode: Array[String] = { //(words: Array[String])}, labels: Array[String], tagType: String): Array[String] = {
			val beliefs = graph.potentialBeliefs
			val tags = new ArrayBuffer[String]
			val groups = beliefs.filter(_.name.startsWith("label")).groupBy{pot =>
				val labelPattern(widx, lidx) = pot.name
				widx
			}
			for (group <- groups.toArray.sortBy(_._1.toInt)) {
				val idx = group._1.toInt
				val pots = group._2
				assert(pots.size != 0, "Pots for group %d are empty in decoding?".format(idx))
				val maxpot = narad.util.Functions.argmax[Potential](_.value, pots)
				val labelPattern(i,j) = maxpot.name
				tags += j
	//			if (tagType == "CONCAT") tags += j.split("\\^").mkString("\t") else tags += j
				//tags += j //labels(j.toInt)
			}
	//		println
			tags.toArray
		}
*/

//	def options = params

/*
	def train = {}

	def test = {}
	*/

/*
package narad.nlp.tagger
import narad.io.util.CoNLLReader
import narad.io.datum.CoNLLDatum
import narad.util.ArgParser
import narad.bp.structure._
import narad.bp.train._
import narad.bp.util._
import scala.collection.mutable.HashSet
import java.io._

object Tagger {
	
	def main(args: Array[String]) {
		val options = new ArgParser(args)
		val verbose = options.getBoolean("--verbose")
		if (options.getBoolean("--extract.features")) {
			extractFeatures(options)
		}
		if (options.getBoolean("--train")) {
			train(options)
		}
		if (options.getBoolean("--test")) {
			test(options)
		}
	}
	
	def extractFeatures(options: ArgParser, verbose: Boolean=false) = {
		val trainFilename = options.getString("--train.file")
		val testFilename  = options.getString("--test.file")
		val trainOutput   = options.getString("--train.output.file", "train.feats")
		val testOutput    = options.getString("--test.output.file", "test.feats")
		val format        = options.getString("--input.format")
		val tagType       = options.getString("--tag.type", "CONCAT")
		val mwindow       = options.getString("--")
		val window        = options.getInt("--feature.window", 0)
		val printInterval = options.getInt("--print.interval", 100)
		val pruned				= options.getBoolean("--prune", true)
		val dictionary    = TagDictionary.construct(trainFilename, mode=tagType)
		val syntaxFeats   = true
		val bigramFeats   = true
		val useMorph      = true
		assert(!bigramFeats, "should not be calling this method - use NLP.scala")
		val labels = dictionary.all
		System.err.println("Using label set (%d labels):\n%s\n".format(labels.size, labels.mkString("\n")))
		featureLoop(trainFilename, trainOutput, dictionary, tagType, bigramFeats, syntaxFeats, useMorph, window, printInterval)
		featureLoop(testFilename,  testOutput,  dictionary, tagType, bigramFeats, syntaxFeats, useMorph, window, printInterval)
	}
	
	def featureLoop(infile: String, outfile: String, dict: TagDictionary, tagType: String, bigramFeats: Boolean, useMorphFeats: Boolean, 
		useSyntaxFeats: Boolean, window: Int=2, printInterval: Int=100, useIndices: Boolean = true) = {
			var i = 0
			val out = new FileWriter(outfile)
			for (datum <- CoNLLReader.iterator(infile)) {
				if (i % printInterval == 0) System.err.println("  example %d...".format(i))
				val slen = datum.slen
				val arity = dict.all.toArray.size
				//			println(datum)
				out.write("@slen\t%d\n".format(slen))
				out.write("@arity\t%d\n".format(arity))
				out.write("@words\t%s\n".format(datum.words.mkString(" ")))
				out.write("@tagset\t%s\n".format(dict.all.toArray.mkString(" ")))
				out.write("@tagtype\t%s\n".format(tagType))
				out.write("@bigram\t%s\n".format(bigramFeats))
				//				sparseFeatures(datum, out, dict, tagType, bigramFeats, useMorphFeats, useSyntaxFeats, window, useIndices)

				val slen = datum.slen
				if ()
				for (idx <- 1 to slen) {
					val features = TaggerFeatures.unigramFeatures(datum, i, useMorph=useMorphFeats, useSyntax=useSyntaxFeats)
					val word = datum.form(idx)
					for (lpair <- dict.tags(word).zipWithIndex) {
						writeUnigramFeatures(lpair, features, idx) = {					
					}
					if (bigramFeats && idx > 1) {
						for (prevpair <- dict.tags(word).zipWithIndex) {
							for (curpair <- dict.tags(word).zipWithIndex) {
								writeBigramFeatures(prevpair, curpair, feats, idx)
							}
						}
					}
				}
				out.write("\n")
				i += 1
			}
			out.close			
		}

		def writeFeatures(datum, otagpair: (String, Int), feats: Array[String], i: Int, ll: Int) = {
			val builder = new StringBuilder()
			for (f <- feats) builder.append(" " + label + "_" + f)
			val correct = isCorrect(datum, i, label, tagType)
			val ll = if (useIndices) li else label
			out.write("label(%d,%s)\t%s%s\n".format(i, ll, if (correct) "+" else "", builder.toString.trim))			
		}

		def isCorrect(datum: CoNLLDatum, idx: Int, label: String, tagType: String): Boolean = {
			val correct = tagType match {
				case "COARSE" => datum.cpostag(idx) == label
				case "FINE" => datum.postag(idx) == label
				case "CONCAT" => datum.cpostag(idx) + "^" + datum.postag(idx) == label
				case _=> System.err.println("Invalid tag.type: %s".format(tagType)); false 
			}
			correct		
		}

		
		
		
		
		

		def denseFeatures(datum: CoNLLDatum, out: FileWriter, dict: TagDictionary, tagType: String, bigramFeats: Boolean, useMorphFeats: Boolean, 
			useSyntaxFeats: Boolean, window: Int=2, useIndices: Boolean = true) = {
				val slen = datum.slen
				for (i <- 1 to slen) {
					val features = TaggerFeatures.unigramFeatures(datum, i, useMorph=useMorphFeats, useSyntax=useSyntaxFeats)
					val word = datum.form(i)
					for (lpair <- dict.all.toArray.zipWithIndex) {
						val label = lpair._1
						val li = lpair._2
						val builder = new StringBuilder()
						for (f <- features) builder.append(" " + label + "_" + f)
						val correct = isCorrect(datum, i, label, tagType)
						//					out.write("label(%d,%s)\t%s%s\n".format(i, (li-1).toString, if (correct) "+" else "", builder.toString.trim))
						val ll = if (useIndices) li else label
						out.write("label(%d,%s)\t%s%s\n".format(i, ll, if (correct) "+" else "", builder.toString.trim))
					}

					if (bigramFeats && i > 1) {
						val features = TaggerFeatures.bigramFeatures(datum, tidx=i, fidx=i-1, window=window, useMorph=useMorphFeats, useSyntax=useSyntaxFeats)
						val word2 = datum.form(i-1)
						val labels1 = dict.all.toArray //if (dict.contains(word))  //dict.tags(word).toArray else dict.all.iterator.toArray
						val labels2 = dict.all.toArray // if (dict.contains(word2)) dict.all.toArray // dict.tags(word2).toArray else dict.all.iterator.toArray
						for (l1 <- labels1; l2 <- labels2) {
							val correct = isCorrect(datum, i, l1, tagType) && isCorrect(datum, i-1, l2, tagType)
							val builder = new StringBuilder()
							for (f <- features) builder.append(" " + l1 + "_" + l2 + "_" + f)
							out.write("blabel(%d,%s,%s)\t%s%s\n".format(i, l1, l2, if (correct) "+" else "", builder.toString.trim))
						}
					}
				}
			}


			def sparseFeatures(datum: CoNLLDatum, out: FileWriter, dict: TagDictionary, tagType: String, bigramFeats: Boolean, useMorphFeats: Boolean, 
				useSyntaxFeats: Boolean, window: Int=2, useIndices: Boolean = true) = {
					val slen = datum.slen
					for (i <- 1 to slen) {
						val features = TaggerFeatures.unigramFeatures(datum, i, useMorph=useMorphFeats, useSyntax=useSyntaxFeats)
						val word = datum.form(i)
						for (lpair <- dict.all.toArray.zipWithIndex) {
							val label = lpair._1
							val li = lpair._2
							val builder = new StringBuilder()
							for (f <- features) builder.append(" " + label + "_" + f)
							val correct = isCorrect(datum, i, label, tagType)
							//					out.write("label(%d,%s)\t%s%s\n".format(i, (li-1).toString, if (correct) "+" else "", builder.toString.trim))
							val ll = if (useIndices) li else label
							out.write("label(%d,%s)\t%s%s\n".format(i, ll, if (correct) "+" else "", builder.toString.trim))
						}

						if (bigramFeats && i > 1) {
							val features = TaggerFeatures.bigramFeatures(datum, tidx=i, fidx=i-1, window=window, useMorph=useMorphFeats, useSyntax=useSyntaxFeats)
							val word2 = datum.form(i-1)
							val labels1 = dict.all.toArray //if (dict.contains(word))  //dict.tags(word).toArray else dict.all.iterator.toArray
							val labels2 = dict.all.toArray // if (dict.contains(word2)) dict.all.toArray // dict.tags(word2).toArray else dict.all.iterator.toArray
							for (l1 <- labels1; l2 <- labels2) {
								val correct = isCorrect(datum, i, l1, tagType) && isCorrect(datum, i-1, l2, tagType)
								val builder = new StringBuilder()
								for (f <- features) builder.append(" " + l1 + "_" + l2 + "_" + f)
								out.write("blabel(%d,%s,%s)\t%s%s\n".format(i, l1, l2, if (correct) "+" else "", builder.toString.trim))
							}
						}
					}
				}
				
				
/*	
	def printFeatures(infile: String, outfile: String, dict: TagDictionary, tagType: String, bigramFeats: Boolean, useMorphFeats: Boolean, 
										useSyntaxFeats: Boolean, window: Int=2, printInterval: Int=100, useIndices: Boolean = true) = {
		var i = 0
		val out = new FileWriter(outfile)
		for (datum <- CoNLLReader.iterator(infile)) {
			if (i % printInterval == 0) System.err.println("  example %d...".format(i))
			val slen = datum.slen
//			println(datum)
			out.write("@slen\t%d\n".format(slen))
			out.write("@words\t%s\n".format(datum.words.mkString(" ")))
			out.write("@tagset\t%s\n".format(dict.all.toArray.mkString(" ")))
			out.write("@tagtype\t%s\n".format(tagType))
			out.write("@bigram\t%s\n".format(bigramFeats))
			for (i <- 1 to slen) {
				val features = TaggerFeatures.unigramFeatures(datum, i, useMorph=useMorphFeats, useSyntax=useSyntaxFeats)
				val word = datum.form(i)
				val labels = if (dict.contains(word)) dict.tags(word) else dict.all.iterator
				for (lpair <- labels.zipWithIndex) {
					val label = lpair._1
					val li = lpair._2
					val builder = new StringBuilder()
					for (f <- features) builder.append(" " + label + "_" + f)
					val correct = isCorrect(datum, i, label, tagType)
//					out.write("label(%d,%s)\t%s%s\n".format(i, (li-1).toString, if (correct) "+" else "", builder.toString.trim))
					val ll = if (useIndices) li else label
					out.write("label(%d,%s)\t%s%s\n".format(i, ll, if (correct) "+" else "", builder.toString.trim))
				}
				
				if (bigramFeats && i > 1) {
					val features = TaggerFeatures.bigramFeatures(datum, tidx=i, fidx=i-1, window=window, useMorph=useMorphFeats, useSyntax=useSyntaxFeats)
					val word2 = datum.form(i-1)
					val labels1 = if (dict.contains(word)) dict.tags(word).toArray else dict.all.iterator.toArray
					val labels2 = if (dict.contains(word2)) dict.tags(word2).toArray else dict.all.iterator.toArray
					for (l1 <- labels1; l2 <- labels2) {
						val correct = isCorrect(datum, i, l1, tagType) && isCorrect(datum, i-1, l2, tagType)
						val builder = new StringBuilder()
						for (f <- features) builder.append(" " + l1 + "_" + l2 + "_" + f)
						out.write("blabel(%d,%s,%s)\t%s%s\n".format(i, l1, l2, if (correct) "+" else "", builder.toString.trim))
					}
				}
			}
			out.write("\n")
			i += 1
		}
		out.close
	}
*/
		

	
	/*
	tagType match {
		case "COARSE" => datum.cpostag(i) == label
		case "FINE" => datum.postag(i) == label
		case "CONCAT" => datum.cpostag(i) + "^" + datum.postag(i) == label
		case _=> System.err.println("Invalid tag.type: %s".format(tagType)); false 
	}
	*/
	
	def train(options: ArgParser) = {
		val fidxFile   = options.getString("--fidx.file", "null")
		val initFile   = options.getString("--init.file", "null")
		val outFile    = options.getString("--out.file", "tagger.model")
		val pvsize     = options.getInt("--pv.size", -1)
		val maxEx      = options.getInt("--max.examples", -1)
		val iterations = options.getInt("--iterations", 10)
		val bpIters 	 = options.getInt("--bp.iters", 10)
		val rate       = options.getDouble("--rate", 0.1)
		val initRate   = options.getDouble("--init.rate", 0.01)
		val variance   = options.getDouble("--variance", 1.0)
		val verbose 	 = options.getBoolean("--verbose", false)
		var params = init(initFile, pvsize)
		val nrExamples = if (maxEx == -1) countExamples(fidxFile) else maxEx
		for (i <- 0 until iterations) {
			var nparams = SGDTrainer.train(params, fidxFile, construct, maxExamples = nrExamples, bpIters = 10, verbose = verbose)
			if (i+1 == iterations) {
				writeToFile(nparams.tail, outFile + ".pv")
			}
			else{
				writeToFile(nparams.tail, outFile + "." + (i+1) + ".pv")				
			}
			params = nparams
		}
		
	}
	
	def test(options: ArgParser) = {
		val initFile = options.getString("--init.file", "null")
		val fidxFile = options.getString("--fidx.file", "null")
		val bpIters 	 = options.getInt("--bp.iters", 10)
		val rate       = options.getDouble("--rate", 0.3)
		val initRate   = options.getDouble("--init.rate", 0.01)
		val variance   = options.getDouble("--variance", 1.0)
		val verbose 	 = options.getBoolean("--verbose", false)		
		var params = init(initFile)
		for (ex <- PotentialReader.read(fidxFile)) {
			val slen    = ex.attributes("@slen").toInt
			val words   = ex.attributes("@words").split(" ")
			assert(slen == words.size, "In test fidx file, slen (%d) does not equal the list of words (%d)".format(slen, words.size))
//			val labels  = ex.attributes("@tagset").split(" ")
			val labels = Array[String]()
			val tagType    = ex.attributes("@tagtype")
			val feats   = ex.getFeatures				
			val pots    = ex.getPotentials
			pots.foreach { pot => pot.value = feats(pot.name).foldLeft(0.0)((sum, feat) => sum + params(feat.idx) * feat.value)}
			pots.foreach { pot => pot.value = Math.exp(pot.value) }
			val model = construct(ex, pots)
			SGDTrainer.runBP(model.graph, bpIters, dinit = 1, threshold = 0.1, verbose = verbose)
			model.decode(words, labels, tagType)
		}		
	}
	
	def construct(ex: PotentialExample, pots: Array[Potential]): TaggerModel = {
		val slen    = ex.attributes("@slen").toInt
		val bigrams = ex.attributes("@bigram") == "true"
		TaggerModel.construct(pots, slen, useSyntax=false, useBigrams=bigrams)
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

*/
