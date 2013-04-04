package narad.nlp.tagger

import narad.bp.structure._
import narad.bp.optimize._
import narad.bp.util._
import narad.io.conll._
import java.io._
import collection.mutable.{HashMap, ArrayBuffer, Map}
import narad.bp.util.index.Index

class UnigramTaggerModel(params: TaggerParams) extends TaggerModel(params) with UnigramTaggerFeatures with UpgradeableTo[ChainTaggerModelInstance] {
  val LABEL_PATTERN  = """ulabel\(([0-9]+),(.+)\)""".r

  def upgrade(ex: ModelInstance, dict: Index[String]): ChainTaggerModelInstance = {
    val attrs = ex.ex.getAttributes
    val slen  = attrs.getOrElse("slen", "-1").toInt
    val words = attrs.getOrElse("words", "").split(" ")
    val tags  = attrs.getOrElse("tags", "").split(" ")

    val margThreshold = 0.2

    val beliefs = ex.marginals
    val groups = beliefs.filter(_.name.startsWith("ulabel")).groupBy{pot =>
      val labelPattern(widx, lidx) = pot.name
      widx
    }
    val fg = ex.graph.toBuilder
    for (i <- 1 to slen) {
//      if (upot.value > margThreshold) {
        // get features for factor
        // add binary factor
//      }
//      fg.addTable2Factor(varName1, varName2, arity1, arity2, "bigramFac(%s,%s)".format(v1, v2), bgroup._2)
    }
    return new ChainTaggerModelInstance(fg.toFactorGraph, ex.ex)
  }
}

trait UnigramTaggerFeatures extends TaggerFeatures {
	
	override def extractFeatures(trainFile: String, trainFeatureFile: String, dict: TagDictionary, index: Index[String], params: TaggerParams) = {
		val useIndices = false
		val in = trainFile
		val alltags = dict.all.toArray
		val out = new FileWriter(trainFeatureFile)
    val reader = new CoNLLReader(in)
    var startTime = System.currentTimeMillis()
    val batchSize = params.FEATURE_BATCH_SIZE
    System.err.println("Feat batch size = " + batchSize)
    reader.zipWithIndex.grouped(batchSize).foreach { batch =>
      val batchArray = batch.toArray
      val pexs = new Array[PotentialExample](batchArray.size)
      batchArray.par.map { case(datum, i) =>
        if (i % params.PRINT_INTERVAL == 0) System.err.println("  example %d...[index contains %d elements].".format(i, index.size))
        pexs(i % batchSize) = getExample(datum, alltags, index, params)
      }
      pexs.foreach { pex =>
        pex.writeToFile(out)
      }
    }
    out.close()
    System.err.print("Feature Extraction Finished in %fs.".format((System.currentTimeMillis() - startTime) / 1000.0))
  }

  def getExample(datum: CoNLLDatum, tags: Array[String], index: Index[String], params: TaggerParams): PotentialExample = {
    val slen = datum.slen
    val attributes  = Map[String, String]()
    val potentials = new ArrayBuffer[Potential]
    val featureMap = new HashMap[String, Array[Feature]]
    attributes("slen") = datum.slen.toString
    attributes("words") = datum.words.mkString(" ")
    attributes("tags") = tags.mkString(" ")
    attributes("bigram") = "false"
    datum.words.zipWithIndex.foreach { case(word, widx) =>
      val goldtag = datum.postag(widx+1)
      val feats = unigramFeatures(datum, widx+1, params)
      tags.zipWithIndex.foreach { case(tag, tidx) =>
        val potname = "ulabel(%d,%d)".format(widx+1, tidx)
        potentials += new Potential(1.0, potname, tag == goldtag)
        featureMap(potname) = feats.map(f => new Feature(index.index(tag + "_" + f), 1.0, 0))
      }
    }
    new PotentialExample(attributes, potentials, featureMap)
  }
}
































/*
    util.zipWithIndex.foreach { case(datum, i) =>
      if (i % params.PRINT_INTERVAL == 0) System.err.println("  example %d...[index contains %d elements].".format(i, index.size))
      val pex = getFeatures(datum, alltags, index)
      pex.writeToFile(out)
      out.write("\n")
    }
*/


/*

trait UnigramTaggerFeatures extends TaggerFeatures {

	override def extractFeatures(trainFile: String, trainFeatureFile: String, dict: TagDictionary, params: TaggerParams) = {
		val useIndices = false
		val in = trainFile
		val alltags = dict.all.toArray
		val out = new FileWriter(trainFeatureFile)
		val rout = new FileWriter(trainFeatureFile + ".bpdp")
    val util = new CoNLLReader(in)
		util.zipWithIndex.foreach { case(datum, i) =>
			println("Found datum:")
			println(datum)
			if (i % params.PRINT_INTERVAL == 0) System.err.println("  example %d...".format(i))
			val slen = datum.slen
			out.write("@slen\t%d\n".format(slen))
			out.write("@words\t%s\n".format(datum.words.mkString(" ")))
			out.write("@bigram\tfalse\n")
			rout.write("@slen\t%d\n".format(slen))
			rout.write("@words\t%s\n".format(datum.words.mkString(" ")))
			rout.write("@bigram\tfalse\n")
			datum.words.zipWithIndex.foreach { case(word, widx) =>
				val feats = unigramFeatures(datum, widx+1, useMorph=false, useSyntax=false)
				val tags = if (dict.contains(word)) dict.tags(word).toArray else alltags
				tags.zipWithIndex.foreach { case(tag, tidx) =>
//				dict.getOrElse(word, alltags).toArray.zipWithIndex.foreach { case(tag, tidx) =>
					val builder = new StringBuilder()
					for (f <- feats) builder.append(" " + tag + "_" + f)
					val isCorrect = correct(datum, widx+1, params.MODE) == tag //datum.postag(widx+1) == tag
					val ll = if (useIndices) tidx else tag
					out.write("ulabel(%d,%s)\t%s%s\n".format(widx+1, tag, if (isCorrect) "+" else "", builder.toString().trim))
					rout.write("ulabel(%d,%d)\t%s%s\n".format(widx+1, tidx, if (isCorrect) "+" else "", builder.toString().trim))
				}
			}
			out.write("\n")
			rout.write("\n")
		}
		out.close()
		rout.close()
	}
}
 */


















/*

class ModelMode extends Enumeration {
	type ModelMode = Value
	val Train, Test = Value
}



val in = mode match {
	case Train => params.TrainFile
	case Test => params.TestFile
}
val out = mode match {
	case Train => new FileWriter(params.TrainFeatureFile) 
	case Test =>  new FileWriter(params.TestFeatureFile)
}		
CoNLLReader.iterator(in).zipWithIndex.foreach { case(datum, i) =>




def main(args: Array[String]) = {
	val params = new TaggerParams(args)
	val tagger = new UnigramTaggerModel(params)
	tagger.extractFeatures(params, Value("Train")
	tagger.extractFeatures(params, Value("Test"))
	tagger.train
	tagger.test
}

*/

/*	
			var i = 0
			val out = new FileWriter(params.)
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
	}
}
*/

	
	
/*	
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