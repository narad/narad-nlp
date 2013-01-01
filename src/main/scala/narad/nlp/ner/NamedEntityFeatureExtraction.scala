package narad.nlp.ner
import java.io.{File, FileWriter}
import narad.io.onto.{OntoDatum, OntoReader}

import narad.nlp.ling.Lexicon
import narad.nlp.trees.Token
import narad.util.ArgParser
import scala.collection.mutable.ArrayBuffer

/*
object NamedEntityFeatureExtraction {

	def featureExtractionLoop(nerFile: String, syntaxFile: String, labels: Array[String], outputFile: String, options: ArgParser) = {
		val startTime = System.currentTimeMillis()
		val printInterval  = options.getInt("--print.interval", 100)
		val dictionaryDir  = options.getString("--dictionaries", "/Users/narad/Desktop/work/data/dictionaries")
		val dictionaries   = readDictionaries(dictionaryDir)
		val min = options.getInt("--min", 0)
		System.err.println("%d dictionaries found...".format(dictionaries.size))
		for (dict <- dictionaries) System.err.println("Loaded dictionary: %s (%d)".format(dict.name, dict.size))
		val reader = OntoReader.read(nerFile, syntaxFile, options)
		var i = 1
		val out = new FileWriter(outputFile)
		System.err.print("Processing.")
		for (datum <- reader if datum.size >= min) {
			if (i % printInterval == 0) System.err.print("...%d".format(i))
			featureExtraction(datum, labels, out, dictionaries, options)
			i += 1
		}
		System.err.println()
		System.err.println("Feature extraction time: " + (System.currentTimeMillis() - startTime) / 1000.0 + "s.")		
		out.close()
	}

	def featureExtraction(datum: OntoDatum, labels: Array[String], out: FileWriter, dictionaries: Array[Lexicon], options: ArgParser) = {
		val order = options.getInt("--order", 10)
		val model = options.getString("--model") 
		val useBrackets = options.getBoolean("--use.syntax.brackets")
		val useLabels = options.getBoolean("--use.syntax.labels")
		val ner   = datum.ner
		val tokens = datum.tree.tokens()
		val slen = tokens.size
		if (options.getBoolean("--print.header", true)) {
			out.write("@slen\t%d\n".format(slen))				
			out.write("@labels\t%s\n".format(labels.mkString(" ")))
			out.write("@order\t%d\n".format(order))
		}

		if (options.getBoolean("--ner.features", true)) {
			for (j <- slen to 1 by -1) {
				var labeled = false
				for (i <- Math.max(0, j-order) to j-1) {
					val width = j-i
					val feats = nerFeatures(tokens, i, j)
					val correctSpan = if (ner.containsSpan(i,j) || (width == 1 && !ner.coversSpan(i,j))) "+" else ""
					out.write("nerbracket(%d,%d)\t%s%s\n".format(i, j, correctSpan, feats.mkString(" ")))

					if (width == 1) {
						val builder = new StringBuilder()
						for (f <- feats) builder.append("O_" + f)
						if (!ner.containsSpan(i,j) && !ner.coversSpan(i,j)) {
							out.write("nerlabel(%d,%d,1)\t+%s\n".format(i,j, builder.toString().trim))
						}
						else {
							out.write("nerlabel(%d,%d,1)\t%s\n".format(i,j, builder.toString().trim))
						}
					}

					val correctLabel = if ((!ner.containsSpan(i,j) && width > 1) || (width == 1 && ner.coversSpan(i,j))) "+" else ""
					out.write("nerlabel(%d,%d,0)\t%sNone\n".format(i, j, correctLabel))					

					for (label <- labels) {
						val builder = new StringBuilder()
						for (f <- feats) builder.append(" " + label + "_" + f)
						val correctLabel = if (ner.containsSpanLabel(i,j,label)) "+" else ""
						out.write("nerlabel(%d,%d,%s)\t%s%s\n".format(i, j, labels.indexOf(label)+2, correctLabel, builder.toString().trim))
					}
				}
			}				
		}
		out.write("\n")
	}

	def connectionFeatures(tokens: Array[Token], start: Int, end: Int): Array[String] = {
		val sent = tokens
		val features = new ArrayBuffer[String]
		features += "AGREE"  // Agreement bias
		features += "AG_SPAN_SIZE-%d".format(end - start)
		// features += "AG_START_WORD-%s".format(sent(start).word)
			    features += "AG_START_POS-%s".format(sent(start).pos)
			    features += "AG_START_CPOS-%s".format(sent(start).pos.substring(0,1))

			    features += "AG_START_WORDPOS-%s".format(sent(start).word +"_" + sent(start).pos)
			    features += "AG_START_WORDCPOS-%s".format(sent(start).word +"_" + sent(start).pos.substring(0,1))

		//features += "AG_END_WORD-%s".format(sent(end-1).word)
			    features += "AG_END_POS-%s".format(sent(end-1).pos)
			    features += "AG_END_CPOS-%s".format(sent(end-1).pos.substring(0,1))

		//	    features += "END_WORDPOS-%s".format(sent(end-1).word + "_" + sent(end-1).pos)
		//	    features += "END_WORDCPOS-%s".format(sent(end-1).word + "_" + sent(end-1).pos.substring(0,1))

		//	    features += "STARTEND_WORD-%s_%s".format(sent(start).word, sent(end-1).word)
			    features += "AG_STARTEND_POS-%s_%s".format(sent(start).pos, sent(end-1).pos)
			    features += "AG_STARTEND_CPOS-%s_%s".format(sent(start).pos.substring(0,1), sent(end-1).pos.substring(0,1))
		features.toArray
	}

	def pad(array: Array[Token], spad: Int, epad: Int): Array[Token] = {
		val STARTPOS = "START"
		val ENDPOS = "END"
		val buffer = new ArrayBuffer[Token]
		val end = spad + epad + array.size
		for (i <- 0 until end){
			if (i < spad){
				buffer += Token("[START%d]".format(spad-i), STARTPOS)
			}
			else if (i >= array.size + spad){
				buffer += Token("[END%d]".format(1 + i - (array.size + spad)), ENDPOS)
			}
			else{
				buffer += array(i-spad)	
			}	
		}
		return buffer.toArray
	}



	def readDictionaries(dir: String): Array[Lexicon] = {
		val lexicons = new ArrayBuffer[Lexicon]
		try {
			for (file <- new File(dir).listFiles) {
				if (file.getName.endsWith(".txt")) {
					val lexicon = new Lexicon(file.getName.substring(0, file.getName.indexOf(".")))
					for (entry <- io.Source.fromFile(file).getLines()) {
						lexicon.index(entry.trim)
					}
					lexicons += lexicon
				}			
			}
		}
		catch {
			case e: Exception => {
				System.err.println("Error reading dictionary.")
			}
		}
		return lexicons.toArray
	}
}

*/

//			out.write("@words\t%s\n".format(tokens.map(_.word).mkString(" ")))
//			out.write("@tags\t%s\n".format(tokens.map(_.pos).mkString(" ")))
//			out.write("@labels\t%s\n".format(labels.mkString(" ")))
//			out.write("@order\t%d\n".format(order))
//		out.write("@entities:\t%s\n".format(ner.entities.mkString("\n")))
//			for (i <- 0 until slen; j <- i+1 to slen) { //} if (j-i) <= order) {
	//				val width = j-i
