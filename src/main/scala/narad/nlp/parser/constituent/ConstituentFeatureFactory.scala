package narad.nlp.parser.constituent
import narad.nlp.parse._
import narad.nlp.trees.{Token, Tree}
import narad.io.reader.TreebankReader
import scala.collection.mutable.{ArrayBuffer, HashSet}
import java.lang.StringBuilder
import scala.math._

object ConstituentFeatureFactory {
	val STARTPOS = "$"
	val ENDPOS   = "&"

	def unigramFeatures(token: Token): Array[String] = {
		val features = new ArrayBuffer[String]
		features += "WORD-%s".format(token.word)
		features += "TAG-%s".format(token.pos)
		features += "WORD-TAG-%s".format(token.word, token.pos)
		features += "CAP-%s".format(isCapitalized(token.word).toString)
		features.toArray
	}

	def isCapitalized(str: String): Boolean = {
		val letter = str.substring(0,1)
		letter == letter.toUpperCase
	}

	def getUnigramFeatures(token: Token, label: String) : Array[String] = {
		val features = new ArrayBuffer[String]
		features += "%s_WORD-%s".format(label, token.word)
		features += "%s_POS-%s".format(label, token.pos)
		features += "%s_CAP-%s".format(label, isCapitalized(token.word))
		features += "%s_WORDPOS-%s_%s".format(label, token.word, token.pos)
		features.toArray
	}

	def bigramFeatures(t1: Token, t2: Token): Array[String] = {
		val features = new ArrayBuffer[String]
		features += "BIGRAM-W-W-%s-%s".format(t1.word, t2.word)
		features += "BIGRAM-W-P-%s-%s".format(t1.word, t2.pos)
		features += "BIGRAM-P-W-%s-%s".format(t1.pos,  t2.word)
		features += "BIGRAM-P-P-%s-%s".format(t1.pos,  t2.pos)
		features.toArray		
	}

	def ccmFeatures(tree: Tree, start: Int, end: Int): Array[String] = {
		val tokens = tree.tokens()
		val conjTags = tokens.slice(start, end).map(_.pos).mkString("-")
		val startTag = if (start == 0) "$START" else tokens(start-1).pos
		val endTag   = if (end == tokens.size) "$END" else tokens(end).pos
		val features = new ArrayBuffer[String]
		features += "OUTSIDE_POS-%s-%s".format(startTag, endTag)
		features += "INSIDE_POS-%s".format(conjTags)
		features.toArray
	}
	
	
	def syntaxSpanFeatures(tokens: Array[Token], si: Int, ei: Int, window: Int = 5, mode: Int = 3): Array[String] = {
		val features = new ArrayBuffer[String]
//		val si = start
//		val ei = end
		val start = si - window
		val end = si - window
		val width = 1 + end-start
		
		// Simple features
		features += "[spanwidth]-%d".format(width)
		features += "[span-start]-%d".format(start)   
		features += "[span-end]-%d".format(end)
		
		// Unigram Features
		val startToken = tokens(si)
		val startWord = startToken.word.toLowerCase
		val startTag  = startToken.pos
		features += "[start-word]-%s".format(startWord)
		features += "[start-tag]-%s".format(startTag)
		features += "[start-cap]-%s".format(isCapitalized(startToken.word))
		features += "[start-word-tag]-%s-%s".format(startWord, startTag)

		val endToken = tokens(ei)
		val endWord  = endToken.word.toLowerCase
		val endTag   = endToken.pos
		features += "[end-word]-%s".format(endWord)
		features += "[end-tag]-%s".format(endTag)
		features += "[end-cap]-%s".format(isCapitalized(endToken.word))
		features += "[end-word-tag]-%s-%s".format(endWord, endTag)

		val prevToken = tokens(si-1)
		val prevWord  = prevToken.word.toLowerCase
		val prevTag   = prevToken.pos
		features += "[prev-word]-%s".format(prevWord)
		features += "[prev-tag]-%s".format(prevTag)
		features += "[prev-cap]-%s".format(isCapitalized(prevToken.word))
		features += "[prev-word-tag]-%s-%s".format(prevWord, prevTag)

		val postToken = tokens(ei+1)
		val postWord  = postToken.word.toLowerCase
		val postTag   = postToken.pos
		features += "[post-word]-%s".format(postWord)
		features += "[post-tag]-%s".format(postTag)
		features += "[post-cap]-%s".format(isCapitalized(postToken.word))
		features += "[post-word-tag]-%s-%s".format(postWord, postTag)
	
		// Bigram Features
		features += "[start-word-X-end-word]-%s-%s".format(startWord, endWord)
		features += "[start-word-X-end-tag]-%s-%s".format(startWord, endTag)
		features += "[start-tag-X-end-word]-%s-%s".format(startTag, endWord)
		features += "[start-tag-X-end-tag]-%s-%s".format(startTag, endTag)

		// Tag Concatenation Features
		for (i <- 0 until min(width, window); j <- 1 until min(width, 5)-i) {
			features += i + "[tags-from-start]--" + tokens.slice(si+i,si+j).map(_.pos).mkString("-")			
			if (j <= 3) {
				features += i + "[words-from-start]--" + tokens.slice(si+i,si+j).map(_.word).mkString("-")			
			}
		}
		for (i <- 0 until min(width, window); j <- 1 until min(width, 5)-i) {
			features += i + "[tags-before-end]--" + tokens.slice(ei-j,ei-i).map(_.pos).mkString("-")			
			if (j <= 3) {
				features += i + "[words-before-end]--" + tokens.slice(ei-j,ei-i).map(_.word).mkString("-")			
			}
		}		
		
		// "Footprint" Features
		features += "[start-footprint-1-1]-%s".format(tokens.slice(si-1, si+1).map(_.pos).mkString("-"))
		features += "[start-footprint-1-2]-%s".format(tokens.slice(si-1, si+2).map(_.pos).mkString("-"))
		features += "[start-footprint-2-1]-%s".format(tokens.slice(si-2, si+1).map(_.pos).mkString("-"))
		features += "[start-footprint-2-2]-%s".format(tokens.slice(si-2, si+2).map(_.pos).mkString("-"))
		features += "[start-footprint-3-3]-%s".format(tokens.slice(si-3, si+3).map(_.pos).mkString("-"))

		features += "[end-footprint-1-1]-%s".format(tokens.slice(ei-1, ei+1).map(_.pos).mkString("-"))
		features += "[end-footprint-1-2]-%s".format(tokens.slice(ei-1, ei+2).map(_.pos).mkString("-"))
		features += "[end-footprint-2-1]-%s".format(tokens.slice(ei-2, ei+1).map(_.pos).mkString("-"))
		features += "[end-footprint-2-2]-%s".format(tokens.slice(ei-2, ei+2).map(_.pos).mkString("-"))
		features += "[end-footprint-3-3]-%s".format(tokens.slice(ei-3, ei+3).map(_.pos).mkString("-"))
		
		features += "bigram-footprint-3-3]-%s-%s".format(tokens.slice(si-1, si+1).map(_.pos).mkString("-"),
																										 tokens.slice(ei-1, ei+1).map(_.pos).mkString("-"))
		features += "bigram-footprint-5-5]-%s-%s".format(tokens.slice(si-2, si+2).map(_.pos).mkString("-"),
																										 tokens.slice(ei-2, ei+2).map(_.pos).mkString("-"))

		// Within-Span features
		if (width < 10) {
			for (i <- si to ei) {
				features += "[contains-word]-%s".format(tokens(i).word)
				features += "[contains-tag]-%s".format(tokens(i).pos)			
			}
			for (i <- si until ei) {
				features += "[contains-word-bigram]-%s-%s".format(tokens(i).word, tokens(i+1).word)
				features += "[contains-tag-bigram]-%s-%s".format(tokens(i).pos, tokens(i+1).pos)
			}			
		}
		
		// CCM Features
		features += "[ccm-outside-pos]-%s-%s".format(tokens(si-1).pos, tokens(ei+1).pos)
		features += "[ccm-inside-pos]-%s".format(tokens.slice(si, ei).map(_.pos).mkString("-"))

		features.toArray
	}
	
	def syntaxLabelFeatures(tokens: Array[Token], start: Int, end: Int, twindow: Int = 5, wwindow: Int = 3): Array[String] = {
		val features = new ArrayBuffer[String]
		val words = tokens.map(_.word.toLowerCase)
		val tags  = tokens.map(_.pos)
		val width = end - start
		var last = ""
		features += "START-POS-" + tags(start)
		features += "START-WORD-" + words(start)
		features += "END-POS-" + tags(end)
		features += "END-WORD-" + words(end)

		last = "START-POS-" + tags(start)
		for (i <- 1 to min(twindow, width-1)) {
			features += (last + "-" + tags(start+i))
			last = features.last
		}

		last = "START-WORD-" + words(start)
		for (i <- 1 to min(wwindow, width-1)) {
			features += (last + "-" + words(start+i))
			last = features.last
		}

		last = "END-POS-" + tags(end)
		for (i <- 1 to min(twindow, width-1)) {
			features += (last + "-" + tags(end-i))
			last = features.last
		}
		
		last = "END-WORD-" + words(end)
		for (i <- 1 to min(wwindow, width-1)) {
			features += (last + "-" + words(end-i))
			last = features.last
		}
		
		val numFeats = features.size
		for (i <- 0 until numFeats) {
			features += features(i) + "-" + width
		}
/*		
		last += "OUTSIDE-START-POS-" + tags(start)
		for (i <- 1 to min(twindow, start-1)) {
			features += (features.last + "-" + tags(start-i))
			last = features.last
		}

		last += "OUTSIDE-START-WORD-" + words(start)
		for (i <- 1 to min(wwindow, start-1)) {
			features += (features.last + "-" + words(start-i))
			last = features.last
		}

		last += "OUTSIDE-END-POS-" + tags(start)
		for (i <- 1 to min(twindow, tokens.size-end)) {
			features += (features.last + "-" + tags(end+i))
			last = features.last
		}

		last += "OUTSIDE-END-WORD-" + words(end)
		for (i <- 1 to min(wwindow, tokens.size-end)) {
			features += (features.last + "-" + words(end+i))
			last = features.last
		}
*/		
		features += "[ccm-outside-pos]-%s-%s".format(tokens(start-1).pos, tokens(end+1).pos)
		features += "[ccm-inside-pos]-%s".format(tokens.slice(start, end).map(_.pos).mkString("-"))
		
		
		features.toArray
	}
	
/*		
		features ++= unigramFeatures(tokens(si))   map("%s-%s".format("START", _))
		features ++= unigramFeatures(tokens(ei))   map("%s-%s".format("END", _))
		features ++= unigramFeatures(tokens(si-1)) map("%s-%s".format("PREV", _))
		features ++= unigramFeatures(tokens(ei+1)) map("%s-%s".format("NEXT", _))		
		// Bigram Features
		features ++= bigramFeatures(tokens(si),   tokens(ei)) map("%s-%s".format("START-END", _))

		// Outer Bigram Features
		features ++= bigramFeatures(tokens(si-1), tokens(ei))   map("%s-%s".format("PREV-END", _))
		features ++= bigramFeatures(tokens(si),   tokens(ei+1)) map("%s-%s".format("START-END", _))
		features ++= bigramFeatures(tokens(si-1), tokens(ei+1)) map("%s-%s".format("START-END", _))
		// Inner Bigram Features
		// McDonald-esque Features
		val tags = tokens.slice(si,ei).foldLeft(new HashSet[String])(_ += _.pos)
		for (tag <- tags) {
			features += "CONTAINS-%s-%s-%s".format(tokens(si).word, tag, tokens(ei).word)
			features += "CONTAINS-%s-%s-%s".format(tokens(si).pos, tag, tokens(ei).pos)
		}
		// Within-Span features
		for (i <- si to ei) {
			features += "CONTAINS-WORD-%s".format(tokens(i).word)
			features += "CONTAINS-POS-%s".format(tokens(i).pos)			
		}
		for (i <- si until ei) {
			features += "CONTAINS-BIGRAM-%s-%s".format(tokens(i).word, tokens(i+1).word)
			features += "CONTAINS-BIGRAM-%s-%s".format(tokens(i).pos, tokens(i+1).pos)
		}
		
		for (i <- 0 until min(width, 5); j <- 1 until min(width, 5)-i) {
			features += i + "ps--" + tokens.slice(si+i,si+j).map(_.pos).mkString("-")			
			if (j <= 3) {
				features += i + "ws--" + tokens.slice(si+i,si+j).map(_.word).mkString("-")			
			}
		}

		for (i <- 0 until min(width, 5); j <- 1 until min(width, 5)-i) {
			features += i + "pe--" + tokens.slice(ei-j,ei-i).map(_.pos).mkString("-")			
			if (j <= 3) {
				features += i + "we--" + tokens.slice(ei-j,ei-i).map(_.word).mkString("-")			
			}
		}
		
		if (width < 10) features += "span-shape--" + tokens.slice(si, ei).map(t => if (isCapitalized(t.word)) "C" else "c").mkString("-")
		// CCM Features
		features ++= ccmFeatures(tree, start, end)
		features.toArray	
	}
	*/
	
	
	

	def syntaxFeatures2(tokens: Array[Token], start: Int, end: Int, window: Int = 5, mode: Int = 3): Array[String] = {
		val features = new ArrayBuffer[String]
//		val tokens = pad(tree.tokens, window, window)
//		val si = start + window
//		val ei = end + window-1
		val si = start
		val ei = end
		val width = end-start
		features += "[bias]"

		features += "SPAN_SIZE-%d".format(width)
		features += "SPAN_START-%d".format(start)   
		features += "SPAN_END-%d".format(end)
		// Unigram Features
		features ++= unigramFeatures(tokens(si))   map("%s-%s".format("START", _))
		features ++= unigramFeatures(tokens(ei))   map("%s-%s".format("END", _))
		features ++= unigramFeatures(tokens(si-1)) map("%s-%s".format("PREV", _))
		features ++= unigramFeatures(tokens(ei+1)) map("%s-%s".format("NEXT", _))		

		// Bigram Features
		features ++= bigramFeatures(tokens(si),   tokens(ei)) map("%s-%s".format("START-END", _))

		// Outer Bigram Features
		features ++= bigramFeatures(tokens(si-1), tokens(ei))   map("%s-%s".format("PREV-END", _))
		features ++= bigramFeatures(tokens(si),   tokens(ei+1)) map("%s-%s".format("START-END", _))
		features ++= bigramFeatures(tokens(si-1), tokens(ei+1)) map("%s-%s".format("START-END", _))
		// Inner Bigram Features
		// McDonald-esque Features
		val tags = tokens.slice(si,ei).foldLeft(new HashSet[String])(_ += _.pos)
		for (tag <- tags) {
			features += "CONTAINS-%s-%s-%s".format(tokens(si).word, tag, tokens(ei).word)
			features += "CONTAINS-%s-%s-%s".format(tokens(si).pos, tag, tokens(ei).pos)
		}
		// Within-Span features
		for (i <- si to ei) {
			features += "CONTAINS-WORD-%s".format(tokens(i).word)
			features += "CONTAINS-POS-%s".format(tokens(i).pos)			
		}
		for (i <- si until ei) {
			features += "CONTAINS-BIGRAM-%s-%s".format(tokens(i).word, tokens(i+1).word)
			features += "CONTAINS-BIGRAM-%s-%s".format(tokens(i).pos, tokens(i+1).pos)
		}
		
		// CCM Features
//		features ++= ccmFeatures(tree, start, end)
		features += "[ccm-outside-pos]-%s-%s".format(tokens(si-1).pos, tokens(ei+1).pos)
		features += "[ccm-inside-pos]-%s".format(tokens.slice(si, ei).map(_.pos).mkString("-"))

		features.toArray	
	}


		def syntaxFeatures3(tokens: Array[Token], start: Int, end: Int, window: Int = 5, mode: Int = 3): Array[String] = {
			val features = new ArrayBuffer[String]
	//		val tokens = pad(tree.tokens, window, window)
	//		val si = start + window
	//		val ei = end + window-1
			val si = start
			val ei = end
			val width = end-start
			features += "[bias]"
			features += "SPAN_SIZE-%d".format(end - start)
			features += "SPAN_START-%d".format(start)   
			features += "SPAN_END-%d".format(end)
			// Unigram Features
			features ++= unigramFeatures(tokens(si))   map("%s-%s".format("START", _))
			features ++= unigramFeatures(tokens(ei))   map("%s-%s".format("END", _))
			features ++= unigramFeatures(tokens(si-1)) map("%s-%s".format("PREV", _))
			features ++= unigramFeatures(tokens(ei+1)) map("%s-%s".format("NEXT", _))		

			// Bigram Features
			features ++= bigramFeatures(tokens(si),   tokens(ei)) map("%s-%s".format("START-END", _))

			// Outer Bigram Features
			features ++= bigramFeatures(tokens(si-1), tokens(ei))   map("%s-%s".format("PREV-END", _))
			features ++= bigramFeatures(tokens(si),   tokens(ei+1)) map("%s-%s".format("START-END", _))
			features ++= bigramFeatures(tokens(si-1), tokens(ei+1)) map("%s-%s".format("START-END", _))
			// Inner Bigram Features
			// McDonald-esque Features
			val tags = tokens.slice(si,ei).foldLeft(new HashSet[String])(_ += _.pos)
			for (tag <- tags) {
				features += "CONTAINS-%s-%s-%s".format(tokens(si).word, tag, tokens(ei).word)
				features += "CONTAINS-%s-%s-%s".format(tokens(si).pos, tag, tokens(ei).pos)
			}
			// Within-Span features
			for (i <- si to ei) {
				features += "CONTAINS-WORD-%s".format(tokens(i).word)
				features += "CONTAINS-POS-%s".format(tokens(i).pos)			
			}
			for (i <- si until ei) {
				features += "CONTAINS-BIGRAM-%s-%s".format(tokens(i).word, tokens(i+1).word)
				features += "CONTAINS-BIGRAM-%s-%s".format(tokens(i).pos, tokens(i+1).pos)
			}

			// Tag Concatenation Features
			for (i <- 0 until min(width, window); j <- 1 until min(width, 5)-i) {
				features += i + "[tags-from-start]--" + tokens.slice(si+i,si+j).map(_.pos).mkString("-")			
				if (j <= 3) {
					features += i + "[words-from-start]--" + tokens.slice(si+i,si+j).map(_.word).mkString("-")			
				}
			}
			for (i <- 0 until min(width, window); j <- 1 until min(width, 5)-i) {
				features += i + "[tags-before-end]--" + tokens.slice(ei-j,ei-i).map(_.pos).mkString("-")			
				if (j <= 3) {
					features += i + "[words-before-end]--" + tokens.slice(ei-j,ei-i).map(_.word).mkString("-")			
				}
			}		

			// "Footprint" Features
			features += "[start-footprint-1-1]-%s".format(tokens.slice(si-1, si+1).map(_.pos).mkString("-"))
			features += "[start-footprint-1-2]-%s".format(tokens.slice(si-1, si+2).map(_.pos).mkString("-"))
			features += "[start-footprint-2-1]-%s".format(tokens.slice(si-2, si+1).map(_.pos).mkString("-"))
			features += "[start-footprint-2-2]-%s".format(tokens.slice(si-2, si+2).map(_.pos).mkString("-"))
			features += "[start-footprint-3-3]-%s".format(tokens.slice(si-3, si+3).map(_.pos).mkString("-"))

			features += "[end-footprint-1-1]-%s".format(tokens.slice(ei-1, ei+1).map(_.pos).mkString("-"))
			features += "[end-footprint-1-2]-%s".format(tokens.slice(ei-1, ei+2).map(_.pos).mkString("-"))
			features += "[end-footprint-2-1]-%s".format(tokens.slice(ei-2, ei+1).map(_.pos).mkString("-"))
			features += "[end-footprint-2-2]-%s".format(tokens.slice(ei-2, ei+2).map(_.pos).mkString("-"))
			features += "[end-footprint-3-3]-%s".format(tokens.slice(ei-3, ei+3).map(_.pos).mkString("-"))

			features += "bigram-footprint-3-3]-%s-%s".format(tokens.slice(si-1, si+1).map(_.pos).mkString("-"),
																											 tokens.slice(ei-1, ei+1).map(_.pos).mkString("-"))
			features += "bigram-footprint-5-5]-%s-%s".format(tokens.slice(si-2, si+2).map(_.pos).mkString("-"),
																											 tokens.slice(ei-2, ei+2).map(_.pos).mkString("-"))


			// CCM Features
	//		features ++= ccmFeatures(tree, start, end)
			features += "[ccm-outside-pos]-%s-%s".format(tokens(si-1).pos, tokens(ei+1).pos)
			features += "[ccm-inside-pos]-%s".format(tokens.slice(si, ei).map(_.pos).mkString("-"))

			features.toArray	
		}
		
		

	def unaryFeatures(tree: Tree, index: Int, treeFeatures:Boolean = false, window: Int=4): Array[String] = {
		val features = new ArrayBuffer[String]
		val tokens = pad(tree.tokens(), window, window)
		features += "BIAS"
		// Simple Features
		features += "INDEX-%d".format(index)
		features += "LENGTH-%d".format(index)
		var ccount = 0
		for (i <- index to index+window+window) {
			ccount += 1
			features ++= unigramFeatures(tokens(i))
			features ++= unigramFeatures(tokens(i)).map("%d-%s".format(ccount, _))
		}
		features.toArray
	}

		// Tree Features
/*
def unaryFeatures(tree: Tree, index: Int, treeFeatures:Boolean = false, window: Int=4): Array[String] = {
	val features = new ArrayBuffer[String]
	val tokens = pad(tree.tokens, window, window)
	features += "BIAS"
	// Simple Features
	features += "INDEX-%d".format(index)
	features += "LENGTH-%d".format(index)
	var ccount = 0
	for (i <- index to index+window+window) {
		ccount += 1
		features ++= unigramFeatures(tokens(i))
		features ++= unigramFeatures(tokens(i)).map("%d-%s".format(ccount, _))
	}
	return features.toArray
}


		try {
			if (treeFeatures) {
				val path = tree.derivation(index)
				var scount = 0
				if (path.size > 0) {
					scount += 1
					for (sibling <- path(1)) {
						features += "SIBLING-%d-%s".format(scount, sibling.label)
					}
				}
				var pcount = 0
				for (parent <- path) {
					features += "PARENT-%d-%s".format(pcount, parent.label)
				}	
			}
		}
		catch {
			case e: Exception => System.err.println("Error doing unary featurization for sentence: %s".format(tree.toString))
		}
		*/

	def bigramGenerator(tokens : Array[Token], start: Int, end: Int, cutin: Int, cutout: Int, cpos: Boolean): Array[String] = {
		val features = new ArrayBuffer[String]
		var startword = ""; var startpos  = ""
		val cstr = if (cpos) "c" else ""
		for (i <- (start - cutout) to (start + cutin)){
			startword += tokens(i).word
			if (cpos) startpos += tokens(i).pos.substring(0,1) else startpos += tokens(i).pos
			var endword = ""; var endpos  = "" 
			for (j <- (end - cutin) to (end + cutout)){
				endword += tokens(j).word
				if (cpos) endpos += tokens(j).pos.substring(0,1) else endpos += tokens(j).pos
				features += "MCD_%sSWSPEWEP_c%d_%d-%s_%s_%s_%s".format(cstr, cutin, cutout, startword, startpos, endword, endpos)
				features += "MCD_%sSWSPEW_c%d_%d-%s_%s_%s".format(cstr, cutin, cutout, startword, startpos, endword)
				features += "MCD_%sSWSPSP_c%d_%d-%s_%s_%s".format(cstr, cutin, cutout, startword, startpos, endpos)
				features += "MCD_%sSWEWEP_c%d_%d-%s_%s_%s".format(cstr, cutin, cutout, startword, endword, endpos)
				features += "MCD_%sSPEWEP_c%d_%d-%s_%s_%s".format(cstr, cutin, cutout, startpos, endword, endpos)
				features += "MCD_%sSWEW_c%d_%d-%s_%s".format(cstr, cutin, cutout, startword, endword)
				features += "MCD_%sSPEP_c%d_%d-%s_%s".format(cstr, cutin, cutout, startpos, endpos)
				features += "MCD_%sSWEP_c%d_%d-%s_%s".format(cstr, cutin, cutout, startword, endpos)
				features += "MCD_%sSPEW_c%d_%d-%s_%s".format(cstr, cutin, cutout, startpos, endword)
			}
		}
		features.toArray
	}

	def getUnaryFeatures(tree: Tree, start: Int): Array[String] = {
		val features = new ArrayBuffer[String]
		val window = 2
		val tokens = pad(tree.tokens(), window, window)
		val si = start + window
	//	val ei = end + window
		features += "BIAS"
		features += "WORD-%s".format(tokens(si).word)
		features += "TAG-%s".format(tokens(si).pos)
		features += "PREV_WORD-%s".format(tokens(si-1).word)
		features += "PREV_TAG-%s".format(tokens(si-1).pos)
		features += "PPREV_WORD-%s".format(tokens(si-2).word)
		features += "PPREV_TAG-%s".format(tokens(si-2).pos)
		features += "NEXT_WORD-%s".format(tokens(si+1).word)
		features += "NEXT_TAG-%s".format(tokens(si+1).pos)
		features += "NNEXT_WORD-%s".format(tokens(si+2).word)
		features += "NNEXT_TAG-%s".format(tokens(si+2).pos)
		features += "WORDTAG-%s_%s".format(tokens(si).word, tokens(si).pos)
		features += "PWORDTAG-%s_%s".format(tokens(si-1).word, tokens(si).pos)
		features += "WORDPTAG-%s_%s".format(tokens(si).word, tokens(si-1).pos)
		features += "WORDNTAG-%s_%s".format(tokens(si).word, tokens(si+1).pos)
		features += "NWORDTAG-%s_%s".format(tokens(si+1).word, tokens(si).pos)

		features += "PTAG2-%s_%s".format(tokens(si-1).pos, tokens(si).pos)
		features += "NTAG2-%s_%s".format(tokens(si).pos, tokens(si+1).pos)
		features += "TAG3-%s_%s_%s".format(tokens(si-1).pos, tokens(si).pos, tokens(si+1).pos)

		features += "PWORD2-%s_%s".format(tokens(si-1).word, tokens(si).word)
		features += "NWORD2-%s_%s".format(tokens(si).word, tokens(si+1).word)
		features += "WORD3-%s_%s_%s".format(tokens(si-1).word, tokens(si).word, tokens(si+1).word)

		features.toArray
	}

	def pad(array: Array[Token], spad: Int, epad: Int): Array[Token] = {
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
		buffer.toArray
	}	
	
	def main(args: Array[String]) {
		val ex = "(TOP (FRAG (INTJ (JJ Good) (NN evening)) (, ,) (NP (NN everyone)) (. .)))"
		val tree = TreebankReader.parseExpression(ex, defaultLabel="TOP").removeNones.binarize
		tree.annotateWithIndices(0)
		val labels = tree.getSpans.map(_.label).toArray.distinct
//		println(tree)
//		println
		Featurize.printFeatures(tree, labels, "syntaxunary", "blah", prune=false, vpots=Array[Array[String]](), options=new narad.util.ArgParser(Array[String]()))
	}
	
}