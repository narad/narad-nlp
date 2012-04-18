package narad.projects.cparser
import scala.collection.mutable.{ArrayBuffer, HashSet}
import narad.nlp.parse._


object FeatureFactory {
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
		return letter == letter.toUpperCase
	}

	def getUnigramFeatures(token: Token, label: String) : Array[String] = {
		val features = new ArrayBuffer[String]
		features += "%s_WORD-%s".format(label, token.word)
		features += "%s_POS-%s".format(label, token.pos)
		features += "%s_CAP-%s".format(label, isCapitalized(token.word))
		features += "%s_WORDPOS-%s_%s".format(label, token.word, token.pos)
		return features.toArray
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
		val tokens = tree.tokens
		val conjTags = tokens.slice(start, end).map(_.pos).mkString("-")
		val startTag = if (start == 0) "$START" else tokens(start-1).pos
		val endTag   = if (end == tokens.size) "$END" else tokens(end).pos
		val features = new ArrayBuffer[String]
		features += "OUTSIDE_POS-%s-%s".format(startTag, endTag)
		features += "INSIDE_POS-%s".format(conjTags)
		features.toArray
	}

	def syntaxFeatures(tree: Tree, start: Int, end: Int, window: Int = 2): Array[String] = {
		val features = new ArrayBuffer[String]
		val tokens = pad(tree.tokens, window, window)
		val si = start + window
		val ei = end + window-1
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
		// CCM Features
		features ++= ccmFeatures(tree, start, end)
		features.toArray	
	}


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
		// Tree Features
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
		return features.toArray
	}

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

	def getUnaryFeatures(tree: Tree, start: Int, end: Int): Array[String] = {
		val features = new ArrayBuffer[String]
		val window = 2
		val tokens = pad(tree.tokens, window, window)
		val si = start + window
		val ei = end + window
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

		return features.toArray
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
		return buffer.toArray
	}	
}