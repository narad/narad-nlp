package narad.projects.relmarg
import narad.nlp.relations.RelationToken
import scala.collection.mutable.ArrayBuffer

object REFeatures {
	val stoken = RelationToken("START", "START_POS", "SNA", "SNA", "SNA")
	val etoken = RelationToken("END", "END_POS", "ENA", "ENA", "ENA")
	
	def allcaps(str: String): Boolean = {
		return str.toUpperCase == str
	}
	
	def capitalized(str: String): Boolean = {
		if (str.size == 0) return true
		return (str.substring(0, 1).toUpperCase == str.substring(0, 1))
	}

	def relationFeatures(tokens: Array[RelationToken], idx1: Int, idx2: Int): Array[String] = {
		val feats = new ArrayBuffer[String]
		val a1 = tokens(idx1)
		val a2 = tokens(idx2)
		val dist = idx2-idx1
		
		feats += "BIAS"
		
		val cap1 = if (capitalized(a1.word)) "UC" else "LC"
		val cap2 = if (capitalized(a2.word)) "UC" else "LC"
		feats += "CAP1-%s".format(cap1)
		feats += "CAP2-%s".format(cap2)
		feats += "CAPB-%s-%s".format(cap1, cap2)
		
		feats += "WORD1-%s".format(a1.word)
		feats += "POS1-%s".format(a1.pos)
		feats += "WORD1-%s-POS1-%s".format(a1.word, a1.pos)
		for (w <- a1.word.split("_")) {
			feats += "CONTAINS1-%s".format(w)
		}
		
		feats += "WORD2-%s".format(a2.word)
		feats += "POS2-%s".format(a2.pos)
		feats += "WORD2-%s-POS2-%s".format(a2.word, a2.pos)
		for (w <- a2.word.split("_")) {
			feats += "CONTAINS2-%s".format(w)
		}
		
		feats += "WORD1-%s-WORD2-%s".format(a1.word, a2.word)
		feats += "POS1-%s-POS2-%s".format(a1.pos, a2.pos)
		feats += "WORD1-%s-WORD2-%s-POS1-%S".format(a1.word, a2.word, a1.pos)
		feats += "WORD1-%s-WORD2-%s-POS2-%S".format(a1.word, a2.word, a2.pos)
		feats += "WORD1-%s-POS1-%s-POS2-%s".format(a1.word, a1.pos, a2.pos)
		feats += "WORD2-%s-POS1-%s-POS2-%s".format(a2.word, a1.pos, a2.pos)
	
/*
		feats += "DE1-%s-E1-%s".format(a1.elabel, a2.elabel)
		feats += "DES1-%s-ES1-%s".format(a1.esublabel, a2.esublabel)
		feats += "DEM1-%s-EM1-%s".format(a1.mlabel, a2.mlabel)
*/		
		
		feats += "IDX1-%d".format(idx1)
		feats += "IDX1-%d".format(idx2)
		feats += "DIST-%d".format(dist)
		feats += "POS1-%s-POS2-%s-%d".format(a1.pos, a2.pos, dist)
		return feats.toArray
	}	
	
	def dependencyFeatures(otokens: Array[RelationToken], ohead: Int, odep: Int): Array[String] = {
		val feats = new ArrayBuffer[String]

		val tokens = Array(stoken) ++ otokens ++ Array(etoken)
		val head = ohead + 1
		val dep  = odep + 1

		val dir  = if (dep > head) "R" else "L"
		val dist = Math.abs(head - dep - 1) 
		val htoken = tokens(head)
		val dtoken = tokens(dep)

		val small = if (dep < head) dep else head
		val large = if (dep > head) dep else head 

		feats += "1,3:%s,%s;".format(htoken.word, htoken.pos)
		feats += "1,3:%s,%s;&%s%d".format(htoken.word, htoken.pos, dir, dist)
		feats += "1,3:%s,%s;%s,%s".format(htoken.word, htoken.pos, dtoken.word, dtoken.pos)
		feats += "1,3:%s,%s;%s,%s;&%s%d".format(htoken.word, htoken.pos, dtoken.word, dtoken.pos, dir, dist)
		feats += "1,3:%s,%s;%s".format(htoken.word, htoken.pos, dtoken.word)
		feats += "1,3:%s,%s;%s;&%s%d".format(htoken.word, htoken.pos, dtoken.word, dir, dist)
		feats += "1,3:%s,%s;%s".format(htoken.word, htoken.pos, dtoken.pos)
		feats += "1,3:%s,%s;%s;&%s%d".format(htoken.word, htoken.pos, dtoken.pos, dir, dist)

		feats += "adj3:%s,%s,%s,%s,%s".format("", "", tokens(small+1).pos, tokens(large-1).pos, tokens(large).pos)
		feats += "adj3:%s,%s,%s,%s,%s&%s%d".format("", "", tokens(small+1).pos, tokens(large-1).pos, tokens(large).pos, dir, dist)
		feats += "adj3:%s,%s,%s,%s,%s,%s".format("", tokens(small).pos, "", "", tokens(large).pos, tokens(large+1).pos)
		feats += "adj3:%s,%s,%s,%s,%s,%s&%s%d".format("", tokens(small).pos, "", "", tokens(large).pos, tokens(large+1).pos, dir, dist)
		feats += "adj3:%s,%s,%s,%s,%s".format("", tokens(small).pos, "", tokens(large-1).pos, tokens(large).pos)
		feats += "adj3:%s,%s,%s,%s,%s&%s%d".format("", tokens(small).pos, "", tokens(large-1).pos, tokens(large).pos, dir, dist)
		feats += "adj3:%s,%s,%s,%s,%s".format("", tokens(small).pos, tokens(small+1).pos, "", tokens(large).pos)
		feats += "adj3:%s,%s,%s,%s,%s&%s%d".format("", tokens(small).pos, tokens(small+1).pos, "", tokens(large).pos, dir, dist)
		feats += "adj3:%s,%s,%s,%s,%s,%s".format("", tokens(small).pos, tokens(small+1).pos, "", tokens(large).pos, tokens(large+1).pos)
		feats += "adj3:%s,%s,%s,%s,%s,%s&%s%d".format("", tokens(small).pos, tokens(small+1).pos, "", tokens(large).pos, tokens(large+1).pos, dir, dist)
		feats += "adj3:%s,%s,%s,%s,%s,%s".format("", tokens(small).pos, tokens(small+1).pos, tokens(large-1).pos, "", "")
		feats += "adj3:%s,%s,%s,%s,%s,%s&%s%d".format("", tokens(small).pos, tokens(small+1).pos, tokens(large-1).pos, "", "", dir, dist)
		feats += "adj3:%s,%s,%s,%s,%s,%s".format("", tokens(small).pos, tokens(small+1).pos, tokens(large-1).pos, tokens(large).pos, "")
		feats += "adj3:%s,%s,%s,%s,%s,%s&%s%d".format("", tokens(small).pos, tokens(small+1).pos, tokens(large-1).pos, tokens(large).pos, "", dir, dist)
		feats += "adj3:%s,%s,%s,%s,%s".format(tokens(small-1).pos, "", "", tokens(large).pos, tokens(large+1).pos)
		feats += "adj3:%s,%s,%s,%s,%s&%s%d".format(tokens(small-1).pos, "", "", tokens(large).pos, tokens(large+1).pos, dir, dist)
		feats += "adj3:%s,%s,%s,%s,%s,%s".format(tokens(small-1).pos, tokens(small).pos, "", "", "", tokens(large+1).pos)
		feats += "adj3:%s,%s,%s,%s,%s,%s&%s%d".format(tokens(small-1).pos, tokens(small).pos, "", "", "", tokens(large+1).pos, dir, dist)
		feats += "adj3:%s,%s,%s,%s,%s".format(tokens(small-1).pos, tokens(small).pos, "", "", tokens(large).pos)
		feats += "adj3:%s,%s,%s,%s,%s&%s%d".format(tokens(small-1).pos, tokens(small).pos, "", "", tokens(large).pos, dir, dist)
		feats += "adj3:%s,%s,%s,%s,%s,%s".format(tokens(small-1).pos, tokens(small).pos, "", "", tokens(large).pos, tokens(large+1).pos)
		feats += "adj3:%s,%s,%s,%s,%s,%s&%s%d".format(tokens(small-1).pos, tokens(small).pos, "", "", tokens(large).pos, tokens(large+1).pos, dir, dist)
		feats += "adj3:%s,%s,%s,%s,%s".format(tokens(small-1).pos, tokens(small).pos, "", tokens(large-1).pos, tokens(large).pos)
		feats += "adj3:%s,%s,%s,%s,%s&%s%d".format(tokens(small-1).pos, tokens(small).pos, "", tokens(large-1).pos, tokens(large).pos, dir, dist)

		return feats.toArray
	}
	
	def dconnect(tokens: Array[RelationToken], ohead: Int, odep1: Int, odep2: Int): Array[String] = {
		val feats = new ArrayBuffer[String]
		
//		val stoken = new RelationToken("LEFFT_W", "LEFT_L", "LEFT_P", "LEFT_CP")
//		val etoken = new RelationToken("RIGHT_W", "RIGHT_L", "RIGHT_P", "RIGHT_CP")
//		val tokens = Array(stoken) ++ otokens ++ Array(etoken)
		val head = ohead //+ 1
		val dep1  = odep1 //+ 1
		val dep2 = odep2 //+ 1
		val htoken = tokens(head)
		val dtoken1 = tokens(dep1)
		val dtoken2 = tokens(dep2)
		val cap1 = capitalized(htoken.word)
		val cap2 = capitalized(dtoken1.word)
		val cap3 = capitalized(dtoken2.word)
		
		val dir1  = if (dep1 > head) "R" else "L"
		val dir2  = if (dep2 > head) "R" else "L"
		val dist1 = Math.abs(head - dep1 - 1) 
		val dist2 = Math.abs(head - dep2 - 1) 
		
		feats += "DIST1-2-%d".format(dist1)
		feats += "DIR1-2-%s".format(dir1)
		feats += "DIST1-3-%d".format(dist2)
		feats += "DIR1-3-%s".format(dir2)
		feats += "DIR1-2-%s-DIR1-3-%s".format(dir1, dir2)
		
		feats += "POS1-%s-POS2-%s-POS3-%s".format(htoken.pos, dtoken1.pos, dtoken2.pos)
		feats += "POS1-%s-POS2-%s".format(htoken.pos, dtoken1.pos)
		feats += "POS1-%s-POS3-%s".format(htoken.pos, dtoken2.pos)
		feats += "CAP1-%s-CAP2-%s".format(cap1, cap2)
		feats += "CAP1-%s-CAP3-%s".format(cap1, cap3)
		feats += "CAP1-%s-CAP2-%s-CAP3-%s".format(cap1, cap2, cap3)
		feats += "HWORD-%s".format(htoken.word)
		feats += "HPOS-%s".format(htoken.pos)
		feats += "HWORD-%s-%s".format(htoken.word, htoken.pos)
		for (w <- dtoken1.word.split("_")) {
			feats += "DWORDS1-%s".format(w)			
		}
		for (w <- dtoken2.word.split(" ")) {
			feats += "DWORDS2-%s".format(w)			
		}
		feats += "HW-%s-E1-%s-E2-%s".format(htoken.word, dtoken1.elabel, dtoken2.elabel)
		feats += "HW-%s-EM1-%s-EM2-%s".format(htoken.word, dtoken1.mlabel, dtoken2.mlabel)

		feats += "HPOS-%s-E1-%s-E2-%s".format(htoken.pos, dtoken1.elabel, dtoken2.elabel)
		feats += "HPOS-%s-ES1-%s-ES2-%s".format(htoken.pos, dtoken1.esublabel, dtoken2.esublabel)
		feats += "HPOS-%s-EM1-%s-EM2-%s".format(htoken.pos, dtoken1.mlabel, dtoken2.mlabel)
		feats += "E1-%s-E1-%s".format(dtoken1.elabel, dtoken2.elabel)
		feats += "ES1-%s-ES1-%s".format(dtoken1.esublabel, dtoken2.esublabel)
		feats += "EM1-%s-EM1-%s".format(dtoken1.mlabel, dtoken2.mlabel)
		
//		feats += "W1-%s-W2-%s".format(htoken.word, dtoken1.word)
//		feats += "W1-%s-W3-%s".format(htoken.word, dtoken2.word)	
		feats.toArray
	}
	
	def cconnect(tokens: Array[RelationToken], start: Int, end: Int): Array[String] = {
		val feats = new ArrayBuffer[String]
		val ltoken = tokens(start)
		val rtoken = tokens(end-1)
		val width = end-start
		
		feats += "LPOS-%s".format(ltoken.pos)
		feats += "RPOS-%s".format(rtoken.pos)
		feats += "LPOS-%s-RPOS-%s".format(ltoken.pos, rtoken.pos)
		feats += "width-%d".format(width)
		feats += "LPOS-%s-RPOS-%s-width-%d".format(ltoken.pos, rtoken.pos, width)
		
		feats += "E1-%s-E1-%s".format(ltoken.elabel, rtoken.elabel)
		feats += "ES1-%s-ES1-%s".format(ltoken.esublabel, rtoken.esublabel)
		feats += "EM1-%s-EM1-%s".format(ltoken.mlabel, rtoken.mlabel)
		
		return feats.toArray
	}
	
	def constituencyFeatures(ttokens: Array[RelationToken], start: Int, end: Int, window: Int = 2): Array[String] = {
		val feats = new ArrayBuffer[String]
		val tokens = pad(ttokens, window, window)
		val si = start + window
		val ei = end + window - 1
		val width = end - start
		
		val stoken = tokens(si)
		val etoken = tokens(ei)
		val sstoken = tokens(si-1)
		val eetoken = tokens(ei+1)
		val sitoken = tokens(si+1)
		val eitoken = tokens(ei-1)
		
		feats += "SPAN_SIZE-%d".format(end - start)
		feats += "SPAN_START-%d".format(start)   
		feats += "SPAN_END-%d".format(end)		
		feats ++= getUnigramFeatures(stoken, "START")
		feats ++= getUnigramFeatures(etoken, "END")
		feats ++= getUnigramFeatures(sstoken, "PREV")
		feats ++= getUnigramFeatures(eetoken, "SUCC")		
		feats ++= getUnigramFeatures(stoken, "STARTLEN_%d".format(width))
		feats ++= getUnigramFeatures(etoken, "ENDLEN_%d".format(width))
		feats ++= getUnigramFeatures(sstoken, "SSTARTLEN_%d".format(width))
		feats ++= getUnigramFeatures(eetoken, "EENDLEN_%d".format(width))
		
		feats += "BIGRAM-0-0-%s-%s".format(stoken.word, etoken.word)
		feats += "BIGRAM-0-0-%s-%s".format(stoken.word, etoken.pos)
		feats += "BIGRAM-0-0-%s-%s".format(stoken.word + stoken.pos, etoken.word + etoken.pos)

		feats += "BIGRAM-1-1-%s-%s".format(sstoken.word, eetoken.word)
		feats += "BIGRAM-1-1-%s-%s".format(sstoken.word, eetoken.pos)
		feats += "BIGRAM-1-1-%s-%s".format(sstoken.word + sstoken.pos, eetoken.word + eetoken.pos)

		feats += "BIGRAM-X1-X1-%s-%s".format(stoken.pos + sstoken.pos, etoken.pos + eetoken.pos)
		feats += "BIGRAM-Y1-Y1-%s-%s".format(stoken.pos + sitoken.pos, etoken.pos + eitoken.pos)
		feats += "BIGRAM-Z1-Z1-%s-%s".format(stoken.pos + sstoken.pos + sitoken.pos, etoken.pos + eetoken.pos + eitoken.pos)

		feats += "is-E-%s-is-E-%s".format(stoken.elabel, etoken.elabel)
		
		if (ei - si < 10) {
			tokens.slice(si, ei).map(_.pos).mkString("-")
			for (i <- si to ei) {
				feats += "has-%s".format(tokens(i).pos)
			}
		}
		feats.toArray
	}
	
	def getUnigramFeatures(token: RelationToken, label: String) : Array[String] = {
		val features = new ArrayBuffer[String]
		features += "%s_WORD-%s".format(label, token.word)
		features += "%s_POS-%s".format(label, token.pos)
		features += "%s_CAP-%s".format(label, capitalized(token.word))
		features += "%s_WORDPOS-%s_%s".format(label, token.word, token.pos)
		return features.toArray
	}
	
	
		def pad(array: Array[RelationToken], spad: Int, epad: Int): Array[RelationToken] = {
			val buffer = new ArrayBuffer[RelationToken]
			val end = spad + epad + array.size
			for (i <- 0 until end){
				if (i < spad){
					buffer += RelationToken("[START%d]".format(spad-i), "[START%d]".format(spad-i), "STARTPOS", "STARTPOS", "STARTMORPH")
				}
				else if (i >= array.size + spad){
					buffer += RelationToken("[END%d]".format(1 + i - (array.size + spad)), "[START%d]".format(spad-i), "ENDPOS", "ENDPOS", "ENDMORPH")
				}
				else{
					buffer += array(i-spad)	
				}	
			}
			return buffer.toArray
		}
}



