package narad.projects.relmarg
import scala.collection.mutable.ArrayBuffer

object SRLFeatures {
	
	def allcaps(str: String): Boolean = {
		return str.toUpperCase == str
	}
	
	def capitalized(str: String): Boolean = {
		if (str.size == 0) return true
		return (str.substring(0, 1).toUpperCase == str.substring(0, 1))
	}
	
	def argFeatures(idx: Int, tokens: Array[SRLToken], mode: Int = 2): Array[String] = {
			val feats = new ArrayBuffer[String]
			val token = tokens(idx)
			val cap = if (capitalized(token.word)) "iiaUC" else "iiaLC"
			feats += "isArg"
			feats += "ia-word-%s".format(token.word)
			feats += "ia-pos-%s".format(token.pos)
			feats += "ia-word%s-iapos-%s".format(token.word, token.pos)
			feats += "ia-cap-%s".format(cap)
			feats += "ia-cap-%s-%s".format(cap, token.pos)
			
			if (mode == 2) {
				if (idx > 1) {
					val ptoken = tokens(idx-1)
					val pcap = if (capitalized(ptoken.word)) "ipUC" else "ipLC"
					feats += "iak-%s".format(ptoken.word)
					feats += "iak-%s".format(ptoken.pos)
					feats += "iak-%s-%s".format(ptoken.word, ptoken.pos)
					feats == pcap
					feats += "%s-iak%s".format(token.pos,  ptoken.pos)
					feats += "%s-iak%s".format(token.word, ptoken.pos)
					feats += "%s-iak%s".format(token.word, ptoken.word)
					feats += "%s-iak%s".format(cap, pcap)
				}
				if (idx < tokens.size-1) {
					val atoken = tokens(idx+1)
					val acap = if (capitalized(atoken.word)) "iaUC" else "iaLC"
					feats += "iaa-%s".format(atoken.word)
					feats += "iaa-%s".format(atoken.pos)
					feats += "iaa-%s-%s".format(atoken.word, atoken.pos)
					feats += acap
					feats += "%s-iaa%s".format(token.pos,  atoken.pos)
					feats += "%s-iaa%s".format(token.word, atoken.pos)
					feats += "%s-iaa%s".format(token.word, atoken.word)
					feats += "%s-iaa%s".format(cap, acap)
				}
			}
			return feats.map(_.replaceAll("=", "-")).toArray
		}
			
	def senseFeatures(idx: Int, tokens: Array[SRLToken], mode: Int = 1): Array[String] = {
			val feats = new ArrayBuffer[String]
			val token = tokens(idx)
			val slen = tokens.size
			val cap = if (capitalized(token.word)) "UC" else "LC"
			feats += "pred"
			feats += "ps-word-%s".format(token.word)
			feats += "ps-pos-%s".format(token.pos)
			feats += "ps-word%s-pspos-%s".format(token.word, token.pos)
			feats += "pcap-%s".format(cap)

//  Seems to hurt performance
//			feats += "p-morph-%s".format(token.morph)
//			for (f <- token.morph.split("\\|")) {
//				feats += "morph-s-%s".format(f)				
//			}


			if (mode == 2) {  // This only seems to hurt performance as well
				val w = 3
				val lb = if (idx-w < 0) 0 else idx-w
				val ub = if (idx+w > slen-1) slen-1 else idx+w
				for (i <- lb to ub) {
					feats += "ctxt-%s".format(tokens(idx).word)
				}


				if (idx > 1) {
					val ptoken = tokens(idx-1)
					val pcap = if (capitalized(ptoken.word)) "pUC" else "pLC"
					feats += "p-%s".format(ptoken.word)
					feats += "p-%s".format(ptoken.pos)
					feats += "p-%s-%s".format(ptoken.word, ptoken.pos)
					feats == pcap
					feats += "%s-p%s".format(token.pos,  ptoken.pos)
					feats += "%s-p%s".format(token.word, ptoken.pos)
					feats += "%s-p%s".format(token.word, ptoken.word)
					feats += "%s-p%s".format(cap, pcap)
				}
				if (idx < tokens.size-1) {
					val atoken = tokens(idx+1)
					val acap = if (capitalized(atoken.word)) "aUC" else "aLC"
					feats += "a-%s".format(atoken.word)
					feats += "a-%s".format(atoken.pos)
					feats += "a-%s-%s".format(atoken.word, atoken.pos)
					feats += acap
					feats += "%s-a%s".format(token.pos,  atoken.pos)
					feats += "%s-a%s".format(token.word, atoken.pos)
					feats += "%s-a%s".format(token.word, atoken.word)
					feats += "%s-a%s".format(cap, acap)
				}
			}
			return feats.map(_.replaceAll("=", "-")).toArray
		}
	
		
	def predicateFeatures(idx: Int, tokens: Array[SRLToken], mode: Int = 2): Array[String] = {
		val feats = new ArrayBuffer[String]
		val token = tokens(idx)
		val cap = if (capitalized(token.word)) "UC" else "LC"
		feats += "pred"
		feats += "ps-word-%s".format(token.word)
		feats += "ps-pos-%s".format(token.pos)
		feats += "ps-word%s-pspos-%s".format(token.word, token.pos)
		feats += "pcap-%s".format(cap)
		
		if (mode == 2) {
			if (idx > 1) {
				val ptoken = tokens(idx-1)
				val pcap = if (capitalized(ptoken.word)) "pUC" else "pLC"
				feats += "p-%s".format(ptoken.word)
				feats += "p-%s".format(ptoken.pos)
				feats += "p-%s-%s".format(ptoken.word, ptoken.pos)
				feats == pcap
				feats += "%s-p%s".format(token.pos,  ptoken.pos)
				feats += "%s-p%s".format(token.word, ptoken.pos)
				feats += "%s-p%s".format(token.word, ptoken.word)
				feats += "%s-p%s".format(cap, pcap)
			}
			if (idx < tokens.size-1) {
				val atoken = tokens(idx+1)
				val acap = if (capitalized(atoken.word)) "aUC" else "aLC"
				feats += "a-%s".format(atoken.word)
				feats += "a-%s".format(atoken.pos)
				feats += "a-%s-%s".format(atoken.word, atoken.pos)
				feats += acap
				feats += "%s-a%s".format(token.pos,  atoken.pos)
				feats += "%s-a%s".format(token.word, atoken.pos)
				feats += "%s-a%s".format(token.word, atoken.word)
				feats += "%s-a%s".format(cap, acap)
			}
		}
		return feats.map(_.replaceAll("=", "-")).toArray
	}
	

	def argumentFeatures(aidx: Int, pidx: Int, tokens: Array[SRLToken], mode: Int = 2, morph: Boolean = false): Array[String] = {
		val feats = new ArrayBuffer[String]
		val pred = tokens(pidx)
		val arg  = tokens(aidx)
		val dist = Math.abs(aidx - pidx)
		val dir  = if (aidx > pidx) "RIGHT" else if (aidx < pidx) "LEFT" else "SAME"
		
		//if (mode >= 0) 
		feats += "ARG-BIAS"
		if (mode >= 1) {

			feats += "pred-%s-arg-%s".format(pred.word, arg.word)
			feats += "predtag-%s-argtag-%s".format(pred.pos, arg.pos)
			feats += "pred-%s-argtag-%s".format(pred.word, arg.pos)
			feats += "predtag-%s-arg-%s".format(pred.pos, arg.word)
			feats += "pred-%s-arg-%s-predtag-%s-argtag-%s".format(pred.word, arg.word, pred.pos, arg.pos)			

		}

		if (mode >= 2) {
			feats += "predtag-%s-argtag-%s-%d".format(pred.pos, arg.pos, dist)
			feats += "predtag-%s-argtag-%s-%s".format(pred.pos, arg.pos, dir)
			feats += "predtag-%s-%d-%s".format(pred.pos, dist, dir)
			feats += "argtag-%s-%d-%s".format(arg.pos, dist, dir)

//			feats += "predtag-%s-argtag-%s-%d-%s".format(pred.pos, arg.pos, dist, dir)			
		}
		
		if (mode >= 3) {
			feats += "slen-%d".format(tokens.size)
			feats += "dir-%s".format(dir)
			feats += "dist-%d".format(dist)
			feats += "dir-dist-%s-%d".format(dir, dist)

			feats += "pred-%s".format(pred.word)
			feats += "predtag-%s".format(pred.pos)
			feats += "arg-%s".format(arg.word)
			feats += "argtag-%s".format(arg.pos)
		}
		
		if (mode >= 4) {
			val m1s = pred.morph.split("\\|")
			val m2s = arg.morph.split("\\|")
			for (m1 <- m1s; m2 <- m2s) {
				feats += "P-%sxA-%s".format(m1, m2)
			}
		}
		
		return feats.map(_.replaceAll("=", "-")).toArray
	}
	
	
	def dependencyFeatures(tokens: Array[SRLToken], cidx: Int, pidx: Int): Array[String] = {
		val feats = new ArrayBuffer[String]
		val parent = tokens(pidx)
		val child  = tokens(cidx)
		val dist = Math.abs(cidx - pidx)
		val dir  = if (cidx > pidx) "R" else "L"
		feats += "dp%s-%s".format(parent.word, child.word)
		feats += "dp%s-%s".format(parent.pos, child.pos)
		feats += "dp%s-%s".format(parent.word, child.pos)
		feats += "dp%s-%s".format(parent.pos, child.word)
		feats += "dp%s-%s-%s".format(parent.word, child.word, dir)
		feats += "dp%s-%s-%s".format(parent.pos, child.pos, dir)
		feats += "dp%s-%s-%s".format(parent.word, child.pos, dir)
		feats += "dp%s-%s-%s".format(parent.pos, child.word, dir)
		feats += "dp%d".format(dist)
		feats += "dp%s".format(dir)
		feats += "dp%s-%d".format(dir, dist)
		return feats.map(_.replaceAll("=", "-")).toArray
	}

	def connectFeatures(tokens: Array[SRLToken], cidx: Int, pidx: Int): Array[String] = {
		val feats = new ArrayBuffer[String]
		val parent = tokens(pidx)
		val child  = tokens(cidx)
		val dist = Math.abs(cidx - pidx)
		val dir  = if (cidx > pidx) "R" else "L"
		feats += "sli"
		feats += "sli-%s-%s-%s-%s".format(parent.word, child.word, parent.pos, child.pos)
		feats += "sli-%s-%s".format(parent.word, child.word)
		feats += "sli-%s-%s".format(parent.pos, child.pos)
		feats += "sli-%s-%s".format(parent.word, child.pos)
		feats += "sli-%s-%s".format(parent.pos, child.word)
		feats += "sli-%s-%s-%s".format(parent.word, child.word, dir)
		feats += "sli-%s-%s-%s".format(parent.pos, child.pos, dir)
		feats += "sli-%s-%s-%s".format(parent.word, child.pos, dir)
		feats += "sli-%s-%s-%s".format(parent.pos, child.word, dir)
		feats += "sli-%s-%s-%d".format(parent.word, child.pos, dist)
		feats += "sli-%s-%s-%d".format(parent.pos, child.word, dist)
		feats += "sli-%s-%s-%d".format(parent.pos, child.pos, dist)
		feats += "sli-%s-%s-%d-%s".format(parent.pos, child.pos, dist, dir)
		feats += "sli-%d".format(dist)
		feats += "sli-%s".format(dir)
		feats += "sli%s-%d".format(dir, dist)	
		return feats.map(_.replaceAll("=", "-")).toArray
		}
	
		def morphDependency(otokens: Array[SRLToken], ohead: Int, odep: Int, morph: Boolean = true): Array[String] = {
			val feats = new ArrayBuffer[String]

			val stoken = new SRLToken("LEFFT_W", "LEFT_L", "LEFT_P", "LEFT_CP")
			val etoken = new SRLToken("RIGHT_W", "RIGHT_L", "RIGHT_P", "RIGHT_CP")
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

			feats += "2:%s;".format(htoken.lemma)
			feats += "2:%s;&%s%d".format(htoken.lemma, dir, dist)
			feats += "2:%s;1:%s".format(htoken.lemma, dtoken.word, dir, dist)
			feats += "2:%s;1:%s:&%s%d".format(htoken.lemma, dtoken.word, dir, dist)
			feats += "2:%s;1,3:%s,%s".format(htoken.lemma, dtoken.lemma, dtoken.pos)
			feats += "2:%s;1,3:%s,%s;&%s%d".format(htoken.lemma, dtoken.lemma, dtoken.pos, dir, dist)
			feats += "2:%s;1,4:%s,%s;&%s%d".format(htoken.lemma, dtoken.lemma, dtoken.cpos, dir, dist)
			feats += "2:%s;1,4:%s,%s;&%s%d".format(htoken.lemma, dtoken.lemma, dtoken.cpos, dir, dist)
			feats += "2:%s;2:%s".format(htoken.lemma, dtoken.lemma)
			feats += "2:%s;2:%s&%s%d".format(htoken.lemma, dtoken.lemma, dir, dist)
			feats += "2:%s;3:%s".format(htoken.lemma, dtoken.pos)
			feats += "2:%s;3:%s&%s%d".format(htoken.lemma, dtoken.pos, dir, dist)
			feats += "2:%s;4:%s".format(htoken.lemma, dtoken.cpos)
			feats += "2:%s;4:%s&%s%d".format(htoken.lemma, dtoken.cpos, dir, dist)

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

			if (morph) {
				feats += "P-%sX-A-%s".format(htoken.morph, dtoken.morph)
				val m1s = htoken.morph.split("\\|")
				val m2s = htoken.morph.split("\\|")
				for (m1 <- m1s; m2 <- m2s) {
					feats += "S-H-%sxD-%s".format(m1, m2)
				}
			}

			return feats.map(_.replaceAll("=", "-")).toArray
		}
}