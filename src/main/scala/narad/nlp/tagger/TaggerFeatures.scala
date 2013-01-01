package narad.nlp.tagger

import collection.mutable.{HashMap, ArrayBuffer, Map}
import narad.io.conll.CoNLLDatum
import scala.util.matching.Regex
import narad.bp.util.{Feature, PotentialExample}
import narad.bp.util.index._
import narad.bp.structure.Potential


trait TaggerFeatures {
  val NUMBER_PATTERN = new Regex(".*[0-9]+.*")

  def getFeatures(words: Array[String], tags: Array[String], dict: TagDictionary, hash: Index[String]): PotentialExample = {
    val map  = Map[String, String]()
    val pots = new ArrayBuffer[Potential]
    val featmap = new HashMap[String, Array[Feature]]
    val slen = words.size
    val alltags = dict.all.toArray
    map("slen") = slen.toString
    map("words") = words.mkString(" ")
    map("bigram") = "false"
    for (i <- 1 to slen) {
      val word = words(i)
      val feats = unigramLexicalFeatures(words, i)
      val dtags = if (dict.contains(word)) dict.tags(word).toArray else alltags
      dtags.zipWithIndex.foreach { case(tag, tidx) =>
        val name = "ulabel(%d,%s)".format(i, tag)
        val correct = tags.size > 0 && tag == tags(i)
        pots += new Potential(0.0, name, correct)
        featmap(name) = feats.view.map("%s_%s".format(tag, _)).map(s => new Feature(hash.index(s), 1.0, 0)).toArray
      }
    }
    return new PotentialExample(map, pots.toArray, featmap)
  }

  def extractFeatures(trainFile: String, trainFeatureFile: String, dict: TagDictionary, params: TaggerParams) = {}


  def capitalized(str: String): Boolean = {
    str.toUpperCase == str
  }

  def bigramFeatures(datum: CoNLLDatum, fidx: Int, tidx: Int,
                     useMorph: Boolean, useSyntax: Boolean): Array[String] = {
    val feats = new ArrayBuffer[String]
    val ffeats = unigramFeatures(datum, fidx, offset=0, useMorph=useMorph, useSyntax=useSyntax)
    val tfeats = unigramFeatures(datum, tidx, offset=1, useMorph=useMorph, useSyntax=useSyntax)
    for (ffeat <- ffeats; tfeat <- tfeats) {
      feats += ffeat + "_" + tfeat
    }
    return feats.toArray
  }

  def unigramFeatures(datum: CoNLLDatum, i: Int, offset: Int = 0, useMorph: Boolean, useSyntax: Boolean): Array[String] = {
    val feats = new ArrayBuffer[String]
    val slen = datum.slen
    val word = datum.form(i)

    feats += "[bias]"
    feats += "[form-%d]-%s".format(offset, datum.form(i).toLowerCase)

    val advancedFeats = true
    if (advancedFeats) {
      // Context Features
      for (p <- 1 to 3) {
        if (i-p > 0) {
          feats += "[prev-%d-%s]".format(p, datum.form(i-p))
          feats += "[prev-concat-%d-%s]".format(p, datum.forms.slice(i-p, p).mkString("-"))
        }
        else {
          feats += "[prev-%d-START]".format(p)
        }
        if (i+p <= slen) {
          feats += "[post-%d-%s]".format(p, datum.form(i+p))
          feats += "[post-concat-%d-%s]".format(p, datum.forms.slice(p, p+i).mkString("-"))
        }
        else {
          feats += "[post-%d-END]".format(p)
        }
      }

      // Substring pseudo-morph features
      for (b <- 1 to 4 if word.size >= b) {
        feats += "prefix-%d-%s".format(b, word.substring(0, b))
        feats += "suffix-%d-%s".format(b, word.substring(word.size-b))
      }

      // Contains Number
      feats += "contains-number-%s".format(NUMBER_PATTERN.pattern.matcher(word).matches.toString)
      // Contains Uppercase Character
      feats += "contains-cap-%s".format(word == word.toUpperCase)
      // Contain Hyphen
      feats += "contains-hyphen-%s".format(word.contains("-").toString)
    }

    if (useMorph) {
      val morphs = datum.feat(i)
      var mcount = 0
      feats += "[morphs-%s-%d]-%s".format(datum.form(i), offset, morphs)
      feats += "[morphs-%d]-%s".format(offset, morphs)
      for (morph <- morphs.split("\\|")) {
        feats += "[morph-%s-%d-%d]-%s".format(datum.form(i), offset, mcount, morph)
        feats += "[morph-%d-%d]-%s".format(offset, mcount, morph)
        mcount += 1
      }
    }

    if (useSyntax) {
      val head = datum.head(i)
      if (head == 0) {
        feats += "[head]-root"
      }
      else {
        feats += "[headform]-%s".format(datum.form(head))
        feats += "[headform-childform]-%s-%s".format(datum.form(head), datum.form(i))
        feats += "[headmorphs-childmorphs]-%s-%s".format(datum.feat(head), datum.feat(i))
      }
    }
    return feats.toArray
  }

  def correct(datum: CoNLLDatum, i: Int, mode: String): String = {
    mode match {
      case "COARSE" => datum.cpostag(i)
      case "FINE"   => datum.postag(i)
      case "CASE"   => datum.mcase(i)
      case "GENDER" => datum.mgender(i)
      case "NUMBER" => datum.mnumber(i)
      case "CASE+GENDER+NUMBER" => datum.mcase(i) + "|" + datum.mgender(i) + "|" + datum.mnumber(i)
      case _ => ""
    }
  }












  def bigramLexicalFeatures(words: Array[String], fidx: Int, tidx: Int): Array[String] = {
    val feats = new ArrayBuffer[String]
    val ffeats = unigramLexicalFeatures(words, fidx, offset=0)
    val tfeats = unigramLexicalFeatures(words, tidx, offset=1)
    for (ffeat <- ffeats; tfeat <- tfeats) {
      feats += ffeat + "_" + tfeat
    }
    return feats.toArray
  }

  def unigramLexicalFeatures(words: Array[String], i: Int, offset: Int = 0): Array[String] = {
    val feats = new ArrayBuffer[String]
    val slen = words.size
    val word = words(i)

    feats += "[bias]"
    feats += "[form-%d]-%s".format(offset,words(i).toLowerCase)

    val advancedFeats = true
    if (advancedFeats) {
      // Context Features
      for (p <- 1 to 3) {
        if (i-p >= 0) {
          feats += "[prev-%d-%s]".format(p, words(i)(i-p))
          feats += "[prev-concat-%d-%s]".format(p, words.slice(i-p, p).mkString("-"))
        }
        else {
          feats += "[prev-%d-START]".format(p)
        }
        if (i+p <= slen) {
          feats += "[post-%d-%s]".format(p, words(i+p))
          feats += "[post-concat-%d-%s]".format(p, words.slice(p, p+i).mkString("-"))
        }
        else {
          feats += "[post-%d-END]".format(p)
        }
      }

      // Substring pseudo-morph features
      for (b <- 1 to 4 if word.size >= b) {
        feats += "prefix-%d-%s".format(b, word.substring(0, b))
        feats += "suffix-%d-%s".format(b, word.substring(word.size-b))
      }

      // Contains Number
      feats += "contains-number-%s".format(NUMBER_PATTERN.pattern.matcher(word).matches.toString)
      // Contains Uppercase Character
      feats += "contains-cap-%s".format(word == word.toUpperCase)
      // Contain Hyphen
      feats += "contains-hyphen-%s".format(word.contains("-").toString)
    }
    return feats.toArray
  }
}
























/*
	def lexicalFeatures(token: Token): Array[String] = {
		val feats = new ArrayBuffer[String]
		val word = token.word
		val pos  = token.pos
		feats += "word_%s".format(word)
//		feats += "wordlen_%d".format(word.size)
//		for (i <- 1 to Math.min(4,word.size)) {
//			feats += "suffix_%d_%s".format(i, word.substring(word.size-i))
//		}
		return feats.toArray
	}
}
 */


/*


//      feats += "[capitalized-%d]-%s".format(offset, capitalized(datum.form(i)))
//      feats += "[lemma-%d]-%s".format(offset, datum.lemma(i))
//      feats += "[form-lemma-%d]-%s-%s".format(offset, datum.form(i), datum.lemma(i))

      val window = 3
      for (w <- 1 to window) { //window if (i-w > 0)) {
        try {
          if (w > 1) feats += "[prev-forms-%d]-%s".format(offset, datum.forms.slice(i-w, i-1).mkString("-"))
          feats += "[prev-forms-str-%d]-%s".format(offset, datum.forms.slice(i-w, i).mkString("-"))
        }
        catch {
          case e: Exception => {}
        }
        try {
          if (w > 1) feats += "[post-forms-%d]-%s".format(offset, datum.forms.slice(i, i+w).mkString("-"))
          feats += "[post-forms-str-%d]-%s".format(offset, datum.forms.slice(i, i+w).mkString("-"))
        }
        catch {
          case e: Exception => {}
        }
      }



package narad.nlp.tagger
import narad.io.datum.CoNLLDatum
import scala.collection.mutable.ArrayBuffer

object TaggerFeatures {
	
	def capitalized(str: String): Boolean = {
		return str.toUpperCase == str
	}
	
	def bigramFeatures(datum: CoNLLDatum, fidx: Int, tidx: Int,
										 useMorph: Boolean, useSyntax: Boolean): Array[String] = {
		val feats = new ArrayBuffer[String]
		val ffeats = unigramFeatures(datum, fidx, offset=0, useMorph, useSyntax)
		val tfeats = unigramFeatures(datum, tidx, offset=1, useMorph, useSyntax)
		for (ffeat <- ffeats; tfeat <- tfeats) {
			feats += ffeat + "_" + tfeat
		}
		return feats.toArray
	}

	def unigramFeatures(datum: CoNLLDatum, i: Int, offset: Int = 0, useMorph: Boolean, useSyntax: Boolean): Array[String] = {
		val feats = new ArrayBuffer[String]
		feats += "[form-%d]-%s".format(offset, datum.form(i))	
		feats += "[bias]"
		feats += "[capitalized-%d]-%s".format(offset, capitalized(datum.form(i)))
		feats += "[lemma-%d]-%s".format(offset, datum.lemma(i))
		feats += "[form-lemma-%d]-%s-%s".format(offset, datum.form(i), datum.lemma(i))
		val window = 3
		for (w <- 1 to window) { //window if (i-w > 0)) {
			if (w > 1) feats += "[prev-forms-%d]-%s".format(offset, datum.forms.slice(i-w, i-1).mkString("-"))
			feats += "[prev-forms-str-%d]-%s".format(offset, datum.forms.slice(i-w, i).mkString("-"))			
			try {
				if (w > 1) feats += "[post-forms-%d]-%s".format(offset, datum.forms.slice(i, i+w).mkString("-"))
				feats += "[post-forms-str-%d]-%s".format(offset, datum.forms.slice(i, i+w).mkString("-"))				
			}
			catch {
				case e: Exception => {}
			}
		}
		
		if (useMorph) {
			val morphs = datum.feat(i)
			var mcount = 0
			feats += "[morphs-%s-%d]-%s".format(datum.form(i), offset, morphs)
			feats += "[morphs-%d]-%s".format(offset, morphs)
			for (morph <- morphs.split("\\|")) {
				feats += "[morph-%s-%d-%d]-%s".format(datum.form(i), offset, mcount, morph)
				feats += "[morph-%d-%d]-%s".format(offset, mcount, morph)
				mcount += 1
			}		
		}
		if (useSyntax) {
				val head = datum.head(i)
				if (head == 0) {
//					feats += "[head]-root"
				}
				else {
//					feats += "[headform]-%s".format(datum.form(head))
//					feats += "[headlemma]-%s".format(datum.form(head))
					feats += "[headform-childform]-%s-%s".format(datum.form(head), datum.form(i))
					feats += "[headlemma-childlemma]-%s-%s".format(datum.lemma(head), datum.lemma(i))
					feats += "[headmorphs-childmorphs]-%s-%s".format(datum.feat(head), datum.feat(i))
				}
			}		
			
		return feats.toArray
	}
}
*/

/*	
	def features(datum: CoNLLDatum, idx: Int, window: Int = 2, syntaxFeats: Boolean): Array[String] = {
		val feats = new ArrayBuffer[String]
		val slen = datum.slen
		for (i <- (idx-window) to (idx+window) if (i >= 0 && i <= slen+1)) {
			val offset = i-idx
			if (i > 0 && i <= slen) {
				feats += "[form-%d]-%s".format(offset, datum.form(i))
				feats += "[lemma-%d]-%s".format(offset, datum.lemma(i))
				feats += "[form-lemma-%d]-%s-%s".format(offset, datum.form(i), datum.lemma(i))
				feats += "[idx-%d]-%d".format(offset, idx)
				
//				feats += "[cap-%d]-%s".format(offset, capitalized(datum.form(i)))				
				val morphs = datum.feat(i)
				var mcount = 0
				feats += "[morphs-%s-%d]-%s".format(datum.form(i), offset, morphs)
				feats += "[morphs-%d]-%s".format(offset, morphs)
				for (morph <- morphs.split("\\|")) {
					feats += "[morph-%s-%d-%d]-%s".format(datum.form(i), offset, mcount, morph)
					feats += "[morph-%d-%d]-%s".format(offset, mcount, morph)
					mcount += 1
				}
			}
			else if (i == 0) {
				feats += "[form-%d]-%s".format(offset, "START")				
			}
			else if (i > slen) {
				feats += "[form-%d]-%s".format(offset, "END")
			}
		}

		if (syntaxFeats) {
			val head = datum.head(idx)
			if (head == 0) {
				feats += "[head]-root"
			}
			else {
				feats += "[headform]-%s".format(datum.form(head))
				feats += "[headlemma]-%s".format(datum.form(head))
				feats += "[headform-childform]-%s-%s".format(datum.form(head), datum.form(idx))
				feats += "[headlemma-childlemma]-%s-%s".format(datum.lemma(head), datum.lemma(idx))
				feats += "[headmorphs-childmorphs]-%s-%s".format(datum.feat(head), datum.feat(idx))
			}
		}		
		return feats.toArray
	}
}

*/