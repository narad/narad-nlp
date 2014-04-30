package narad.nlp.ner
import java.io.FileWriter
import narad.io.onto._
import narad.nlp.ling.{Lexicon}
import scala.collection.mutable.ArrayBuffer
import narad.util.RegexPatterns
import narad.nlp.parser.constituent.ConstituentParserParams
import math._
import narad.nlp.ling.{TaggedToken => Token}

trait NamedEntityFeatures {


  def nerFeatures(tokens: Array[Token], i: Int, j: Int, mode: String="SPAN"): Array[String] = {
    mode match {
      case "LINEAR" => getLinearChainNERFeatures(tokens, i, j)
      case _=> getSpanNERFeatures(tokens, i, j)
    }
  }

  def getSpanNERFeatures(tokens: Array[Token], i: Int, j: Int): Array[String] = {
  val feats = new ArrayBuffer[String]
  val width = j-i
  val words = tokens.map(_.word)
  val tags  = tokens.map(_.pos)
    val dictionaries = Array[Lexicon]() // ********* Instantiate
    feats += "[ner-bias]"
    feats += "[ner-words]-%s".format(words.slice(i,j).mkString("-"))
    feats += "[ner-tags]-%s".format(tags.slice(i,j).mkString("-"))
    feats += "[ner-caps]-%s".format(words.slice(i,j).map(isCapitalized2Str(_)).mkString("-"))
    feats += "[ner-words-tags]-%s".format(tokens.slice(i,j).map(p => p.word + "_" + p.pos).mkString("-"))
    feats += "[ner-width]-%d".format(width)
    feats += "[ner-start_pos-end_pos]-%s-%s".format(tags(i), tags(j-1))

    feats ++= unigramFeatures(tokens(i), dictionaries).map("[ner-start]-%s".format(_))
    feats ++= unigramFeatures(tokens(j-1), dictionaries).map("[ner-end]-%s".format(_))
    if (i > 0) {
      feats ++= unigramFeatures(tokens(i-1), dictionaries).map("[ner-before]-%s".format(_))
    }
    else {
      feats += "[ner-no-tok-before]"
    }
    if (j < tokens.size) {
      feats ++= unigramFeatures(tokens(j), dictionaries).map("[ner-after]-%s".format(_))
    }
    else {
      feats += "[ner-no-tok-after]"
    }
    feats.toArray
  }

  def getLinearChainNERFeatures(tokens: Array[Token], i: Int, j: Int): Array[String] = {
    val feats = new ArrayBuffer[String]
    val dictionaries = Array[Lexicon]() // ********* Instantiate
    feats += "[ner-bias]"
    for (k <- i until j) {
      feats ++= unigramFeatures(tokens(k), dictionaries).map("[ner]-%s".format(_))
    }
    feats.toArray
  }

  def unigramFeatures(token: Token, dictionaries: Array[Lexicon]): Array[String] = {
    val feats = new ArrayBuffer[String]
    val word = token.word
    val pos = token.pos
    feats += "POS-%s".format(pos)
    feats += "w-%s".format(word)
    feats += "w-%s-t-%s".format(word, pos)
    feats += "lower-%s".format(word.toLowerCase)
    feats += "simple-%s".format(simplify(word))
    feats += "ws1-%s".format(wordshape(word, 0))
    feats += "ws2-%s".format(wordshape(word, 1))

    //		    # might want to normalize digits before running char ngrams
    //		    for c in char_ngrams(W, 2, 5):
    //		        yield 'ngram-%s' % c
    //		for
    val wlen      = word.size
    val ngramStart = 2
    val ngramEnd   = 5
    for (split <- ngramStart to ngramEnd) {
      if (split <= wlen) {
        feats += "ngram-start-^%s".format(word.substring(0,split))
      }
    }
    for (split <- ngramStart to ngramEnd) {
      if (wlen-split >= 0) {
        feats += "ngram-end-%s$".format(word.substring(wlen-split,wlen))
      }
    }
    if (word.substring(0,1).toUpperCase == word.substring(0,1)) feats += "[is-capitalized]"
    if (word.toUpperCase == word) feats += "[is-all-caps]"
    if (word.toLowerCase == word) feats += "[is-all-lowercase]"
    if (word matches "^[,\\.;:?!()]+$") feats += "[is-punct]"
    if (word matches "[,\\.;:?!()]") feats += "[contains-punct]"
    feats.toArray
  }

  def connectionFeatures(tokens: Array[Token], start: Int, end: Int): Array[String] = {
    val sent = tokens
    val features = new ArrayBuffer[String]
    val width = end - start

    features += "AGREE"

    features += "AGREE_ALL_TAGS-%s".format(sent.slice(start, end).map(_.pos).mkString("-"))

   features += "AGREE_SPAN_WIDTH-%d".format(width)

    features += "AGREE_START_WORD-%s".format(sent(start).word)
    features += "AGREE_START_POS-%s".format(sent(start).pos)
    features += "AGREE_START_WORDPOS-%s-%s".format(sent(start).word, sent(start).pos)
    features += "AGREE_START_CAP-%s".format(isCapitalized2Str(sent(start).word))

    features += "AGREE_END_WORD-%s".format(sent(end-1).word)
    features += "AGREE_END_POS-%s".format(sent(end-1).pos)
    features += "AGREE_END_WORDPOS-%s-%s".format(sent(end-1).word, sent(end-1).pos)
    features += "AGREE_END_CAP-%s".format(isCapitalized2Str(sent(end-1).word))

    features += "AGREE_START_END_POS-%s_%s".format(sent(start).pos, sent(end-1).pos)

    if (width < 10) {
      features += "AGREE_ALL_TAGS-%s".format(sent.slice(start, end).map(_.pos).mkString("-"))
      features += "AGREE_ALL_CAPS-%s".format(sent.slice(start, end).map(t => isCapitalized2Str(t.word)).mkString("-"))
    }
    if (width > 5) {
      for (w <- 1 to 3) {
        features += "AGREE_FIRST_WORDS-%d-%s".format(w, tokens.slice(start, start+w).map(_.word))
        features += "AGREE_FIRST_TAGS-%d-%s".format(w, tokens.slice(start, start+w).map(_.pos))
      }
      for (w <- 1 to 3) {
        features += "AGREE_LAST_WORDS-%d-%s".format(w, tokens.slice(end-w, end).map(_.word))
        features += "AGREE_LAST_TAGS-%d-%s".format(w, tokens.slice(end-w, end).map(_.pos))
      }
    }
    features.toArray
  }

	def wordshape(str: String, rep: Int): String = {
		replaceRepitition(str.map { c =>
			if (c.toUpper == c) "A"
			else if (c.toUpper == c) "a"
			else if (c.toString matches "[0-9]") "1"
			else if (c == ' ') " "
			else c
		}.mkString(""), rep) 
	}

	def replaceRepitition(str: String, allow: Int): String = {
		val buff = new StringBuilder
		var count = 0
		var last = ' '
		for (c <- str) {
			if (c == last){
				count += 1
				if (count <= allow){
					buff += c
				}
			}
			else {
				count = 0
				last = c
				buff += c
			}
		}
		buff.toString()
	}

	def simplify(str: String): String = str match {
		case RegexPatterns.NumericPattern => "NUMERIC"
		case RegexPatterns.DayPattern => "DAY"
		case RegexPatterns.MonthPattern => "MONTH"
		case RegexPatterns.OrdinalPattern => "ORDINAL"
		case RegexPatterns.TimePattern => "TIME"
		case RegexPatterns.DirectionPattern => "CARDINAL-DIRECTION"
		case _=> {
			if (str == "A.D." || str == "B.C.") return "ADBC"
			if (str == ",") return "COMMA"				
			return str.toLowerCase.replaceAll("[0-9]", "#").replaceAll("[,\\.;:?!()]", "@")
		}
	}

  def isCapitalized2Str(str: String) = str.substring(0,1).toUpperCase == str.substring(0,1)

  def constituentSpanFeaturesForNER(otokens: Array[Token], start: Int, end: Int): Array[String] = {
    val window = 3
    val lexicalWindow = 2
    val mode = 2
    val features = new ArrayBuffer[String]
    val si = start + window
    val ei = end + window - 1
    val width = end-start
    val tokens = pad2(otokens, window, window)
    val words = tokens.map(_.word)
    val tags = tokens.map(_.pos)
    val slen = otokens.size

    // Simple features
    features += "[span-bias]"
    features += "[spanwidth]-%d".format(width)
    features += "[span-start]-%d".format(start)
    features += "[span-end]-%d".format(end)
    features += "[span-start-end-width]-%d-%d-%d".format(start, end, width)
    if (width == slen) features += "[top-span]"
    if (start == 0)  features += "[start-span]"
    if (end == slen) features += "[end-span]"

    // Unigram Features
    val startToken = tokens(si)
    val startWord = startToken.word.toLowerCase
    val startTag  = startToken.pos
    features += "[start-word]-%s".format(startWord)
    features += "[start-tag]-%s".format(startTag)
    features += "[start-cap]-%s".format(isCapitalized2(startToken.word))
    features += "[start-word-tag]-%s-%s".format(startWord, startTag)

    val endToken = tokens(ei)
    val endWord  = endToken.word.toLowerCase
    val endTag   = endToken.pos
    features += "[end-word]-%s".format(endWord)
    features += "[end-tag]-%s".format(endTag)
    features += "[end-cap]-%s".format(isCapitalized2(endToken.word))
    features += "[end-word-tag]-%s-%s".format(endWord, endTag)

    val prevToken = tokens(si-1)
    val prevWord  = prevToken.word.toLowerCase
    val prevTag   = prevToken.pos
    features += "[prev-word]-%s".format(prevWord)
    features += "[prev-tag]-%s".format(prevTag)
    features += "[prev-cap]-%s".format(isCapitalized2(prevToken.word))
    features += "[prev-word-tag]-%s-%s".format(prevWord, prevTag)

    val postToken = tokens(ei+1)
    val postWord  = postToken.word.toLowerCase
    val postTag   = postToken.pos
    features += "[post-word]-%s".format(postWord)
    features += "[post-tag]-%s".format(postTag)
    features += "[post-cap]-%s".format(isCapitalized2(postToken.word))
    features += "[post-word-tag]-%s-%s".format(postWord, postTag)

    // Bigram Features
    features += "[start-word-&&-end-word]-%s-%s".format(startWord, endWord)
    features += "[start-word-&&-end-tag]-%s-%s".format(startWord, endTag)
    features += "[start-tag-&&-end-word]-%s-%s".format(startTag, endWord)
    features += "[start-tag-&&-end-tag]-%s-%s".format(startTag, endTag)

    // Tag Concatenation Features
    for (i <- 1 until min(width, window)) {
      features += "[tags-from-start-%d]-%s".format(i+1, tags.slice(si,si+i+1).mkString("-"))
      if (i <= lexicalWindow) {
        features += "[words-from-start-%d]-%s".format(i+1, words.slice(si,si+i+1).mkString("-"))
      }
    }
    for (i <- 1 until window) {
      features += "[tags-from-end-%d]-%s".format(i+1, tags.slice(ei,ei+i+1).mkString("-"))
      if (i <= lexicalWindow) {
        features += "[words-from-end-%d]-%s".format(i+1, words.slice(ei,ei+i+1).mkString("-"))
      }
    }

    for (i <- 1 until window) {
      features += "[tags-upto-start-%d]-%s".format(i+1, tags.slice(si-i,si+1).mkString("-"))
      if (i <= lexicalWindow) {
        features += "[words-upto-start-%d]-%s".format(i+1, words.slice(si-i,si+1).mkString("-"))
      }
    }
    for (i <- 1 until min(width, window)) {
      features += "[tags-upto-end-%d]-%s".format(i+1, tags.slice(ei-i,ei+1).mkString("-"))
      if (i <= lexicalWindow) {
        features += "[words-upto-end-%d]-%s".format(i+1, words.slice(ei-i,ei+1).mkString("-"))
      }
    }
    // CCM Features
    features += "[outside-1-tags]-%s-&&-%s".format(tags(si-1), tags(ei+1))
    features += "[outside-2-tags]-%s-&&-%s".format(tags.slice(si-2, si).mkString("-"), tags.slice(ei+1, ei+1+2).mkString("-"))
    features += "[outside-3-tags]-%s-&&-%s".format(tags.slice(si-3, si).mkString("-"), tags.slice(ei+1, ei+1+3).mkString("-"))
    features += "[outside-1-words]-%s-&&-%s".format(words(si-1), words(ei+1))
    features += "[outside-2-words]-%s-&&-%s".format(words.slice(si-2, si).mkString("-"), words.slice(ei, ei+2).mkString("-"))

    if (mode == 3) {
      if (width >= 2) features += "[inside-1-tags]-%s-&&-%s".format(tags.slice(si, si+1).mkString("-"), tags.slice(ei, ei+1).mkString("-"))
      if (width >= 4) features += "[inside-2-tags]-%s-&&-%s".format(tags.slice(si, si+2).mkString("-"), tags.slice(ei-1, ei+1).mkString("-"))
      if (width >= 6) features += "[inside-3-tags]-%s-&&-%s".format(tags.slice(si, si+3).mkString("-"), tags.slice(ei-2, ei+1).mkString("-"))
    }
    if (width <= 10) features += "[inside-all-pos]-%s".format(tags.slice(si, ei+1).mkString("-"))

    if (mode == 3) {
      if (width >= 2) features += "[inside-1-words]-%s-&&-%s".format(words.slice(si, si+1).mkString("-"), words.slice(ei, ei+1).mkString("-"))
      if (width >= 4) features += "[inside-2-words]-%s-&&-%s".format(words.slice(si, si+2).mkString("-"), words.slice(ei-1, ei+1).mkString("-"))
      if (width >= 6) features += "[inside-3-words]-%s-&&-%s".format(words.slice(si, si+3).mkString("-"), words.slice(ei-2, ei+1).mkString("-"))
      if (width <= 5) features += "[inside-all-words]-%s".format(words.slice(si, ei+1).mkString("-"))
    }

    // "Footprint" Tag Features
    features += "[start-footprint-1-1]-%s".format(tags.slice(si-1, si+2).mkString("-"))
    features += "[start-footprint-1-2]-%s".format(tags.slice(si-1, si+3).mkString("-"))
    features += "[start-footprint-2-1]-%s".format(tags.slice(si-2, si+2).mkString("-"))
    features += "[start-footprint-2-2]-%s".format(tags.slice(si-2, si+3).mkString("-"))
    features += "[start-footprint-3-3]-%s".format(tags.slice(si-3, si+4).mkString("-"))

    features += "[end-footprint-1-1]-%s".format(tags.slice(ei-1, ei+2).mkString("-"))
    features += "[end-footprint-1-2]-%s".format(tags.slice(ei-1, ei+3).mkString("-"))
    features += "[end-footprint-2-1]-%s".format(tags.slice(ei-2, ei+2).mkString("-"))
    features += "[end-footprint-2-2]-%s".format(tags.slice(ei-2, ei+3).mkString("-"))
    features += "[end-footprint-3-3]-%s".format(tags.slice(ei-3, ei+4).mkString("-"))

    features += "[bigram-footprint-3-3]-%s-&&-%s".format(tags.slice(si-1, si+2).mkString("-"),
      tags.slice(ei-1, ei+2).mkString("-"))
    if (mode == 3) {
      features += "[bigram-footprint-5-5]-%s-&&-%s".format(tags.slice(si-2, si+3).mkString("-"),
        tags.slice(ei-2, ei+3).mkString("-"))
    }


    // Verb-Pairing Features
    if (mode == 3) {
      for (i <- si+1 to ei) {
        if (tokens(i).pos.startsWith("V")) {
          //        features += "[has-verb]-%s".format(tokens(i).word)
          features += "[has-verb-words]-%s-%s-%s".format(tokens(i).word, startWord, endWord)
        }
      }
    }
    //   println(features.toArray.mkString(" "))
    features.toArray
  }

  def pad2(array: Array[Token], spad2: Int, epad2: Int): Array[Token] = {
    val START_POS = "$"
    val END_POS = "&"
    val buffer = new ArrayBuffer[Token]
    val end = spad2 + epad2 + array.size
    for (i <- 0 until end){
      if (i < spad2){
        buffer += Token("[START%d]".format(spad2-i), START_POS)
      }
      else if (i >= array.size + spad2){
        buffer += Token("[END%d]".format(1 + i - (array.size + spad2)), END_POS)
      }
      else{
        buffer += array(i-spad2)
      }
    }
    buffer.toArray
  }

  def isCapitalized2(str: String): Boolean = {
    val letter = str.substring(0,1)
    letter == letter.toUpperCase
  }
}






































/*
    for (dict <- dictionaries) {
      if (dict.contains(word.toLowerCase)) {
        feats + "[dict-%s]".format(dict.name)
      }
    }
*/

//.replaceAll("(.+)", "$1").replaceAll"(\b\w+\b)\s+(\1(\s+|$))+", "$1")

/*
    for (idx <- i until j) {
      feats ++= unigramFeatures(tokens(idx), dictionaries)
    }
    feats ++= unigramFeatures(tokens(i), dictionaries).map("[start]-%s".format(_))
    feats ++= unigramFeatures(tokens(j-1), dictionaries).map("[end]-%s".format(_))
    if (i > 0) {
      feats ++= unigramFeatures(tokens(i-1), dictionaries).map("[before]-%s".format(_))
    }
    else {
      feats += "[no-tok-before]"
    }
    if (j < tokens.size) {
      feats ++= unigramFeatures(tokens(j), dictionaries).map("[after]-%s".format(_))
    }
    else {
      feats += "[no-tok-after]"
    }
    */

//    features += "AG_START_CPOS-%s".format(sent(start).pos.substring(0,1))
//    features += "AG_START_WORDCPOS-%s".format(sent(start).word +"_" + sent(start).pos.substring(0,1))
//    features += "AG_END_CPOS-%s".format(sent(end-1).pos.substring(0,1))

//	    features += "END_WORDPOS-%s".format(sent(end-1).word + "_" + sent(end-1).pos)
//	    features += "END_WORDCPOS-%s".format(sent(end-1).word + "_" + sent(end-1).pos.substring(0,1))

//	    features += "STARTEND_WORD-%s_%s".format(sent(start).word, sent(end-1).word)
//    features += "AG_STARTEND_CPOS-%s_%s".format(sent(start).pos.substring(0,1), sent(end-1).pos.substring(0,1))



/*
  def pad2(array: Array[Token], spad2: Int, epad2: Int): Array[Token] = {
    val STARTPOS = "START"
    val ENDPOS = "END"
    val buffer = new ArrayBuffer[Token]
    val end = spad2 + epad2 + array.size
    for (i <- 0 until end){
      if (i < spad2){
        buffer += new Token("[START%d]".format(spad2-i), STARTPOS)
      }
      else if (i >= array.size + spad2){
        buffer += new Token("[END%d]".format(1 + i - (array.size + spad2)), ENDPOS)
      }
      else{
        buffer += array(i-spad2)
      }
    }
    return buffer.toArray
  }
      */



//		feats += "[start]-%d".format(i)
//		feats += "[end]-%d".format(i)

//		if (width == 1) {
//			feats += "[words]-%s".format(words(i))
//			feats += "[tags]-%s".format(tags(i))
//			feats += "[words-tags]-%s".format(words(i), tags(i))
//		}
//		else { // Span is greater than 1
//			feats += "[words]-" + words.slice(i,j).mkString("-")
//			feats += "[tags]-" + tags.slice(i,j).mkString("-")						
//			feats += "[words-tags]-%s".format(tokens.slice(i,j).map(p => p.word + "_" + p.pos).mkString("-"))
//		}
		
/*
		for (feat <- unigramFeatures(tokens(i), dictionaries)) {
			feats += "[start-" + feat
		}
		if (width > 1) {
			for (feat <- unigramFeatures(tokens(j-1), dictionaries)) {
				feats += "[last-" + feat
			}
			feats += "[words]-" + words.slice(i,j).mkString("-")
			feats += "[tags]-" + tags.slice(i,j).mkString("-")			
		}
*/




/// backup for doing things Tim's way
/*
      for (j <- slen to 1 by -1) {
        var labeled = false
        for (i <- scala.math.max(0, j-order) to j-1) {
          val width = j-i
          val feats = nerFeatures(tokens, i, j)
          val correctSpan = if (ner.containsSpan(i,j) || (width == 1 && !ner.coversSpan(i,j))) "+" else ""
          out.write("nerbracket(%d,%d)\t%s%s\n".format(i, j, correctSpan, feats.mkString(" ")))

          if (width == 1) {
            val builder = new StringBuilder()
            for (f <- feats) builder.append("O_" + f)
            if (!ner.containsSpan(i,j) && !ner.coversSpan(i,j)) {
              out.write("nerlabel(%d,%d,1)\t+%s\n".format(i,j, builder.toString.trim))
            }
            else {
              out.write("nerlabel(%d,%d,1)\t%s\n".format(i,j, builder.toString.trim))
            }
          }
          val correctLabel = if ((!ner.containsSpan(i,j) && width > 1) || (width == 1 && ner.coversSpan(i,j))) "+" else ""
          out.write("nerlabel(%d,%d,0)\t%sNone\n".format(i, j, correctLabel))

          for (label <- labels) {
            val builder = new StringBuilder()
            for (f <- feats) builder.append(" " + label + "_" + f)
            val correctLabel = if (ner.containsSpanLabel(i,j,label)) "+" else ""
            out.write("nerlabel(%d,%d,%s)\t%s%s\n".format(i, j, labels.indexOf(label)+2, correctLabel, builder.toString.trim))
          }
        }

 */

