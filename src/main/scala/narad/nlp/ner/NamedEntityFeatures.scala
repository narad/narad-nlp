package narad.nlp.ner
import narad.nlp.ling.Lexicon
import narad.nlp.trees._
import scala.collection.mutable.ArrayBuffer
import narad.util.RegexPatterns

object NamedEntityFeatures {

	def extractFeatures(tokens: Array[Token], i: Int, j: Int, dictionaries: Array[Lexicon]): Array[String] = {
		val feats = new ArrayBuffer[String]
		val width = j-i
		val words = tokens.map(_.word)
		val tags  = tokens.map(_.pos)
		feats += "[bias]"
		feats += "[words]-%s".format(words.slice(i,j).mkString("-"))		
		feats += "[tags]-%s".format(tags.slice(i,j).mkString("-"))		
		feats += "[words-tags]-%s".format(tokens.slice(i,j).map(p => p.word + "_" + p.pos).mkString("-"))		
		feats += "[width]-%d".format(width)
		feats += "[start_pos-end_pos]-%s-%s".format(tags(i), tags(j-1))
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
		feats.toArray
	}


	def unigramFeatures(token: Token, dictionaries: Array[Lexicon]): Array[String] = {
		val feats = new ArrayBuffer[String]
		val word = token.word
		val pos = token.pos
		feats += "POS-%s".format(pos)
		feats += "w-%s".format(word)
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
		for (dict <- dictionaries) {
			if (dict.contains(word.toLowerCase)) {
				feats + "[dict-%s]".format(dict.name)
			}
		}
		feats.toArray
	}
	

	def wordshape(str: String, rep: Int): String = {
		replaceRepitition(str.map { c =>
			if (c.toUpperCase == c) "A"
			else if (c.toLowerCase == c) "a"
			else if (c.toString matches "[0-9]") "1"
			else if (c == ' ') " "
			else c
		}.mkString(""), rep) 
	}

	//.replaceAll("(.+)", "$1").replaceAll"(\b\w+\b)\s+(\1(\s+|$))+", "$1")

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
		buff.toString
	}

	def simplify(str: String): String = str match {
		case RegexPatterns.NumericPattern => "NUMERIC"
		case RegexPatterns.DayPattern => "DAY"
		case RegexPatterns.MonthPattern => "MONTH"
		case RegexPatterns.NumericPattern => "WRITTEN-NUMBER"
		case RegexPatterns.OrdinalPattern => "ORDINAL"
		case RegexPatterns.TimePattern => "TIME"
		case RegexPatterns.DirectionPattern => "CARDINAL-DIRECTION"
		case _=> {
			if (str == "A.D." || str == "B.C.") return "ADBC"
			if (str == ",") return "COMMA"				
			return str.toLowerCase.replaceAll("[0-9]", "#").replaceAll("[,\\.;:?!()]", "@")
		}
	}
}










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



