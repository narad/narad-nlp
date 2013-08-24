

/*
package narad.nlp.trees
import  narad.nlp.ling.Word
import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet}
import scala.util.matching.Regex
import java.util.Set

case class Token(word: String, pos: String) {
	override def toString(): String = "(%s %s)".format(pos, word)	
	
}

*/
/*
		val punctuationPattern = """[,\\.;:?!()]""".r
		val initialPattern = """"[A-Z]\.""".r
	//	val singlechar = pattern(ALPHA)
	//	val capletter = pattern("[A-Z]")
		val numericPattern = """((?:\d{1,3}(?:\,\d{3})*|\d+)(?:\.\d+)?)""".r
		val timePattern = """(\d\d?) \s*? (?:(\:)? \s*? (\d\d))? \s* ([ap]\.m\.?|[ap]m|[ap])? \s* (?:\(? (GMT|EST|PST|CST)? \)? )? (?:\W|$)""".r

	/*	allcaps = pattern(CAPS + "+")
		initcaps = pattern(CAPS + ".*")

		nonalpha = pattern('[^a-zA-Z]')
		spaces = pattern('\s+')
		lower = pattern('([a-z]+)')
		upper = pattern('([A-Z]+)', False)
		acronym = pattern('[A-Z][A-Z\.]*\.[A-Z\\.]*', False)
	*/

	//	contains_digits = pattern_contains('(\d\d)')
	//	contains_punct = pattern_contains('[!"%&\'()*+,./:;<=>?@[\\]^_`{|}~]')
	//	abbrev  = """([A-Z]?[a-z]+\.)""".r
	//	punct   = """[!"#&\'\(\)/:;<>\?@\[\]\_`{\|}~^]+""".r
	//	alpha   = """[A-Za-z]+""".r
	//	roman   = """(M?M?M?(?:CM|CD|D?C?C?C?)(?:XC|XL|L?X?X?X?)(?:IX|IV|V?II?|III))""".r
		val monthPattern = """
		    (?:Jan(?:uary|\.)
		        |Febr?(?:uary|\.)
		        |Mar(?:ch|\.)
		        |Apr(?:il|\.)
		        |May
		        |Jun(?:e|\.)
		        |Jul(?:y|\.)
		        |Aug(?:ust|\.)
		        |Sept?(?:ember|\.)
		        |Oct(?:ober|\.)
		        |Nov(?:ember|\.)
		        |Dec(?:ember|\.)
		    )
		    """.r

		val doftwPattern = """
				    (?:Mon
				        |Tues?
				        |Wed(?:nes)?
				        |Thurs?
				        |Fri
				        |Satu?r?
				        |Sun
				    )(?:day|\.)
				    """.r


	  val cardinalDirectionPattern = Array("[Nn]orth", "[Ss]outh", "[Ee]ast", "[Ww]est").mkString("|").r
	//					                                       '[Nn]orth[Ww]est', '[Ss]outh[Ww]est',
	//					                                       '[Nn]orth[Ee]ast', '[Ss]outh[Ee]ast',
	//					                                       'NW', 'SW', 'NE', 'SE'
	//					                                       'N\.W\.', 'S\.W\.', 'N\.E\.', 'S\.E\.').mkString("|").r

		val ordinalPattern = """\d+(?:st|nd|rd|th)
						        |first|second|third|fourth|fifth|sixth|seventh|eighth|ninth|tenth
						        |eleventh|twelfth|thirteenth|fourteenth|fifteenth|sixteenth
						        |seventeenth|eighteenth|nineteenth
						        |twentieth|thirtieth|fou?rtieth|fiftieth|sixtieth|seventieth
						        |eightieth|ninetieth
						        |hundredth|thousandth|millionth|billionth
						    """.r

		val fractionPattern = """
						        half|halve|third|fourth|fifth|sixth|seventh|eighth|ninth|tenth
						        |eleventh|twelfth|thirteenth|fourteenth|fifteenth|sixteenth
						        |seventeenth|eighteenth|nineteenth
						        |twentieth|thirtieth|fou?rtieth|fiftieth|sixtieth|seventieth
						        |eightieth|ninetieth
						        |hundredth|thousandth|millionth|billionth""".r


  def features(dictionaries: Array[Set[String]]): Array[String] = {
			val str = word
				val features = new ArrayBuffer[String]
				features += "POS-%s".format(pos)
				features += "w-%s".format(str)
				features += "lower-%s".format(str.toLowerCase)
				features += "simple-%s".format(simplify(str))
				features += "ws1-%s".format(wordshape(str, 0))
				features += "ws2-%s".format(wordshape(str, 1))

		//		    # might want to normalize digits before running char ngrams
		//		    for c in char_ngrams(W, 2, 5):
		//		        yield 'ngram-%s' % c
		//		for 		
		    val wlen      = word.size
			  val ngramStart = 2
			  val ngramEnd   = 5
				for (split <- ngramStart to ngramEnd) {
					if (split <= wlen) {
						features += "ngram-start-^%s".format(word.substring(0,split))
					}
				}
				for (split <- ngramStart to ngramEnd) {
					if (wlen-split >= 0) {
						features += "ngram-end-%s$".format(word.substring(wlen-split,wlen))						
					}
				}
				
				if (str.substring(0,1).toUpperCase == str.substring(0,1)) features += "[is-capitalized]"
				if (str.toUpperCase == str) features += "[is-all-caps]"
				if (str.toLowerCase == str) features += "[is-all-lowercase]"
				if (str matches "^[,\\.;:?!()]+$") features += "[is-punct]"
				if (str matches "[,\\.;:?!()]") features += "[contains-punct]"
				dictionaries.zipWithIndex.foreach { case(dict, index) =>
				  if (dict.contains(word.toLowerCase)) {
						features + "[dict-%d]".format(index)
					}
				}
				features.toArray
			}
	
	def wordshape(str: String, rep: Int): String = {
		replaceRepitition(str.map { c =>
			if (c.toUpperCase == c) "A"
			else if (c.toLowerCase == c) "a"
			else if (c.toString matches "[0-9]") "1"
			else if (c == ' ') " "
			else c
		}.mkString(""), rep)
		//.replaceAll("(.+)", "$1")
		//replaceAll"(\b\w+\b)\s+(\1(\s+|$))+", "$1")
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
		buff.toString
	}
	
		def simplify(str: String): String = {
			if (str == "A.D." || str == "B.C.") return "ADBC"
			if (str == ",") return "COMMA"
	//		if (str.matches "((?:\d{1,3}(?:\,\d{3})*|\d+)(?:\.\d+)?)" return "NUMERIC"
	//		if (str matches "(?:Mon|Tues?|Wed(?:nes)?|Thurs?|Fri|Satu?r?|Sun)(?:day|\.)") return "DOFTW"
	//		if (str matches "(?:Jan(?:uary|\.)|Febr?(?:uary|\.)|Mar(?:ch|\.)|Apr(?:il|\.)|May|Jun(?:e|\.)|Jul(?:y|\.)|Aug(?:ust|\.)|Sept?(?:ember|\.)|Oct(?:ober|\.)|Nov(?:ember|\.)|Dec(?:ember|\.))") return "MONTH"
	//		if (str matches numberPattern) return "WRITTEN-NUMBER"
	//		if (str matches ordinalPattern) return "ORDINAL"
	//		if (str matches "(\d\d?) \s*? (?:(\:)? \s*? (\d\d))? \s* ([ap]\.m\.?|[ap]m|[ap])?\s* (?:\(? (GMT|EST|PST|CST)? \)? )? (?:\W|$)") return "TIME"
		//	if (str matches cardinalDirectionPattern) return "CARDINAL-DIRECTION"
			return str.toLowerCase.replaceAll("[0-9]", "#").replaceAll("[,\\.;:?!()]", "@")
		}
}

*/




