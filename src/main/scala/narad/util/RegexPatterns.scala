package narad.util
import scala.util.matching.Regex

object RegexPatterns {
	val PunctuationPattern = """[,\\.;:?!\\(\\)]""".r
	val InitialPattern     = """"[A-Z]\.""".r
	val NumericPattern     = """((?:\d{1,3}(?:\,\d{3})*|\d+)(?:\.\d+)?)""".r
	val TimePattern        = """(\d\d?) \s*? (?:(\:)? \s*? (\d\d))? \s* ([ap]\.m\.?|[ap]m|[ap])? \s* (?:\(? (GMT|EST|PST|CST)? \)? )? (?:\W|$)""".r
	val MonthPattern       = """(?:Jan(?:uary|\.)|Febr?(?:uary|\.)|Mar(?:ch|\.)|Apr(?:il|\.)|May|Jun(?:e|\.)|Jul(?:y|\.)|Aug(?:ust|\.)|Sept?(?:ember|\.)|Oct(?:ober|\.)|Nov(?:ember|\.)|Dec(?:ember|\.))""".r
	val DayPattern         = """(?:Mon|Tues?|Wed(?:nes)?|Thurs?|Fri|Satu?r?|Sun)(?:day|\.)""".r
	val DirectionPattern   = Array("[Nn]orth", "[Ss]outh", "[Ee]ast", "[Ww]est", "[Nn]orth[Ww]est", "[Ss]outh[Ww]est", "NW", "SW", "NE", "SE", "N\\.W\\.", "S\\.W\\.", "N\\.E\\.", "S\\.E\\.").mkString("|").r

		//	val singlechar = pattern(ALPHA)
		//	val capletter = pattern("[A-Z]")
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

	val OrdinalPattern = """\d+(?:st|nd|rd|th)
	|first|second|third|fourth|fifth|sixth|seventh|eighth|ninth|tenth
	|eleventh|twelfth|thirteenth|fourteenth|fifteenth|sixteenth
	|seventeenth|eighteenth|nineteenth
	|twentieth|thirtieth|fou?rtieth|fiftieth|sixtieth|seventieth
	|eightieth|ninetieth
	|hundredth|thousandth|millionth|billionth
	""".r

	val FractionPattern = """
	half|halve|third|fourth|fifth|sixth|seventh|eighth|ninth|tenth
	|eleventh|twelfth|thirteenth|fourteenth|fifteenth|sixteenth
	|seventeenth|eighteenth|nineteenth
	|twentieth|thirtieth|fou?rtieth|fiftieth|sixtieth|seventieth
	|eightieth|ninetieth
	|hundredth|thousandth|millionth|billionth""".r
	
	
}