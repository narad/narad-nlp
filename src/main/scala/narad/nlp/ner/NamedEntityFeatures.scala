package narad.nlp.ner
import java.io.FileWriter
import narad.io.onto._
import narad.nlp.ling.{Lexicon, TaggedToken}
import scala.collection.mutable.ArrayBuffer
import narad.util.RegexPatterns

trait NamedEntityFeatures {

  def extractFeatures(trainNerFile: String, trainSyntaxFile: String, trainFeatureFile: String, labels: Array[String],
                      order: Int, params: NamedEntityParams) = {
    val reader = new OntoReader(trainNerFile, trainSyntaxFile, params)
    val out = new FileWriter(trainFeatureFile)
    reader.zipWithIndex.foreach { case(datum, i) =>
 //     if (i % params.PRINT_INTERVAL == 0) System.err.println("  example %d...".format(i))
      val slen = datum.slen
      val ner = datum.ner
      val tokens = datum.tokens
      System.err.println("IN extract features for NER")
      out.write("@slen\t%d\n".format(slen))
      out.write("@maxseg\t%d\n".format(order))
      out.write("@labels\t%s\n".format(labels.mkString(" ")))
      for (j <- slen to 1 by -1) {
        var labeled = false
        for (i <- scala.math.max(0, j-order) to j-1) {
          val width = j-i
          val feats = nerFeatures(tokens, i, j)
          val correctSpan = if (ner.containsSpan(i,j) || (width == 1 && !ner.coversSpan(i,j))) "+" else ""
          out.write("nerbracket(%d,%d)\t%s%s\n".format(i, j, correctSpan, feats.mkString(" ")))

          val correctLabel = if ((!ner.containsSpan(i,j) && width > 1) || (width == 1 && ner.coversSpan(i,j))) "+" else ""
          out.write("nerlabel(%d,%d,0)\t%sNone\n".format(i, j, correctLabel))

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

          for (label <- labels) {
            val builder = new StringBuilder()
            for (f <- feats) builder.append(" " + label + "_" + f)
            val correctLabel = if (ner.containsSpanLabel(i,j,label)) "+" else ""
            out.write("nerlabel(%d,%d,%s)\t%s%s\n".format(i, j, labels.indexOf(label)+2, correctLabel, builder.toString.trim))
          }
        }
      }
      out.write("\n")
    }
    out.close()
  }


  def connectionFeatures(tokens: Array[TaggedToken], start: Int, end: Int): Array[String] = {
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

  def pad(array: Array[TaggedToken], spad: Int, epad: Int): Array[TaggedToken] = {
    val STARTPOS = "START"
    val ENDPOS = "END"
    val buffer = new ArrayBuffer[TaggedToken]
    val end = spad + epad + array.size
    for (i <- 0 until end){
      if (i < spad){
        buffer += new TaggedToken("[START%d]".format(spad-i), STARTPOS)
      }
      else if (i >= array.size + spad){
        buffer += new TaggedToken("[END%d]".format(1 + i - (array.size + spad)), ENDPOS)
      }
      else{
        buffer += array(i-spad)
      }
    }
    return buffer.toArray
  }



def nerFeatures(tokens: Array[TaggedToken], i: Int, j: Int): Array[String] = {
		val feats = new ArrayBuffer[String]
		val width = j-i
		val words = tokens.map(_.word)
		val tags  = tokens.map(_.pos)
    val dictionaries = Array[Lexicon]() // ********* Instantiate
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


	def unigramFeatures(token: TaggedToken, dictionaries: Array[Lexicon]): Array[String] = {
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
	//		if (dict.contains(word.toLowerCase)) {
	//			feats + "[dict-%s]".format(dict.name)
	//		}
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
		buff.toString()
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

