package narad.nlp.parser.constituent

import collection.mutable.ArrayBuffer
import math._
import narad.nlp.trees.ConstituentTree
import narad.nlp.ling.{TaggedToken => Token}

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 8/31/13
 * Time: 9:32 PM
 */
trait ConstituentParserFeatures {

  def constituentSpanFeatures(otokens: Array[Token], start: Int, end: Int,
                               params: ConstituentParserParams, tagset: Array[String] = Array()): Array[String] = {
    val feats = 1
    if (feats == 1) {
      constituentSpanFeatures1(otokens, start, end, params, tagset)
    }
    else {
      constituentSpanFeatures2(otokens, start, end, params)
    }
  }

    def constituentSpanFeatures1(otokens: Array[Token], start: Int, end: Int,
                              params: ConstituentParserParams, tagset: Array[String] = Array()): Array[String] = {
    val window = params.FEATURE_WINDOW
    val lexicalWindow = 2
    val mode = 2
    val features = new ArrayBuffer[String]
    val si = start + window
    val ei = end + window - 1
    val width = end-start
    val tokens = pad(otokens, window, window)
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

    // Within-Span features
    val dtags = tags.slice(si, ei+1).distinct
    for (tag <- tagset) {
      if (dtags.contains(tag)) {
        features += "[contains-tag]-%s".format(tag)
        //        features += "[contains-tag-size]-%s-%d".format(tag, width)
      }
      else if (!tagset.isEmpty) {
        features += "[doesnt-contains-tag]-%s".format(tag)
        //        features += "[doesnt-contains-tag-size]-%s-%d".format(tag, width)
      }
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










  def constituentSpanFeatures2(tokens1: Array[Token], start: Int, end: Int, params: ConstituentParserParams): Array[String] = {
    val slen = tokens1.size
    val window = params.FEATURE_WINDOW
    val tokens = pad(tokens1, window, window)
    val features = new ArrayBuffer[String]

    val si = start + window
    val ei = end + window - 1
    val width = end-start

    // Simple features
//    features += "[spanwidth]-%d".format(width)
//    features += "[span-start]-%d".format(start)
//    features += "[span-end]-%d".format(end)
 //   if (slen > 5) {
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
        // CCM Features
        features += "[ccm-outside-pos]-%s-%s".format(tokens(si-1).pos, tokens(ei+1).pos)
        features += "[ccm-inside-pos]-%s".format(tokens.slice(si, ei+1).map(_.pos).mkString("-"))
        features += "[ccm-all-pos]-%s".format(tokens.slice(si-1, ei+2).map(_.pos).mkString("-"))
      }
/*    }
    else {
      val startToken = tokens(si)
      features += "[start-word]-%s".format(startToken.word.toLowerCase)
      features += "[start-tag]-%s".format(startToken.pos)
      val endToken = tokens(ei)
      features += "[end-word]-%s".format(endToken.word.toLowerCase)
      features += "[end-tag]-%s".format(endToken.pos)
      features += "[ccm-outside-pos-small]-%s-%s".format(tokens(si-1).pos, tokens(ei+1).pos)
      features += "[ccm-inside-pos-small]-%s".format(tokens.slice(si, ei+1).map(_.pos).mkString("-"))
      features += "[ccm-all-pos-small]-%s".format(tokens.slice(si-1, ei+2).map(_.pos).mkString("-"))
      features += "[span-small-bias]"
      features += "[spanwidth-small]-%d".format(width)
      features += "[span-start-end-width-small]-%d-%d-%d".format(start, end, width)
      if (width == slen) features += "[top-span-small]"
      if (start == 0)  features += "[start-span-small]"
      if (end == slen) features += "[end-span-small]"
    }*/
    features.toArray
  }

  def footprint(tokens: Array[String], si: Int, ei: Int, pcutout: Int, pcutin: Int, ecutin: Int, ecutout: Int): String = {
    "[foot-%d-%d-%d-%d]-%s-&&-%s".format(pcutout, pcutin, ecutin, ecutout,
      tokens.slice(si-pcutout, si+pcutin+1).mkString("-"),
      tokens.slice(ei-ecutin, ei+ecutout+1).mkString("-"))
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

  def getUnaryFeatures(tree: ConstituentTree, start: Int): Array[String] = {
    val features = new ArrayBuffer[String]
    val window = 2
    val tokens = pad(tree.tokens.toArray, window, window)
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
    val START_POS = "$"
    val END_POS = "&"
    val buffer = new ArrayBuffer[Token]
    val end = spad + epad + array.size
    for (i <- 0 until end){
      if (i < spad){
        buffer += Token("[START%d]".format(spad-i), START_POS)
      }
      else if (i >= array.size + spad){
        buffer += Token("[END%d]".format(1 + i - (array.size + spad)), END_POS)
      }
      else{
        buffer += array(i-spad)
      }
    }
    buffer.toArray
  }

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

  def ccmFeatures(tree: ConstituentTree, start: Int, end: Int): Array[String] = {
    val tokens = tree.tokens.toArray
    val conjTags = tokens.slice(start, end).map(_.pos).mkString("-")
    val startTag = if (start == 0) "$START" else tokens(start-1).pos
    val endTag   = if (end == tokens.size) "$END" else tokens(end).pos
    val features = new ArrayBuffer[String]
    features += "OUTSIDE_POS-%s-%s".format(startTag, endTag)
    features += "INSIDE_POS-%s".format(conjTags)
    features.toArray
  }
}



























/*
      reader.zipWithIndex.foreach { case(tree, i) =>
      if (i % params.PRINT_INTERVAL == 0) System.err.println("  example %d...".format(i))
      val slen = tree.length
      out.write("@slen\t%d\n".format(slen))
      out.write("@words\t%s\n".format(tree.words.mkString(" ")))
      out.write("@tags\t%s\n".format(tree.tags.mkString(" ")))
      println("tree = " + tree)
      val btree = tree.binarize().removeUnaryChains
      //extractBracketFeatures(btree, out, params)
      val bf = getBracketFeatures(btree, index, params)
      out.write(bf.toString + "\n")
    }
*/


/*
def extractBracketFeatures(tree: ConstituentTree, out: FileWriter, params: ConstituentParserParams) {
  val BRACK_NAME = params.BRACK_NAME
  val slen = tree.slen
  for ( width <- 2 to slen; start <- 0 to (slen - width)) {
    val end = start + width
    val feats = constituentSpanFeatures(tree.tokens(), start, end, params)
    val isCorrect = tree.containsSpan(start, end)
    out.write("%s(%d,%d)\t%s%s\n".format(BRACK_NAME, start, end, if (isCorrect) "+" else "", feats.mkString(" ")))
  }
}
*/




/*
for (i <- 0 until min(width, window); j <- 1 until min(width, maxCut)-i) {
  features += "[tags-from-start-%d]-%s".format(j-i, tags.slice(si+i,si+j+1).mkString("-"))
  if (j <= 3) {
    features += "[words-from-start-%d]-%s".format(j-i, words.slice(si+i,si+j+1).mkString("-"))
  }
}
for (i <- 0 until min(width, window); j <- 1 until min(width, maxCut)-i) {
  features += "[tags-before-end-%d]-%s".format(i, tags.slice(ei-j,ei-i+1).mkString("-"))
  if (j <= 3) {
    features += "[words-before-end-%d]-%s".format(i, words.slice(ei-j,ei-i+1).mkString("-"))
  }
}
for (i <- 1 until window) {
  features += "[tags-before-start-%d]-%s".format(si-i, tags.slice(si-i,si+1).mkString("-"))
  if (i <= 3) {
    features += "[words-before-start-%d]-%s".format(si-i, words.slice(si-i,si+1).mkString("-"))
  }
}
*/

/*
for (i <- 0 until min(width, window); j <- 1 until min(width, maxCut)-i) {
  features += "[tags-from-start-%d]-%s".format(i, tokens.slice(si+i,si+j).map(_.pos).mkString("-"))
  if (j <= 3) {
    features += "[words-from-start-%d]-%s".format(i, tokens.slice(si+i,si+j).map(_.word).mkString("-"))
  }
}
for (i <- 0 until min(width, window); j <- 1 until min(width, maxCut)-i) {
  features += "[tags-before-end-%d]-%s".format(i, tokens.slice(ei-j,ei-i).map(_.pos).mkString("-"))
  if (j <= 3) {
    features += "[words-before-end-%d]-%s".format(i, tokens.slice(ei-j,ei-i).map(_.pos).mkString("-"))
  }
}
if (si > 1) {
  for (i <- si-2 until 0) {
    features += "[tags-before-start-%d]-%s".format(si-i, tokens.slice(i,si).map(_.pos).mkString("-"))
    if (si-i <= 3) {
      features += "[words-before-start-%d]-%s".format(si-i, tokens.slice(i,si).map(_.pos).mkString("-"))
    }
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
*/



/*
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
*/

/*
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

	def ccmFeatures(tree: ConstituentTree, start: Int, end: Int): Array[String] = {
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



	def unaryFeatures(tree: ConstituentTree, index: Int, treeFeatures:Boolean = false, window: Int=4): Array[String] = {
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

		// ConstituentTree Features
/*
def unaryFeatures(tree: ConstituentTree, index: Int, treeFeatures:Boolean = false, window: Int=4): Array[String] = {
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

	def getUnaryFeatures(tree: ConstituentTree, start: Int): Array[String] = {
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

/*
	def main(args: Array[String]) {
		val ex = "(TOP (FRAG (INTJ (JJ Good) (NN evening)) (, ,) (NP (NN everyone)) (. .)))"
		val tree = TreebankReader.parseExpression(ex, defaultLabel = "TOP").removeNones().binarize()
		tree.annotateWithIndices(0)
		val labels = tree.getSpans.map(_.label).toArray.distinct
//		println(tree)
//		println
		Featurize.printFeatures(tree, labels, "syntaxunary", "blah", prune=false, vpots=Array[Array[String]](), options=new narad.util.ArgParser(Array[String]()))
	}
	*/

}

*/