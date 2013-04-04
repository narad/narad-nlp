package narad.nlp.disfluency

import narad.nlp.trees.Token
import collection.mutable.ArrayBuffer

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 3/24/13
 * Time: 10:52 AM
 * To change this template use File | Settings | File Templates.
 */
trait DisfluencyFeatures {

  def DisfluencyFeatures(tokens: Array[Token], i: Int, j: Int, k: Int, l: Int): Array[String] = {
    val ab = new ArrayBuffer[String]()
    ab += "[form-form]-%s-%s".format(tokens(i).word, tokens(j).word)
    if (tokens(i).word == tokens(j).word) ab += "[ident-form]"
    if (tokens(i).pos == tokens(j).pos) ab += "[ident-pos]"

    ab += "[form-form-gap]-%s-%s-%d".format(tokens(i).word, tokens(j).word, j-i)
    if (tokens(i).word == tokens(j).word) ab += "[ident-form-gap]-%d".format(j-i)
    if (tokens(i).pos == tokens(j).pos) ab += "[ident-pos-gap]-%d".format(j-i)
    ab.toArray
  }

  def similarityWordFeats(tokens: Array[Token], i: Int, j: Int): Array[String] = {
    val ab = new ArrayBuffer[String]()
    ab += "[form-form]-%s-%s".format(tokens(i).word, tokens(j).word)
    if (tokens(i).word == tokens(j).word) ab += "[ident-form]"
    if (tokens(i).pos == tokens(j).pos) ab += "[ident-pos]"

    ab += "[form-form-gap]-%s-%s-%d".format(tokens(i).word, tokens(j).word, j-i)
    if (tokens(i).word == tokens(j).word) ab += "[ident-form-gap]-%d".format(j-i)
    if (tokens(i).pos == tokens(j).pos) ab += "[ident-pos-gap]-%d".format(j-i)
    ab.toArray
  }

  def similarityFeats(tokens: Array[Token], start1: Int, end1: Int, start2: Int, end2: Int): Array[String] = {
    val ab = new ArrayBuffer[String]()
    ab += tokens.map(_.word).slice(start1,end1).mkString("-") + "-SIMTO-" + tokens.map(_.word).slice(start2,end2).mkString("-")
    ab += tokens.map(_.pos).slice(start1,end1).mkString("-") + "-SIMTO-" + tokens.map(_.pos).slice(start2,end2).mkString("-")
    if (end1 == start1+1 && end2 == start2+1 && tokens(start1).word == tokens(start2).word) {
      ab += "[same-word]"
    }
    ab.toArray
  }

  def interregnumFeats(tokens: Array[Token], start: Int, end: Int): Array[String] = {
    val buf = new ArrayBuffer[String]()
    buf += "inter-" + tokens.map(_.word).slice(start, end).mkString("-")
    buf += "inter-" + tokens.map(_.pos).slice(start, end).mkString("-")
    buf.toArray
  }

}
