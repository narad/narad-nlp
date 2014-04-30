package narad.nlp.tagger

import collection.mutable.ArrayBuffer
import narad.nlp.tagger.factorial._

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 5/6/13
 * Time: 2:15 PM
 * To change this template use File | Settings | File Templates.
 */

class MorphTaggerParams(args: Array[String]) extends FactorialTaggerParams(args) {
  def POS  = getBoolean("--pos", false)
  def COARSE_POS  = getBoolean("--coarse.pos", false)
  def CASE = getBoolean("--case", false)
  def PERSON = getBoolean("--person")
  def GENDER = getBoolean("--gender")
  def NUMBER = getBoolean("--number")


  def ATTRIBUTES = {
    val ab = new ArrayBuffer[String]()
    if (POS) ab += "FINE"
    if (COARSE_POS) ab += "COARSE"
    if (CASE) ab += "CASE"
    if (PERSON) ab += "PERSON"
    if (GENDER) ab += "GENDER"
    if (NUMBER) ab += "NUMBER"
    ab.toArray
  }

  def HIDDEN_SYNTAX = getBoolean("--hidden.syntax", false)
  def OBSERVE_SYNTAX = getBoolean("--observe.syntax", false)
  def OBSERVE_BIGRAM = getBoolean("--observe.bigram", false)
  def SPARSE = getBoolean("--sparse", false)
  def INTEGERIZE = getBoolean("--integerize")

  override def MARGINALIZATION = HIDDEN_SYNTAX
}

trait MorphFeatures extends TaggerFeatures {}

