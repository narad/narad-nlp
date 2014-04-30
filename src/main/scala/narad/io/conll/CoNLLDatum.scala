package narad.io.conll

import narad.util.ArgParser
import narad.bp.structure.Potential
import scala.collection.mutable.{ArrayBuffer, HashSet}
import narad.nlp.srl.SRLDatum
import collection.mutable

// ID
// Form
// Lemma
// CPosTag
// PosTag
// Feats
// Head
// DepRel
// PHead
// PDepRel

abstract class CoNLLDatum() {

  def slen: Int

  def form(i: Int): String

  def forms: Iterable[String]

  def word(i: Int): String

  def words: Iterable[String]

  def head(i: Int): Int

  def heads: Iterable[Int]

  def postag(i: Int): String

  def postags: Iterable[String]

  def cpostag(i: Int): String

  def cpostags: Iterable[String]

  def lemma(i: Int): String

  def lemmas: Iterable[String]



  def attribute(i: Int, mode: String): String

}

// class CoNLL2009Datum() extends CoNLLDatum {}

class CoNLL2003Datum(grid: Array[Array[String]]) extends CoNLLDatum {
	
	def form(i: Int) = grid(i-1)(1)
	
	def forms = (for (i <- 0 until slen) yield grid(i)(1))

	def lemma(i: Int) = grid(i-1)(2)

	def lemmas = (for (i <- 0 until slen) yield grid(i)(2))

	def cpostag(i: Int) = grid(i-1)(3)
	
	def cpostags = (for (i <- 0 until slen) yield grid(i)(3))

	def postag(i: Int) = grid(i-1)(4)
	
	def postags = (for (i <- 0 until slen) yield grid(i)(4))

	def feat(i: Int) = grid(i-1)(5)
	
	def feats = (for (i <- 0 until slen) yield grid(i)(5))
	
	def head(i: Int) = grid(i-1)(6).toInt
	
	def heads = (for (i <- 0 until slen) yield grid(i)(6).toInt)

	def deprel(i: Int) = grid(i-1)(7)
	
	def deprels = (for (i <- 0 until slen) yield grid(i)(7))

	def phead(i: Int) = grid(i-1)(8).toInt

	def pheads = (for (i <- 0 until slen) yield grid(i)(8).toInt)

	def pdeprel(i: Int) = grid(i-1)(10)
	
	def pdeprels = (for (i <- 0 until slen) yield grid(i)(10))
	
	def slen = grid.size

	override def toString = grid.map(_.mkString("X")).mkString("\n")

	def word(i: Int) = form(i)
	
	def words = forms

  def attribute(i: Int, mode: String): String = {
    mode match {
      case "COARSE" => cpostag(i)
      case "FINE"   => postag(i)
      case "CASE"   => mcase(i)
      case "GENDER" => mgender(i)
      case "NUMBER" => mnumber(i)
      case "CASE+GENDER+NUMBER" => mcase(i) + "|" + mgender(i) + "|" + mnumber(i)
      case _ => ""
    }
  }

  // Morph Accessors
//  -|p|-|-|-|n|n|-
  def mcase(i: Int): String = {
    val cols = feat(i).toLowerCase.split("\\|")
    if (cols.size == 8) return cols(6)
    for (i <- 0 until cols.size) {
      if (cols(i).contains("case")) return cols(i)
    }
    return ("-")
  }

  def mgender(i: Int): String = {
    val cols = feat(i).toLowerCase.split("\\|")
    if (cols.size == 8) return cols(5)
    for (i <- 0 until cols.size) {
      if (cols(i).contains("gen")) return cols(i)
    }
    return ("-")
  }

  def mnumber(i: Int): String = {
    val cols = feat(i).toLowerCase.split("\\|")
    if (cols.size == 8) return cols(1)
    for (i <- 0 until cols.size) {
      if (cols(i).contains("num")) return cols(i)
    }
    return ("-")
  }

  def mperson(i: Int): String = {
    val cols = feat(i).toLowerCase.split("\\|")
    if (cols.size == 8) return cols(0)
    for (i <- 0 until cols.size) {
      if (cols(i).contains("per")) return cols(i)
    }
    return ("-")
  }
}

object CoNLLDatum {
	
	// ID FORM LEMMA PLEMMA POS PPOS FEAT PFEAT HEAD PHEAD DEPREL PDEPREL FILLPRED PRED APREDs
	def constructFromCoNLL(lines: Array[String]): CoNLLDatum = {
    val numFields = lines.head.split("\t").size
//    println("Number of fields in CoNLL file: " + numFields)
    if (numFields >= 12) {
      SRLDatum.constructFromCoNLL(lines)
    }
    else {
      return new CoNLL2003Datum(lines.map(_.split("\t")))
    }
	}
}

