package narad.io.conll

import narad.util.ArgParser
import narad.bp.structure.Potential
import scala.collection.mutable.{ArrayBuffer, HashSet}

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

class CoNLLDatum(grid: Array[Array[String]]) {
	
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
    val cols = feat(i).toLowerCase.split("|")
    if (cols.size == 8) return cols(7)
    for (i <- 0 until cols.size) {
      if (cols(i).contains("gen")) return cols(i)
    }
    return ("-")
  }

  def mnumber(i: Int): String = {
    val cols = feat(i).toLowerCase.split("|")
    if (cols.size == 8) return cols(1)
    for (i <- 0 until cols.size) {
      if (cols(i).contains("num")) return cols(i)
    }
    return ("-")
  }
}

object CoNLLDatum {
	
	// ID FORM LEMMA PLEMMA POS PPOS FEAT PFEAT HEAD PHEAD DEPREL PDEPREL FILLPRED PRED APREDs
	def constructFromCoNLL(lines: Array[String]): CoNLLDatum = {
		return new CoNLLDatum(lines.map(_.split("\t")))
	}
}

