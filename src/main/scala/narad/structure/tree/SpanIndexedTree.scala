package narad.structure.tree

import collection.mutable.ArrayBuffer
import narad.nlp.trees.Span
import javax.management.remote.rmi._RMIConnection_Stub

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 6/13/13
 * Time: 5:16 PM
 */
class SpanIndexedTree[+T](node: T, children : List[Tree[T]] = List()) extends Tree[T](node, children.asInstanceOf[List[Tree[T]]]) {

  lazy private val spans = index

  def index: Array[Array[ArrayBuffer[Span]]] = {
    val ispans = Array.fill(length+1,length+1)(new ArrayBuffer[Span])
    var numLeaves = 0
    for (t <- leafFirstSearch) {
      if (t.isLeaf) {
        ispans(numLeaves)(numLeaves+1) += new Span(numLeaves, numLeaves+1, node.toString, 0)
        numLeaves += 1
      }
      else {
        val len = t.length
        val height = ispans(numLeaves-len)(numLeaves).size
        ispans(numLeaves-len)(numLeaves) += new Span(numLeaves-len, numLeaves, node.toString, height)
      }
    }
    ispans
  }

  def containsSpan(i: Int, j: Int): Boolean = {
    if (i < 0 || j < 0) return false
    if (i > length || j > length) return false
    !spans(i)(j).isEmpty
  }

  def containsSpan(i: Int, j: Int, l: String): Boolean = {
    if (!containsSpan(i, j)) return false
    return spans(i)(j).exists(_.label == l)
  }

  def containsUnarySpan(i: Int, j: Int): Boolean = {
    if (i < 0 || j < 0) return false
    if (i > length || j > length) return false
    spans(i)(j).exists(_.isUnary)
  }

  def containsUnarySpan(i: Int, j: Int, l: String): Boolean = {
    if (i < 0 || j < 0) return false
    if (i > length || j > length) return false
    spans(i)(j).exists(s => s.isUnary && s.label == l)
  }

  def containsUnarySpan(i: Int, j: Int, l: String, h: Int): Boolean = {
    if (i < 0 || j < 0) return false
    if (i > length || j > length) return false
    spans(i)(j).exists(s => s.isUnary && s.label == l && s.height == h)
  }

  def containsLabel(i: Int, j: Int, l: String): Boolean = {
    if (i < 0 || j < 0) return false
    if (i > length || j > length) return false
    spans(i)(j).exists(s => s.label == l)
  }

  def highestUnarySpan(i: Int, j: Int): String = {
    if (i < 0 || j < 0) return "none"
    if (i > length || j > length) return "none"
    if (spans(i)(j).filter(_.isUnary).size > 0) {
      spans(i)(j).filter(_.isUnary).sortBy(_.height * -1).head.label
    }
    else {
      "none"
    }
  }

  def toSpans: Iterable[Span] = {
    for (i <- 0 until length; j <- 1 to length; k <- 0 until spans(i)(j).size) yield spans(i)(j)(k)
  }

  def spansAt(i: Int, j: Int): Iterable[Span] = spans(i)(j).toIterable
}