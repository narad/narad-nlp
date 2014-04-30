package narad.nlp.trees

import narad.bp.optimize.{EvalContainer, Scorable}
import narad.util.HashCounter

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 5/1/13
 * Time: 3:31 PM
 * To change this template use File | Settings | File Templates.
 */
class LabeledDependencyTree(val h: Array[Int], val rels: Array[String]) extends DependencyTree(h)

class DependencyTree(val heads: Array[Int]) extends Scorable {

  def crosses(i: Int, j: Int): Boolean = {
    for (ii <- 0 until heads.size) {
      if (crosses(i, j, ii+1, heads(ii))) return true
    }
    false
  }

  def crosses(i: Int, j: Int, k: Int, l: Int): Boolean = {
    (i < k && k < j && j < l) || (k < i && j < k && l < j)
  }

  def hasHead(i: Int, j: Int): Boolean = {
    heads(i-1) == j
  }

  def hasDirectedPath(i: Int, j: Int): Option[List[Int]] = {
    if (hasHead(i,j)) {
      return Some(List(j))
    }
    else {
      if (headOf(i) > 0) {
       hasDirectedPath(headOf(i), j) match {
         case Some(x) => Some(List(i) ++ x)
         case _=> None
       }
      }
      else {
        None
      }
    }
  }

  def hasPath(i: Int, j: Int): Option[List[Int]] = {
    val lpath = hasDirectedPath(i,j)
    val rpath = hasDirectedPath(j,i)
    lpath match {
      case Some(x) => return lpath
      case _=>
    }
    rpath match {
      case Some(x) => return rpath
      case _=>
    }
    None
  }

  def headOf(i: Int) = {
    heads(i-1)
  }

 // def heads = heads

  def score(other: Scorable): DependencyEvalContainer = {
    other match {
      case x: DependencyTree => {
        val ec = new DependencyEvalContainer
        ec("correct") = heads.zip(x.heads).foldLeft(0){case(count, (t, g)) => if (t == g) 1 else 0}
        ec("test") = heads.size
        ec("gold") = x.heads.size
        ec
      }
      case _ => {
        System.err.println("Unscorable object passed to DependencyTree.score().")
        new DependencyEvalContainer
      }
    }
  }

  def size = heads.size

  override def toString = heads.mkString("\n")
}

class DependencyEvalContainer extends HashCounter[String] with EvalContainer {

  def combine(that: EvalContainer): DependencyEvalContainer = {
    that match {
      case x: DependencyEvalContainer => {
        val e = new DependencyEvalContainer
        for (k <- this.keys) {
          e.increment(k, this(k))
        }
        for (k <- x.keys) {
          e.increment(k, x(k))
        }
        return e
      }
      case _ => {
        println("exiting the combine op")
        this
      }
    }
  }

  override def toString = {
    "DP EC test" + this.toArray.mkString("\n")
  }
}

//   def combine(other: EvalContainer): EvalContainer
