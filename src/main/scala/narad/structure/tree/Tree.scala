package narad.structure.tree

import collection.mutable.ArrayBuffer
import narad.nlp.trees.Span

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 12/16/12
 * Time: 10:00 PM
 * To change this template use File | Settings | File Templates.
 */

class Tree[+T](val node: T, children : List[Tree[T]] = List()) {

  private lazy val len = leaves.size

  def breadthFirstSearch: Iterator[Tree[T]] = {
    Iterator.single(this) ++ children.iterator ++ children.flatMap(_.breadthFirstSeachHelper)
  }

  private def breadthFirstSeachHelper: Iterator[Tree[T]] = {
    children.iterator ++ children.flatMap(_.breadthFirstSeachHelper)
  }

  def depthFirstSearch: Iterator[Tree[T]] = {
    Iterator.single(this) ++ children.flatMap(_.depthFirstSearch)
  }

  def leafFirstSearch: Iterator[Tree[T]] = {
    children.flatMap(_.leafFirstSearch).iterator ++ Iterator.single(this)
  }

  def height: Int = {
    if (isLeaf) {
      0
    }
    else {
      children.map(_.height).max + 1
    }
  }

  def isLeaf: Boolean = {
    children.size == 0
  }

  def leaves: Iterator[T] = {
//    depthFirstSearch.filter(_.isLeaf).map(_.node)
    depthFirstSearch.collect { case x if x.isLeaf => x.node }
  }

  def length: Int = len

  override def toString(): String = {
    if (isLeaf) {
      return "(%s)".format(node.toString())
    }
    else {
      return "(%s %s)".format(node.toString(), children.map(_.toString()).mkString(" "))
    }
  }

  def width: Int = {
    leaves.size
  }
}


object StringTree {

  //         A
  //        / \
  //       B   E
  //      / \   \
  //     C  D   F

  def main(args: Array[String]) {
    val CTree = new Tree[String]("C")
    val DTree = new Tree[String]("D")
    val FTree = new Tree[String]("F")
    val BTree = new Tree[String]("B", List(CTree, DTree))
    val ETree = new Tree[String]("E", List(FTree))
    val ATree = new Tree[String]("A", List(BTree, ETree))
    println(ATree.toString())
    println("height: " + ATree.height)
    println("\nDepth First Search:")
    for (st <- ATree.depthFirstSearch) { println("DFS: " + st.toString)}
    println("\nBreadth First Search:")
    for (st <- ATree.breadthFirstSearch) { println("BFS: " + st.toString)}
    println("\nLeaves: " + ATree.leaves.mkString(", "))
  }
}










/*
{ //extends Iterable[Tree[T]] {

  // def breadthFirstSearch
  // def depthFirstSearch
}
*/
