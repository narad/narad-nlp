package narad.structure.graph


/*
 Copyright 2010 David Hall

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/

/**
 * Represents a graph, which consists of Nodes, Edges, and ways to go from Edges to Nodes, and vice-versa.
 *
 * @author dlwh
 */
trait Graph[Node,Edge] {
  /**
   * Returns an iterator over all edges in the graph.
   */
  def edges: Iterator[Edge];

  /**
   * Returns the nodes that are on either side of a graph.
   */
//  def endpoints(e: Edge):(Node,Node);

  /**
   * Returns all nodes in the graph.
   */
  def nodes: Iterable[Node];

  /**
   * Returns an iterator of all edges that touch a node.
   */
  def edgesFrom(n: Node): Iterator[Edge];

  /**
   * Returns all nodes that are "Successors" of the current node.
   * In an undirected graph, this will return all neighbors. In a directed
   * graph, it will return only those nodes that are connnected by an edge
   * where its source is the passed-in node.
   */
  def successors(n: Node): Iterator[Node]

  /**
   * Returns the edge associated with a node, if any exists.
   *
   * TODO: maybe should make it an Iterable or something.
   */
  def getEdge(n1: Node, n2: Node): Option[Edge];

}

/**
 * A Digraph refines the notion of a graph, adding sources and sinks to edges.
 *
 * @author dlwh
 */
trait Digraph[Node,Edge] extends Graph[Node,Edge] {
  def source(e: Edge): Node
  def sink(e: Edge): Node

  def filter(f: Edge=>Boolean) = new Digraph.Filtered(this,f);

  def endpoints(e: Edge) = (source(e),sink(e));
}


object Graph {
  /* Constructs a graph from an edge list. */
  def fromEdges[N](edges: (N,N)*):Graph[N,(N,N)] = Digraph.fromEdges(edges:_*);
  /**
   * Constructs a graph from an adjacency list. The list must be symmetric for it to
   * be an undirected graph.
   */
  def fromAdjacencyList[N](adjacencyList: Map[N,Seq[N]]): Graph[N,(N,N)] = {
    Digraph.fromAdjacencyList(adjacencyList);
  }

}


object Digraph {

  /**
   * Constructs a graph from an edge list.
   */
  def fromEdges[N](edges: (N,N)*) = {
    val adjList = edges.groupBy(_._1).mapValues(_.map(_._2));
    Digraph.fromAdjacencyList(adjList);
  }

  /**
   * Constructs a graph from an adjacency list.
   */
  def fromAdjacencyList[N](adjacencyList: Map[N,Seq[N]]):Digraph[N,(N,N)] = {
    type Node = N;
    type Edge = (N,N);
    new Digraph[N,(N,N)] {

      def edges = for( (n,adj) <- adjacencyList iterator; m <- adj iterator) yield (n,m);
      override def endpoints(e: Edge):(Node,Node) = e;
      lazy val nodes = (adjacencyList.keys ++ adjacencyList.values.flatten).toSet
      def successors(n: Node) = {
        adjacencyList.getOrElse(n, Seq.empty).iterator
      }
      def getEdge(n1: Node, n2: Node) = {
        for(adj <- adjacencyList.get(n1); m <- adj.find(_ == n2)) yield (n1,n2);
      }
      def source(e: Edge): Node = e._1
      def sink(e: Edge): Node = e._2;

      def edgesFrom(n: Node): Iterator[Edge] = adjacencyList.getOrElse(n,Seq.empty).iterator.map(n2 => (n,n2));

      override def toString() = "Graph[" + adjacencyList + "]";
    }
  }

  class Filtered[N,E](g: Digraph[N,E], f: E=>Boolean) extends Digraph[N,E] {
    def sink(e: E) = g.sink(e);

    def source(e: E) = g.source(e);

    def getEdge(n1: N, n2: N) = g.getEdge(n1,n2).filter(f);

    def successors(n: N) = g.edgesFrom(n).filter(e => f(e) && source(e) == n).map(sink _);

    def edgesFrom(n: N) = g.edgesFrom(n).filter(f)

    def nodes = g.nodes

    def edges = g.edges.filter(f);
  };
}


// http://www.scala-lang.org/node/124
/*
abstract class Graph {
  type Edge
  type Node <: NodeIntf
  abstract class NodeIntf {
    def connectWith(node: Node): Edge
  }
  def nodes: List[Node]
  def edges: List[Edge]
  def addNode: Node
}

abstract class DirectedGraph extends Graph {
  type Edge <: EdgeImpl
  class EdgeImpl(origin: Node, dest: Node) {
    def from = origin
    def to = dest
  }
  class NodeImpl extends NodeIntf {
    def connectWith(node: Node): Edge = {
      val edge = newEdge(this, node)
      edges = edge :: edges
      edge
    }
  }
  protected def newNode: Node
  protected def newEdge(from: Node, to: Node): Edge
  var nodes: List[Node] = Nil
  var edges: List[Edge] = Nil
  def addNode: Node = {
    val node = newNode
    nodes = node :: nodes
    node
  }
}
*/