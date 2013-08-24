package narad.nlp.trees

//import narad.nlp.ner._
import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet}
import narad.nlp.ling.TaggedToken
import narad.structure.tree.{SpanIndexedTree, Tree}
import collection.mutable
import narad.bp.optimize.Scorable
import narad.util.eval.{AccuracyContainer, MultiAccuracyContainer}
import narad.nlp.parser.metrics.EvalBContainer


class ConstituentTree(node: ConstituentNode, val children: List[ConstituentTree] = List()) extends SpanIndexedTree[ConstituentNode](node, children) with Scorable {

  def label: String = node.label

  def isPreterminal = node.isPreterminal

  def isNonterminal = node.isNonterminal

  def extractRules: HashSet[String] = {
    val rules = new HashSet[String]
    if (!isLeaf && !node.isPreterminal) {
      rules += "%s => %s".format(node.label, children.map(_.node.label).mkString(" "))
      children.foreach { child =>
        rules ++= child.extractRules
      }
    }
    rules
  }

  def coarsenLabels: ConstituentTree = {
    val coarsed = coarsen(node.label)
    if (node.label == coarsed) {
      new ConstituentTree(node, children.map(_.coarsenLabels))
    }
    else {
      if (isNonterminal) {
        new ConstituentTree(new NonterminalNode(coarsed), children.map(_.coarsenLabels))
      }
      else {
        new ConstituentTree(new PreterminalNode(coarsed, node.asInstanceOf[PreterminalNode].word), children.map(_.coarsenLabels))
      }
    }
  }
  /*
      this match {
        case x  NonterminalNode => new ConstituentTree(new NonterminalNode(coarsed), children.map(_.coarsenLabels))
        case x: PreterminalNode => new ConstituentTree(new PreterminalNode(coarsed, x.word), children.map(_.coarsenLabels))
      }
    }
  }
  */

  def coarsen(ll: String): String = {
    if (ll == "-DFL-") return ll
    var l = ll
    while (l.startsWith("^")) {
      l = l.substring(1)
    }
    if (l.contains("|"))
      l = l.substring(0, l.indexOf("|"))
    if (l.contains("-"))
      l = l.substring(0, l.indexOf("-"))
    if (l.contains("="))
      l = l.substring(0, l.indexOf("="))
    if (l.contains("^"))
      l = l.substring(0, l.indexOf("^"))
    return l
  }

  def tags: Iterator[String] = leaves.collect { case l: PreterminalNode => l.label }

  def words: Iterator[String] = leaves.collect { case l: PreterminalNode => l.word }

  def tokens: Iterator[TaggedToken] = leaves.collect { case l: PreterminalNode => TaggedToken(l.word, l.label) }

  def score(other: Scorable): EvalBContainer = {
    other match {
      case that: ConstituentTree => EvalBContainer.construct(goldTree=that, testTree=this)
      case _=> new EvalBContainer()
    }
  }
  /*
      leaves.collect { l =>
        l match {
          case x : PreterminalNode => new Token(x.word, x.label)
          case _ => None
          }
        }
      }
  */
  // pairs collect { case (x: Int, y: String) => (x, y) }

  def setYield(words: Array[String], tags: Array[String], offset: Int = 0): ConstituentTree = {
    println("setting yield")
    var tally = 0
    node match {
      case x: NonterminalNode => {
        println(" set nonterm")
        new ConstituentTree(node, children.map{ c =>
          val child = c.setYield(words, tags, offset + tally)
          tally += c.width
          child
        })
      }
      case x: PreterminalNode => {
        println("offset = " + offset)
        new ConstituentTree(new PreterminalNode(tags(offset), words(offset)))
      }
    }
  }

  override def index: Array[Array[ArrayBuffer[Span]]] = {
    val ispans = Array.fill(length+1,length+1)(new ArrayBuffer[Span])
    var numLeaves = 0
    for (t <- leafFirstSearch) {
      if (t.isLeaf) {
        ispans(numLeaves)(numLeaves+1) += new Span(numLeaves, numLeaves+1, t.label, 0)
        numLeaves += 1
      }
      else {
        val len = t.length
        val height = ispans(numLeaves-len)(numLeaves).size
        ispans(numLeaves-len)(numLeaves) += new Span(numLeaves-len, numLeaves, t.label, height)
      }
    }
    ispans
  }

  override def toString(): String = {
    node match {
      case x: NonterminalNode => return "(%s %s)".format(x.label, getChildren.map(_.toString()).mkString(" "))
      case x: PreterminalNode => return "(%s %s)".format(x.label, x.word)
    }
  }
/*
    if (isLeaf) {
      return "(%s)".format(node.toString())
    }
    else {
      return "(%s %s)".format(node.toString(), children.map(_.toString()).mkString(" "))
    }
  }
*/

  def slice(i: Int, j: Int): ConstituentTree = {
    //    println("SLICING THIS: " + this.toString())
    //    println("Slicing %d -> %d".format(s, e))
    val ospans = toSpans.toArray
    //    println("ORIGINAL SPANS\n")
    //    ospans.foreach(sp => println("  -o- " + sp.toString()))

    val fspans = ospans.filter{ s => s.start >= i && s.end <= j && s.width > 1} // && (span.end-span.start > 1 || span.isUnary)}
    //    println("FILTERED SPANS\n")
    //    ospans.foreach("  -f- " + _.toString())

    //    val ss = spans.toArray.filter{span => span.start >= s && span.end <= e && (span.end-span.start > 1 && !span.isUnary)}
    val ss2 = fspans.map{span => Span(span.start-i, span.end-i, span.label, span.height)}
    //(for (t <- this) yield Span(t.start(), t.end(), t.label(), t.isUnary)).toArray //.filter{span => span.start >= s && span.end <= end}
    //    println(ss2.mkString("\n"))
    val t = TreeFactory.constructFromSpans(ss2, j-i, words.slice(i, j).toArray, tags.slice(i, j).toArray)
  //  t.annotateWithIndices(0)
//    t.setYield(words.slice(i, j).toArray, tags.slice(i, j).toArray)
    t
  }


  /*
    def binarize(mode: String = "RIGHT_0MARKOV"): ConstituentTree = {
    if (children.size > 2) {
      val grandchildren = children.slice(1, children.size)
      var binLabel = if (isBinarized) node.label else "@%s".format(node.label)

      return new ConstituentTree(node, List[ConstituentTree](children(0).binarize(), new ConstituentTree(new NonterminalNode(binLabel), grandchildren).binarize()))
    }
    else{
      return new ConstituentTree(node, children.map(_.binarize()))
    }
  }
   */

  def binarize(mode: String = "RIGHT_0MARKOV"): ConstituentTree = {
    if (children.size > 2) {
      val grandchildren = children.slice(1, children.size)
      mode match {
        case "RIGHT_0MARKOV" => {
          return new ConstituentTree(node, List[ConstituentTree](children(0).binarize(),
                                            new ConstituentTree(new NonterminalNode("@%s".format(node.label)),
                                            grandchildren).binarize()))
        }
        case "RIGHT_SINGLE" => {
          return new ConstituentTree(node, List[ConstituentTree](children(0).binarize(),
            new ConstituentTree(new NonterminalNode("@"),
              grandchildren).binarize()))
        }
      }
    }
    else{
      return new ConstituentTree(node, children.map(_.binarize()))
    }
  }

  def isBinarized: Boolean = node.label.contains("@")

  def removeUnaryChains(): ConstituentTree = {
    return new ConstituentTree(node,
      if (children.size == 1) {
        val uh = unaryHelper()
        unaryHelper().map(_.removeUnaryChains())
      }
      else {
        children.map(_.removeUnaryChains())
      })
  }

  def unaryHelper(): List[ConstituentTree] = {
    if (children.size == 0) {
      return List(this)
    }
    if (children.size == 1) {
      children(0).unaryHelper()
    }
    else {
      return children
    }
  }

  def removeNones(): ConstituentTree = {
    val nchildren = children.map(_.removeNones()).filter(_ != null.asInstanceOf[ConstituentTree])
    if (label == "-NONE-" || label == "-RRB-" || label == "-LRB-" || (children.size > 0 && nchildren.size == 0)) {
      return null.asInstanceOf[ConstituentTree]
    }
    else {
      return new ConstituentTree(node, nchildren)
    }
  }

  override def breadthFirstSearch = super.breadthFirstSearch.map(_.asInstanceOf[ConstituentTree])

  override def depthFirstSearch = super.depthFirstSearch.map(_.asInstanceOf[ConstituentTree])

  override def leafFirstSearch = super.leafFirstSearch.map(_.asInstanceOf[ConstituentTree])

  def getChildren = children.map(_.asInstanceOf[ConstituentTree])
}



object ConstituentTree {

  // (S  (NP (DET the) (NN cat)) (VP (VB scratches) (NP (DET the) (NN dog)))
  // (0,1), (1,2), (2,3), (3,4), (4,5), (0,2), (3,5), (4,5), (0,5)
  def main(args: Array[String]) {
    val TheTree1 = new ConstituentTree(PreterminalNode("DET", "the"))
    val CatTree = new ConstituentTree(PreterminalNode("NN", "cat"))
    val ScratchesTree = new ConstituentTree(PreterminalNode("VB", "scratches"))
    val TheTree2 = new ConstituentTree(PreterminalNode("DET", "the"))
    val DogTree = new ConstituentTree(PreterminalNode("NN", "dog"))

    val TheCatTree = new ConstituentTree(NonterminalNode("NP"), List(TheTree1, CatTree))
    val TheDogTree = new ConstituentTree(NonterminalNode("NP"), List(TheTree2, DogTree))
    val ScratchesTheDogTree = new ConstituentTree(NonterminalNode("VP"), List(ScratchesTree, TheDogTree))
    val t = new ConstituentTree(NonterminalNode("S"), List(TheCatTree, ScratchesTheDogTree))

    println(t.toString())
    println
    val len = t.length
    for (i <- 0 to len; j <- 0 to len) {
      println("i = %d, j = %d : ".format(i,j) + t.containsSpan(i, j))
    }
  }
}



object TreeFactory {

  def buildTree(label: String = "SPAN", word: String = "", children: List[ConstituentTree] = List()): ConstituentTree = {
    if (word == "") {
      new ConstituentTree(new NonterminalNode(label), children)
    }
    else {
      new ConstituentTree(new PreterminalNode(label, word), children)
    }
  }

  def constructFromSpans(spans: Array[Span], slen: Int, words: Array[String] = Array(), tags: Array[String] = Array()): ConstituentTree = {
    return TreeFactory.buildTree(label="TOP", children=findChildren(spans.sortBy(sp => (sp.width * 1000) + sp.height).reverse, 0, slen, words, tags))
  }

  def findChildren(spans: Array[Span], start: Int, end: Int, words: Array[String] = Array(), tags: Array[String] = Array()): List[ConstituentTree] = {
    val children = new ArrayBuffer[ConstituentTree]
    val max = findMaxSpan(spans, start, end)
    max match {
      case None => {
        if (words.isEmpty || tags.isEmpty) {
          List.fill(end-start)(TreeFactory.buildTree(label="TAG", word="TMP", children=List()))
        }
        else {
          (start until end).map { i => TreeFactory.buildTree(label=tags(i), word=words(i), children=List()) }.toList
        }
      }
      case Some(maxspan) => {
        if (maxspan.start > start) {
          val leftChildren = findChildren(spans, start, maxspan.start, words, tags)
          if (leftChildren.size > 0) children ++= leftChildren
        }
        val cchildren = findChildren(spans.filter(_ != maxspan), maxspan.start, maxspan.end, words, tags)
        children += TreeFactory.buildTree(label=maxspan.label, children=cchildren)
        if (maxspan.end < end) {
          val rightChildren = findChildren(spans, maxspan.end, end, words, tags)
          if (rightChildren.size > 0) children ++= rightChildren
        }
        children.toList
      }
    }
  }

  def findMaxSpan(spans: Array[Span], start: Int, end: Int): Option[Span] = {
    for (span <- spans) {
      if (span.start >= start && span.end <= end) return Some(span)
    }
    return None
  }
}





























/*
case class ConstituentTree(annotation: Annotation, children: Array[ConstituentTree]) extends Scorable with Iterable[ConstituentTree] { // Iterator[ConstituentTree]{ //extends Tree[Constituent](children)
  var len = -1
  var ispans = null.asInstanceOf[Array[Array[ArrayBuffer[Span]]]]
  var indexed = false

  def index() = {
    var l = tokens().size+1+1
    ispans = Array.fill(l,l)(new ArrayBuffer[Span])
    for (t <- this) {
      ispans(t.start())(t.end()) += Span(t.start(), t.end(), t.label())
    }
    for (i <- 0 until ispans.size; j <- 0 until ispans(i).size) {
      for (k <- 0 until ispans(i)(j).size) ispans(i)(j)(k).height = ispans(i)(j).size - k - 1
    }
    len = l
    indexed = true
  }

  def label(): String = annotation("label")
  def setLabel(l: String) = annotation("label") = l

  def word(): String = annotation("word")
  def start(): Int = annotation("start").toInt
  def end(): Int = annotation("end").toInt
  def width(): Int = end - start
  def isBinarized: Boolean = label().contains("@")
  def isUnary: Boolean = isUnaryRewrite
  def isRoot: Boolean = label == "TOP"
//	def toSpan: Span = Span(start(), end(), label(), isUnary)
  def words = tokens.map(_.word)
  def tags = tokens.map(_.pos)
  def slen = tokens().size

  def nonterminals: Iterator[ConstituentTree] = {
    this.iterator.filter(!_.isLeaf)//.map(_.label)
    //(for (t <- this if !t.isLeaf) yield t.label())
  }

  def preterminals: Iterator[ConstituentTree] = {
    this.iterator.filter(_.isLeaf)//.map(_.label)
    //    (for (t <- this if t.isLeaf) yield t.label())
  }

  def rules: Iterator[Rule] = {
    preterminals.map(p => Rule(p.label(), p.children.map(_.label())))
    //    (for (t <- this if !t.isPreterminal) yield Rule(t.label(), t.children.map(_.label())).toString)
  }

  /*
  def spans: Iterator[Span] = {
    (for (t <- this) yield Span(t.start(), t.end(), t.label(), t.isUnary))
  }
  */

  def spans: Array[Span] = {
    if (!indexed) index()
    val sa = new ArrayBuffer[Span]()
    for (i <- 0 until ispans.size; j <- 0 until ispans(i).size; k <- 0 until ispans(i)(j).size) sa += ispans(i)(j)(k)
    sa.toArray
  }

  def removeTop = {
    assert(children.size == 1, "Error: cannot remove top tree node because it has more than one child: \n%s".format(toString()))
    children(0)
  }

  def wrapTop(topLabel: String) = {
    val ann = new Annotation
    ann += "label" -> topLabel
    new ConstituentTree(ann, Array(this))
  }

  def labels(start: Int, end: Int): HashSet[String] = {
    if (!indexed) index()
    val sets = new HashSet[String]
    ispans(start)(end).map(_.label).foreach(sets += _)
    sets
  }



  def isLeaf: Boolean = {
    return children.size == 0
  }

  def isPreterminal: Boolean = {
    return children.size == 0
  }

  def containsSpan(s: Int, e: Int): Boolean = {
    if (!indexed)  index()
    if (e > len) return false
    ispans(s)(e).size > 0
  }

  def containsSpan(start: Int, end: Int, label: String): Boolean = {
    if (!indexed)  index()
    if (end	> len) return false
    ispans(start)(end).toArray.filter(_.label == label).size > 0
  }

  def containsUnarySpan(start: Int, end: Int): Boolean = {
    if (!indexed)  index()
    if (end > len) return false
    ispans(start)(end).toArray.filter(_.isUnary).size > 0
  }

  def containsUnarySpan(start: Int, end: Int, label: String): Boolean = {
    if (end	> len)
      return false
    if (!indexed) index()
    val uns = ispans(start)(end).toArray.filter(_.isUnary)
    return uns.size > 0 && uns(0).label == label
//		spans(start)(end).toArray.filter(s => s.label == label && s.isUnary).size > 0
  }


  override def slice(s: Int, e: Int): ConstituentTree = {
//    println("SLICING THIS: " + this.toString())
//    println("Slicing %d -> %d".format(s, e))
    val ospans = spans.toArray
//    println("ORIGINAL SPANS\n")
//    ospans.foreach(sp => println("  -o- " + sp.toString()))

    val fspans = ospans.filter{span => span.start >= s && span.end <= e && (span.end-span.start > 1 || span.isUnary)}
//    println("FILTERED SPANS\n")
//    ospans.foreach("  -f- " + _.toString())

//    val ss = spans.toArray.filter{span => span.start >= s && span.end <= e && (span.end-span.start > 1 && !span.isUnary)}
    val ss2 = fspans.map{span => Span(span.start-s, span.end-s, span.label, span.height)}
     //(for (t <- this) yield Span(t.start(), t.end(), t.label(), t.isUnary)).toArray //.filter{span => span.start >= s && span.end <= end}
//    println(ss2.mkString("\n"))
    val t = TreeFactory.constructFromSpans(ss2, e-s)
    t.annotateWithIndices(0)
    t.setYield(words.slice(s,e), tags.slice(s,e))
    t
  }

  /*
    for (t <- this) {
      if (t.start() == s) return t.clone()
    }
    return null.asInstanceOf[ConstituentTree]
  }

  override def clone: ConstituentTree = {
    new ConstituentTree(this.annotation.clone(), this.children.map(_.clone))
  }
/*
  override def slice(s: Int, e: Int): ConstituentTree = {
    val ss = (for (t <- this) yield Span(t.start(), t.end(), t.label(), t.isUnary)).toArray //.filter{span => span.start >= s && span.end <= end}
    println(ss.mkString("\n"))
    TreeFactory.constructFromSpans(ss, slen)
  }
  */


 /*
  override def slice(s: Int, e: Int): ConstituentTree = {
    println("-- %d before".format(children.size))
    val kids = this.children.toArray.filter{c =>
      println("Child = %s (%d, %d)".format(c.label, c.start, c.end))
      c.start() >= s || c.end() <= e}.map(_.slice(s, e)).toArray
    println("-- %d after".format(kids.size))
    println

    return ConstituentTree(this.annotation, kids)

*/
    /*
    new ConstituentTree(this.annotation,
                        this.children.filter{c =>
                          println("Child = %s (%d, %d)".format(c.label, c.start, c.end))
                          c.start() >= s || c.end() <= e}.map(_.slice2(s, e)).toArray)
                          */

    */

  def tokens(): Array[Token] = {
    val tokens = new ArrayBuffer[Token]
    if (isLeaf) {
      if (annotation.contains("word")){
        tokens += new Token(word(), label())
      }
      else{
        tokens += new Token("$WORD$", label())
      }
    }
    else {
      for (child <- children) { tokens ++= child.tokens }
    }
    tokens.toArray
  }

  def taggedTokens(): Array[TaggedToken] = {
    val tokens = new ArrayBuffer[TaggedToken]
    if (isLeaf) {
      if (annotation.contains("word")){
        tokens += new TaggedToken(word(), label())
      }
      else{
        tokens += new TaggedToken("$WORD$", label())
      }
    }
    else {
      for (child <- children) { tokens ++= child.taggedTokens }
    }
    tokens.toArray
  }


  def annotateWithIndices(idx: Int = 0): Int = {
    if (isLeaf) {
      annotation += "start" -> idx.toString
      annotation += "end"   -> (idx+1).toString
      return idx+1
    }
    else{
      var tally = idx
      for (child <- children) {
        tally = child.annotateWithIndices(tally)
      }
      annotation += "start" -> idx.toString
      annotation += "end" -> tally.toString
      return tally
    }
  }

  def coarsen(ll: String): String = {
      var l = ll
      if (l.contains("-"))
        l = l.substring(0, l.indexOf("-"))
      if (l.contains("="))
        l = l.substring(0, l.indexOf("="))
      if (l.contains("^"))
        l = l.substring(0, l.indexOf("^"))
      return l
  }

  def coarsenLabels(): Int = {
    setLabel(coarsen(label()))
    for (child <- children) {
      child.coarsenLabels()
    }
    return 1
  }

  def setYield(words: Array[String], tags: Array[String]) {
    if (isLeaf) {
      val idx = annotation("start").toInt
//			println(words.size + " / " + tags.size + " vs. " + idx)
      annotation += "label" -> tags(idx)
      annotation += "word"  -> words(idx)
    }
    else {
      children.map(_.setYield(words, tags))
    }
  }

  def replaceTags(tags: Array[String]) {
    if (isLeaf) {
      val idx = annotation("start").toInt
      annotation += "label" -> tags(idx)
    }
    else {
      children.map(_.replaceTags(tags))
    }
  }

  def binarize(): ConstituentTree = {
    if (children.size > 2) {
      val grandchildren = children.slice(1, children.size)
      var binLabel = if (isBinarized) label() else "@%s".format(label())
      var ann = new Annotation
      ann += "label" -> binLabel
      return new ConstituentTree(annotation, Array[ConstituentTree](children(0).binarize(), new ConstituentTree(ann, grandchildren).binarize()))
    }
    else{
      return new ConstituentTree(annotation, children.map(_.binarize()))
    }
  }

  def removeTopNode(): ConstituentTree = {
    if (label == "TOP") {
      assert(children.size == 1, "TOP node has more than one child, when it should be a unary wrapper.")
      return children(0)
    }
    else {
      return this
    }
  }

  def removeUnaryChains(): ConstituentTree = {
//		System.err.println("Unary removal...")
//		System.out.println("%s(%d,%d)".format(label, start, end))
//		if (children.size == 1 && children(0).children.size ==1 && children(0).children(0.isLeaf)
      return new ConstituentTree(annotation,
      if (children.size == 1) {
//				unaryHelper().map(_.removeUnaryChains)
        val uh = unaryHelper()
//				println("-- " + uh.label)
        unaryHelper().map(_.removeUnaryChains())
      }
      else {
        children.map(_.removeUnaryChains())
      })
  }


    def unaryHelper(): Array[ConstituentTree] = {
      if (children.size == 0) {
        return Array(this)
      }
      if (children.size == 1) {
        children(0).unaryHelper()
      }
      else {
        return children
      }
    }



  /*
  def unaryHelper(): ConstituentTree = {
    if (children.size == 1) {
      return children(0).unaryHelper
    }
    else {
      return this
    }
  }
  */
/*
    return new ConstituentTree(annotation, children.map { child =>
      if (child.children.size == 1) {
        child.children(0).removeUnaryChains
      }
      else {
        child.removeUnaryChains
      }
      })
    }
*/



  def isUnaryRewrite: Boolean = children.size == 1 && !isPreterminal
//		def isUnaryRewrite: Boolean = children.size == 1 && children(0).children.size <= 1

    def removeNones(): ConstituentTree = {
      val nchildren = children.map(_.removeNones()).filter(_ != null.asInstanceOf[ConstituentTree])
      if (label == "-NONE-" || (children.size > 0 && nchildren.size == 0)) {
        return null.asInstanceOf[ConstituentTree]
      }
      else {
        return new ConstituentTree(annotation, nchildren)
      }
    }

def derivation(idx: Int): Array[ConstituentTree] = {
  if (isLeaf && start == idx) {
    return Array(this)
  }
  else {
    val buffer = new ArrayBuffer[ConstituentTree]
    for (child <- children) {
      buffer ++= child.derivation(idx)
    }
    if (buffer.toArray.size > 0) {
      buffer += this
    }
    return buffer.toArray
  }
}

def depthFirstQueue: Array[ConstituentTree] = {
  val trees = new ArrayBuffer[ConstituentTree]
  trees += this
  children.foreach(trees ++= _.depthFirstQueue)
  trees.toArray
}

def printAnnotations() {
  for (tree <- flatten) {
    println(tree.annotation)
  }
}

def flatten = depthFirstQueue

  /*
def nonterminals: Array[String] = {
  val buf = new ArrayBuffer[String]
  buf += label
  for (child <- children) {
    buf ++= child.nonterminals
  }
  buf.toArray
}
*/

def setLabels(l: String) {
  this.annotation += "label" -> l
  children.foreach(_.setLabels(l))
}

def extractRules: HashSet[String] = {
  val rules = new HashSet[String]
  if (!isLeaf && !isPreterminal) {
    rules += "%s => %s".format(label(), children.map(_.label()).mkString(" "))
    children.foreach { child =>
      rules ++= child.extractRules
    }
  }
  rules
}



def rulesWithCounts: HashMap[String, Int] = {
  val r = new HashMap[String, Int]
  for (t <- this if !t.isPreterminal) {
    val rule = Rule(t.label(), t.children.map(_.label())).toString
    if (r.contains(rule)) {
      r(rule) += 1
    }
    else {
      r(rule) = 1
    }
  }
  return r
}


override def toString(): String = {
  if (isPreterminal) {
    if (annotation.contains("word")){
      return "(%s %s)".format(label(), word())
    }
    else{
      return "(%s %s)".format(label(), "word")
    }
  }
  else {
    return "(%s %s)".format(label(), children.map(_.toString()).mkString(" "))
  }
}

def toBareString: String = {
  if (isPreterminal) {
    return "(TAG word)"
  }
  else {
    return "(%s %s)".format("CST", children.map(_.toBareString).mkString(" "))
  }
}

  def iterator = depthFirstQueue.iterator

  def score(other: Scorable): EvalBContainer = {
    other match {
      case that: ConstituentTree => EvalBContainer.construct(goldTree=that, testTree=this)
      case _=> new EvalBContainer()
    }
  }
}
*/






/*
    other match {
      case x : ConstituentTree => {
        val testTree = this
        val goldTree = x
//        println("GOLD TREE: " + goldTree)
//        println("TEST TREE: " + testTree)
//        println

        goldTree.annotateWithIndices(0)
        testTree.annotateWithIndices(0)
        val goldTokens = goldTree.tokens()
        val testTokens = testTree.tokens()
        val goldSpans = goldTree.spans.toArray
        val testSpans = testTree.spans.toArray

        val labels = (goldSpans ++ testSpans).map(_.label).distinct

        assert(goldTokens.size == testTokens.size, "Trees are not over sentences of the same length (%d gold vs %d test).".format(goldTokens.size, testTokens.size))
        val slen = goldTokens.size
        val e = new EvalBContainer
        e("tags") = goldTokens.map(_.pos).zip(testTokens.map(_.pos)).filter(p => p._1 == p._2).size
        e("tokens") = slen
        e("tagging accuracy") = e.count("tags") / e.count("tokens")


        val lfound = new HashSet[Int]
        val ufound = new HashSet[Int]
        for (gspan <- goldSpans; tidx <- 0 until testSpans.size) {
          val tspan = testSpans(tidx)
          if (gspan == tspan && !lfound.contains(tidx)) {
            e.increment("labeled correct")
            e.increment(gspan.label + " correct")
            lfound += tidx
          }
          if (gspan.start == tspan.start && gspan.end == tspan.end && !ufound.contains(tidx)) {
            e.increment("unlabeled correct")
            ufound += tidx
          }
        }
        e("gold comps") = goldSpans.size
        e("test comps") = testSpans.size


        var ccount = 0
        for (tspan <- testSpans) {
          if (goldSpans.exists(_.crosses(tspan))) ccount += 1
        }
        e("crosses") = ccount
        if (ccount == 0) e.increment("no crosses")
        if (ccount <= 2) e.increment("<=2 crosses")


        for (label <- labels) {
          e("gold %s comps".format(label)) = goldSpans.filter(_.label == label).size
          e("test %s comps".format(label)) = testSpans.filter(_.label == label).size
        }


        val up  = e.count("unlabeled correct") / e.count("test comps")
        val ur  = e.count("unlabeled correct") / e.count("gold comps")
        val uf1 = if ((up + ur) == 0) 0 else 2 * ((up * ur) / (up + ur))
        e("up") = up
        e("ur") = ur
        e("uf1") = uf1

        val lp  = e.count("labeled correct") / e.count("test comps")
        val lr  = e.count("labeled correct") / e.count("gold comps")
        val lf1 = if ((lp + lr) == 0) 0 else 2 * ((lp * lr) / (lp + lr))
        e("lp") = lp
        e("lr") = lr
        e("lf1") = lf1
        if (lf1 == 1) e.increment("complete match")
        println(e.toString)
        e
      }
      case _ => {
        new EvalBContainer()
      }
    }
    */


/*
// ------------ Iteration ------------- //

  var idx = 0
  var iterover = null.asInstanceOf[Array[ConstituentTree]]

  def hasNext: Boolean = {
    if (idx == 0) {
      iterover = depthFirstQueue
    }
    val nextCheck = idx < iterover.size
    if (nextCheck == false) {
      idx = 0
    }
    nextCheck
  }

  def next(): ConstituentTree = {
    val t = iterover(idx); idx += 1; t
  }

  def reset = idx = 0
     */







/*
  def labels(start: Int, end: Int): HashSet[String] = {
    val labels = new HashSet[String]
    for (subtree <- this) {
      if (start == subtree.start && end == subtree.end) {
        labels += subtree.label
      }
    }
    labels
  }
*/
