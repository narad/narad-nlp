package narad.nlp.trees

//import narad.nlp.ner._
import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet}
import narad.nlp.ling.TaggedToken
import narad.structure.tree.Tree
import collection.mutable


case class ConstituentNode(label: String) {}

case class PreterminalNode(label: String, word: String) {}

trait TreeAnnotation {

  def label
}

case class ConstituentTree(annotation: Annotation, children: Array[ConstituentTree]) extends Iterable[ConstituentTree] { // Iterator[ConstituentTree]{ //extends Tree[Constituent](children)
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

  def coarsen(l: String): String = {
    if (l.contains("-")) {
       l.substring(0, l.indexOf("-"))
    }
    else {
      l
    }
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

}

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



object TreeFactory {
		
	def buildTree(label: String = "SPAN", word: String = "", children: Array[ConstituentTree] = Array()): ConstituentTree = {
		val ann = new Annotation
		ann += "label" -> label
		ann += "word"  -> word
		new ConstituentTree(ann, children)
	}

  def constructFromSpans(spans: Array[Span], slen: Int): ConstituentTree = {
//    println("Spans incoming to construct from spans:")
//    spans.foreach(p => println(" -- " + p + ": h=" + p.height))
    return TreeFactory.buildTree(label="TOP",
      children=findChildren(spans.sortBy(sp => (sp.width * 1000) + sp.height).reverse, 0, slen))
  }

  def findChildren(spans: Array[Span], start: Int, end: Int): Array[ConstituentTree] = {
    val children = new ArrayBuffer[ConstituentTree]
    val max = findMaxSpan(spans, start, end)
    max match {
      case None => return Array.fill(end-start)(TreeFactory.buildTree(label="TAG", children=Array()))
      case Some(maxspan) => {
        if (maxspan.start > start) {
          val leftChildren = findChildren(spans, start, maxspan.start)
          if (leftChildren.size > 0) children ++= leftChildren
        }
        val cchildren = findChildren(spans.filter(_ != maxspan), maxspan.start, maxspan.end)
        children += TreeFactory.buildTree(label=maxspan.label, children=cchildren)
        if (maxspan.end < end) {
          val rightChildren = findChildren(spans, maxspan.end, end)
          if (rightChildren.size > 0) children ++= rightChildren
        }
        return children.toArray
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
