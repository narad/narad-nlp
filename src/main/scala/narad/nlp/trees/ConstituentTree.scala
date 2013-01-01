package narad.nlp.trees

//import narad.nlp.ner._
import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet}
import narad.nlp.ling.TaggedToken
import narad.structure.tree.Tree


case class ConstituentNode(label: String) {}

case class PreterminalNode(label: String, word: String) {}

trait TreeAnnotation {

  def label
}

case class ConstituentTree(annotation: Annotation, children: Array[ConstituentTree]) extends Iterator[ConstituentTree]{ //extends Tree[Constituent](children)
	

	def label(): String = annotation("label")
	def word(): String = annotation("word")
	def start(): Int = annotation("start").toInt
	def end(): Int = annotation("end").toInt
	def width(): Int = end - start
	def isBinarized: Boolean = label().contains("@")
	def isUnary: Boolean = isUnaryRewrite
	def isRoot: Boolean = label == "TOP"
	def toSpan: Span = Span(start(), end(), label(), isUnary)
	
	def slen = tokens().size

  def getSpans: Iterator[Span] = {
		return (for (t <- this if !t.isLeaf) yield Span(t.start(), t.end(), t.label(), t.isUnary))
	}

	def removeTop = {
		assert(children.size == 1, "Error: cannot remove top tree node because it has more than one child: \n%s".format(toString()))
		children(0)
	}
	
	def labels(start: Int, end: Int): HashSet[String] = {
		if (!indexed) index()
		val sets = new HashSet[String]
		spans(start)(end).map(_.label).foreach(sets += _)
		sets
	}

  var len = -1
	var spans = null.asInstanceOf[Array[Array[ArrayBuffer[Span]]]]
	var indexed = false
  def index() = {
		var l = tokens().size+1+1
		spans = Array.fill(l,l)(new ArrayBuffer[Span])
		for (t <- this) {
			spans(t.start())(t.end()) += Span(t.start(), t.end(), t.label(), t.isUnary)
		}
		len = l
		indexed = true 
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
		spans(s)(e).size > 0
	}
	
	def containsSpan(start: Int, end: Int, label: String): Boolean = {
		if (!indexed)  index()
		if (end	> len) return false
		spans(start)(end).toArray.filter(_.label == label).size > 0
	}
	
	def containsUnarySpan(start: Int, end: Int): Boolean = {
		if (!indexed)  index()
    if (end > len) return false
		spans(start)(end).toArray.filter(_.isUnary).size > 0
	}

	def containsUnarySpan(start: Int, end: Int, label: String): Boolean = {
    if (end	> len)
    	return false
		if (!indexed) index()
		val uns = spans(start)(end).toArray.filter(_.isUnary)
		return uns.size > 0 && uns(0).label == label
//		spans(start)(end).toArray.filter(s => s.label == label && s.isUnary).size > 0
	}

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
		
def nonterminals: Array[String] = {
	val buf = new ArrayBuffer[String]
	buf += label
	for (child <- children) {
		buf ++= child.nonterminals
	}
	buf.toArray
}

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

def rules: Iterator[String] = {
	(for (t <- this if !t.isPreterminal) yield Rule(t.label(), t.children.map(_.label())).toString)
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

}


object TreeFactory {
		
	def buildTree(label: String = "SPAN", word: String = "", children: Array[ConstituentTree] = Array()): ConstituentTree = {
		val ann = new Annotation
		ann += "label" -> label
		ann += "word"  -> word
		new ConstituentTree(ann, children)
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
