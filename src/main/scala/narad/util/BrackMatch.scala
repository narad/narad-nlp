/*
import collection.JavaConversions._
import edu.stanford.nlp.trees._
import edu.stanford.nlp.parser.lexparser._
import edu.stanford.nlp.parser.metrics._
import java.io._
import java.util.ArrayList
import scala.collection.mutable.{ArrayBuffer,HashMap,HashSet,ListBuffer}
import scala.util.matching.Regex
import scala.io.Source


object BrackMatch {

  case class Token(word: String, pos: String){
    override def toString: String = "(%s %s)".format(word, pos)
  }

  def main(args: Array[String]) : Unit = {
    val fileReader = new BufferedReader(new InputStreamReader(System.in, "UTF-8"))
    
    //	for (i <- 0 until nterms.size){ println ("%d %s".format(i, nterms(i)) )}
    
    val treeReader = new LabeledScoredTreeReaderFactory().newTreeReader(fileReader)
    
    //     val op = new Options(new EnglishTreebankParserParams())
    //     val binarizer = new TreeAnnotatorAndBinarizer(op.tlpParams, op.forceCNF, !Train.outsideFactor(), true)
    //     binarizer.setDoSelectiveSplit(false)
    
    val treebankLanguagePack = (new Options(new EnglishTreebankParserParams())).tlpParams.treebankLanguagePack()
    val headFinder = treebankLanguagePack.headFinder()
    val insideFactor = true        // true: DT JJ NN -> DT "JJ NN", false: DT "DT"
    val markovFactor = true
    val markovOrder = 1
    val useWrappingLabels = true
    val unaryAtTop = false
    val doSelectiveSplit = false
    val selectiveSplitThreshold = 100 // Arbitrary - should not be used
    val markFinalStates = false
    val binarizer = new TreeBinarizer(headFinder, treebankLanguagePack, insideFactor, markovFactor, markovOrder, useWrappingLabels, unaryAtTop, selectiveSplitThreshold, markFinalStates)
    binarizer.setDoSelectiveSplit(doSelectiveSplit)

    while(true){
      try {
   	val tree = treeReader.readTree()
	val tokens = getTokens(tree)
	val length = tokens.length
	featurize(binarizer.transformTree(tree))
	//featurize(tree, nterms)
      }
      catch{
	// If null, treeReader has been exhausted, exit
	case e:java.lang.NullPointerException => System.exit(0)
      }
    }
  }
  
  def featurize(tree: Tree) {
    val tokens = getTokens(tree)
    val length = tokens.length
    //	  println(tokens.mkString(" "))
    val features = new ArrayBuffer[String]
    print("@slen\t")
    println(length)

//     for ( con <- tree.constituents() ) {
//       println("brack(%d,%d)".format(con.start, con.end))
//     }

    val upper = if ( length > 11 ) 11 else length
    for ( width <- 2 until upper; start <- 0 to (length - width)) {
      // for (start <- 0 until length; end <- start+2 until length){
      val end = start + width
      println("agree(%d,%d)\t%s".format(start, end, getBracketFeatures(tree, start, end)))
    }
    println()
  }


  def getBracketFeatures(tree: Tree, start: Int, end: Int): String = {
    val window = 3
    val sent = getTokens(tree)
    val features = new ArrayBuffer[String]

    //   Create an array of the tokens appearing prior
    //   to the start of this var's span, indexed by
    //   proximity to the span.
    val pBuf = new ArrayBuffer[Token]
    for (i <- 1 to window){
      if(start-i > 0){
	pBuf += sent(start-i)
      }
      else{
	val j = (start - i).abs
	pBuf += Token("<START%d>".format(j), "<START%d>".format(j))
      }
    }

    //  Create the analogous array for tokens after
    //  the span.
    val sBuf = new ArrayBuffer[Token]
    for (i <- 1 to window){
      if(end+i < sent.size){
	sBuf += sent(end+i)
      }
      else{
	val j = (sent.size - end - i).abs
	sBuf += Token("<END%d>".format(j), "<END%d>".format(j))
      }
    }

    val prevs = pBuf.toArray
    val succs = sBuf.toArray

    // CCM features?
    // features += "OUTSIDE_POS-%s_%s".format(prevs(0).pos, succs(0).pos)
    // val foo = new ArrayBuffer[String]
    // for ( i <- start until end ) {
    //   foo += sent(i).pos
    // }
    // features += "INSIDE_POS-" + foo.mkString("_")
    // return features.mkString(" ")

    // Agreement bias
    features += "AGREE"

    // Length Features
    features += "AG_SPAN_SIZE-%d".format(end - start)

    // Span-Border Features
    // features += "AG_START_WORD-%s".format(sent(start).word)
    features += "AG_START_POS-%s".format(sent(start).pos)
    features += "AG_START_CPOS-%s".format(sent(start).pos.substring(0,1))

    features += "AG_START_WORDPOS-%s".format(sent(start).word +"_" + sent(start).pos)
    features += "AG_START_WORDCPOS-%s".format(sent(start).word +"_" + sent(start).pos.substring(0,1))

    // features += "AG_END_WORD-%s".format(sent(end-1).word)
    features += "AG_END_POS-%s".format(sent(end-1).pos)
    features += "AG_END_CPOS-%s".format(sent(end-1).pos.substring(0,1))

    // features += "END_WORDPOS-%s".format(sent(end-1).word + "_" + sent(end-1).pos)
    // features += "END_WORDCPOS-%s".format(sent(end-1).word + "_" + sent(end-1).pos.substring(0,1))

    // features += "STARTEND_WORD-%s_%s".format(sent(start).word, sent(end-1).word)
    features += "AG_STARTEND_POS-%s_%s".format(sent(start).pos, sent(end-1).pos)
    features += "AG_STARTEND_CPOS-%s_%s".format(sent(start).pos.substring(0,1), sent(end-1).pos.substring(0,1))

    // features += "OUTSIDE_WORD-%s_%s".format(prevs(0).word, succs(0).word)
    // features += "OUTSIDE_POS-%s_%s".format(prevs(0).pos, succs(0).pos)
    // features += "OUTSIDE_CPOS-%s_%s".format(prevs(0).pos.substring(0,1), succs(0).pos.substring(0,1))

    // More linear patterns from mcdfeats
    // Conjunctions with length

    features.mkString(" ")
  }

  // Returns list of Tree's tokens, each token holding word and pos.
  def getTokens(tree: Tree): Array[Token] = {
    val tokens = new ArrayBuffer[Token]()
    val tyield = tree.taggedYield()
    for (i <- 0 until tyield.length){
      val taggedWord = tyield.get(i)
      tokens += Token(taggedWord.word(), taggedWord.tag())
    }
    tokens.toArray
  }


  // Returns true if tree contains a constituent span from start to end.
  def hasSpan(tree: Tree, start: Int, end: Int): Boolean = {
//     for(con <- tree.constituents()){
//       if (con.start == start && con.end == end){ return true }
//     }

    for (subtree <- tree){
      val span = findSpan(subtree, tree)
      if (span._1 == start && span._2 == end){
	return true
      }
    }

    return false
  }


  def hasLabeledSpan(tree: Tree, start: Int, end: Int, label: String): Boolean = {
    val outYield = tree.taggedYield()
    for (subtree <- tree){
      if (unlex(subtree.nodeString) == label) {
	val span = findSpan(subtree, tree)
	if (span._1 == start && span._2 == end){
	  return true
	}			
      }
    }
    return false
  }


  def findSpan(subtree: Tree, tree: Tree): Tuple2[Int,Int] = {
    val oYield = tree.taggedYield()
    val iYield = subtree.taggedYield()
    if(iYield.size > 1){
      //	println(oYield.size() + " vs " + iYield.size())
      var scanning = false
      var start = -1
      for (i <- 0 until oYield.size){
	if(scanning){
	  //			println(i-start)
	  if(oYield(i).word != iYield(i-start).word){
	    scanning = false
	    start = -1
	  }
	  if (i-start == iYield.size()-1) {
	    return new Tuple2[Int, Int](start, start + iYield.size)
	  }
	}
	if(oYield(i).word == iYield(0).word){
	  start = i
	  scanning = true
	}
      }
      return new Tuple2[Int, Int](-1,-1)
    }
	else if(iYield.size == 1){
	  val wi = oYield.map(_.word).toList.indexOf(iYield(0).word)
	  return new Tuple2[Int,Int](wi, wi+1)
	}
    return new Tuple2[Int,Int](-1, -1)
  }
  
  def unlex(str: String): String = {
    str.replace(" ", "_").replaceAll("\\[[^\\]]+\\/[^\\]]+\\]", "")
  }

  // Batch evaluation using stanford.nlp implementation of evalb
  def evaluate(guessTrees: List[Tree], goldTrees: List[Tree], writer: PrintWriter) {
    val eval = new Evalb("eval", true)
    (guessTrees zip goldTrees).foreach{case(guess,gold) => eval.evaluate(guess, gold, writer)}
  }
}


*/