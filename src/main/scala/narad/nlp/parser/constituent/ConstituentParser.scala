package narad.nlp.parser.constituent

import narad.bp.optimize.{L2Regularizer, Optimizer}
import narad.bp.util.{PotentialExample, PotentialReader}
import narad.io.tree.TreebankReader
import narad.bp.structure._
import narad.bp.inference.{InferenceOrder, BeliefPropagation}
import collection.mutable.{ArrayBuffer, HashSet}
import java.io.FileWriter
import narad.bp.structure.Potential
import narad.bp.util.PotentialExample
import scala.util.matching.Regex
import narad.nlp.trees._
import narad.io.tree._
import scala.collection.mutable.Queue
import narad.bp.util.index._

object ConstituentParser {

  def main(args: Array[String]) = {
    val params = new ConstituentParserParams(args)
    println(params.MODEL)
    val parser = params.MODEL match {
      case "BRACK" => new ConstituentBracketParser(params)
      case "LABEL" => new ConstituentLabelParser(params)
    }
    if (params.EXTRACT_FEATURES) {
      System.err.println("Extracting features...")
      val reader = new TreebankReader(params.TRAIN_FILE)
      val index = new ArrayIndex[String]()
      val stats = TreebankStatistics.construct(new TreebankReader(params.TRAIN_FILE).iterator)
      parser.extractFeatures(params.TRAIN_FILE, params.TRAIN_FEATURE_FILE, stats, index, params)
      parser.extractFeatures(params.TEST_FILE, params.TEST_FEATURE_FILE, stats, index, params)
      /*
            val trees = util.iterator.toArray
            val btrees = trees.map(_.binarize.removeUnaryChains())
            trees.foreach(_.annotateWithIndices(0))
            btrees.foreach(_.annotateWithIndices(0))
            val blabels = btrees.map(_.nonterminals.map(_.label).toArray).flatten.distinct.sortBy(_.toString)
            val ulabels = trees.map(_.nonterminals.map(_.label).toArray).flatten.distinct.sortBy(_.toString)     // ***FIX THIS LINE
            //      val ulabels = trees.map(_.map(_.label()).toArray).toArray.flatten.distinct.sortBy(_.toString)
            //      val blabels  = btrees.map(_.map(_.label()).toArray).toArray.flatten.distinct.sortBy(_.toString)
            System.err.println("Using labels:\n%s".format(blabels.mkString("\n")))
            parser.extractFeatures(params.TRAIN_FILE, params.TRAIN_FEATURE_FILE, blabels, ulabels, params)
            parser.extractFeatures(params.TEST_FILE, params.TEST_FEATURE_FILE, blabels, ulabels, params)
      */
    }
    else if (params.TRAIN) {
      val optimizer = new Optimizer(parser, params) with L2Regularizer
      val data = new PotentialReader(params.TRAIN_FIDX_FILE)
      optimizer.train(data)
    }
    else if (params.TEST) {
      val optimizer = new Optimizer(parser, params)
      val data = new PotentialReader(params.TEST_FIDX_FILE)
      optimizer.test(data)
    }
  }
}

class ConstituentParser(params: ConstituentParserParams) extends FactorGraphModel with ConstituentBracketFeatures with BeliefPropagation {
  val INDICES_PATTERN = """.*\(([0-9]+),([0-9]+).*""".r
  val LABEL_PATTERN1   = """spanLabel(.*)\(([0-9]+),([0-9]+)\).*""".r
  val LABEL_PATTERN2   = """.*\(([0-9]+),([0-9]+),([0-9]+).*""".r

  val BRACK_PATTERN       = """brack\(([0-9]+),([0-9]+)\)""".r
  val UNARY_PATTERN       = """unary\(([0-9]+),([0-9]+)\)""".r
  val UNARY_LABEL_PATTERN1 = """unaryLabel(.*)\(([0-9]+),([0-9]+)\)""".r
  val UNARY_LABEL_PATTERN2 = """unaryLabel\(([0-9]+),([0-9]+),(.+)\)""".r


  def constructFromExample(ex: PotentialExample, pv: Array[Double]): ModelInstance = {
    val slen   = ex.attributes.getOrElse("slen", "-1").toInt
    val pots = ex.exponentiated(pv)
    println("\n\nEXPONENTED:\n" + pots.mkString("\n"))
    val fg = new FactorGraphBuilder(pots)

    val groups = pots.filter(!_.name.contains("unary")).groupBy{p =>
      val INDICES_PATTERN(start, end) = p.name
      (start.toInt, end.toInt)
    }
    var useLabels = false
    val brackIdxs = Array.ofDim[Int](slen+1, slen+1) //new ArrayBuffer[Int]()
    val labelIdxs = Array.fill[ArrayBuffer[Int]](slen+1, slen+1)(new ArrayBuffer[Int])
    for (width <- 2 to slen; start <- 0 to (slen - width)) {
      val end = start + width
      val gpots = groups((start, end))
      brackIdxs(start)(end) = fg.addVariable("brackvar(%d,%d)".format(start, end), arity=2)
      fg.addUnaryFactor("brackvar(%d,%d)".format(start, end), "brackfac(%d,%d)".format(start, end), gpots(0))
      if (gpots.size > 1) {
        useLabels = true
        for (gpot <- gpots.tail) {
          val LABEL_PATTERN1(label, s, e) = gpot.name
          labelIdxs(start)(end) += fg.addVariable("labelvar(%d,%d,%s)".format(start, end, label), arity=2)
          fg.addUnaryFactor("labelvar(%d,%d,%s)".format(start, end, label), "labelfac(%d,%d,%s)".format(start, end, label), gpot)
        }
      }
    }
    if (useLabels) {
      for (width <- 2 to slen; start <- 0 to (slen - width)) {
        val end = start + width
        //fg.addIsAtMost1Factor(new Regex("brackvar\\(%d,%d\\)".format(start, end)), new Regex("labelvar\\(%d,%d,.+\\)".format(start, end)), "isAtMost(%d,%d)".format(start, end))
        fg.addIsAtMost1FactorByIndices(brackIdxs(start)(end), labelIdxs(start)(end).toArray, "isAtMost(%d,%d)".format(start, end))
      }
    }
 //   fg.addCKYFactor(new Regex("brackvar"), slen=slen)
//    fg.addCKYFactorByIndices(brackIdxs.toArray, slen=slen)


    val useUnaries = false
    if (useUnaries) {
      val ugroups = pots.filter(_.name.contains("unary")).groupBy{p =>
        val INDICES_PATTERN(start, end) = p.name
        (start.toInt, end.toInt)
      }
      for (start <- 0 until slen) {
        val end = start + 1
        val upots = ugroups((start, end))
        brackIdxs(start)(end) = fg.addVariable("unaryvar(%d,%d)".format(start, end), arity=2)
        fg.addUnaryFactor("unaryvar(%d,%d)".format(start, end), "unaryfac(%d,%d)".format(start, end), upots(0))
        if (upots.size > 1) {
          useLabels = true
          for (gpot <- upots.tail) {
            val UNARY_LABEL_PATTERN1(label, s, e) = gpot.name
            labelIdxs(start)(end) += fg.addVariable("unaryLabelvar(%d,%d,%s)".format(start, end, label), arity=2)
            fg.addUnaryFactor("unaryLabelvar(%d,%d,%s)".format(start, end, label), "unaryLabelfac(%d,%d,%s)".format(start, end, label), gpot)
          }
//          fg.addIsAtMost1Factor(new Regex("unaryvar\\(%d,%d\\)".format(start, end)), new Regex("unaryLabelvar\\(%d,%d,.+\\)".format(start, end)), "isAtMost(%d,%d)".format(start, end))
          fg.addIsAtMost1FactorByIndices(brackIdxs(start)(end), labelIdxs(start)(end).toArray, "isAtMost(%d,%d)".format(start, end))
        }
      }
    }
    return new ConstituentParserModelInstance(fg.toFactorGraph, ex)
  }

  def decode(instance: ModelInstance) = {
    val slen    = instance.ex.attributes.getOrElse("slen", "-1").toInt
    val words   = instance.ex.attributes.getOrElse("words", "").trim.split(" ")
    val tags    = instance.ex.attributes.getOrElse("tags", "").trim.split(" ")
    System.err.println("Decoding...\n" + words.mkString(" "))
    //    println("slen = " + slen)
    //    println("words size = " + words.size)
    //    println("tags size = " + tags.size)
    val beliefs = instance.marginals
    val spans = new ArrayBuffer[Span]

    val labelGroups = beliefs.filter(_.name.contains("spanLabel")).groupBy{p =>
      val INDICES_PATTERN(start, end) = p.name
      (start.toInt, end.toInt)
    }
    if (slen > 2) {
      val ckybeliefs = ckyBrackets(beliefs.filter(_.name.startsWith("brack(")), slen).groupBy{p =>
        val INDICES_PATTERN(start, end) = p.name
        (start.toInt, end.toInt)
      }
      for (width <- 2 to slen; start <- 0 to (slen - width)) {
        val end = start + width
        if (ckybeliefs((start, end))(0).value > 0.5) {
          val label = if (labelGroups.contains((start, end))) {
            maxLabel(labelGroups((start, end)))
          }
          else {
            "EHH"
          }
          spans += new Span(start.toInt, end.toInt, label)
        }
      }
    }

    val unaryGroups = beliefs.filter(_.name.contains("unaryLabel")).groupBy{p =>
      val INDICES_PATTERN(start, end) = p.name
      (start.toInt, end.toInt)
    }
    System.err.println("UNARY BELIEFS = " + beliefs.filter(_.name.contains("unaryLabel")).size)
    beliefs.filter(_.name.contains("unary")).foreach( System.err.println(_))
    beliefs.filter(p => p.name.contains("unary(") && p.value > 0.5).foreach { unary =>
      val UNARY_PATTERN(start, end) = unary.name
      spans += new Span(start.toInt, end.toInt, maxLabel(unaryGroups((start.toInt, end.toInt))), height=1)
    }

    //    System.err.println("SPANS:\n" + spans.mkString("\n"))
    val tree = TreeFactory.constructFromSpans(spans.toArray.filter(s => !s.label.contains("@") && s.width != slen), slen)
    tree.annotateWithIndices(0)

    tree.setYield(words, tags)
    System.out.println(tree.toString() + "\n")

    if (params.OUTPUT_FILE != null) {
      val out = new FileWriter(params.OUTPUT_FILE, true)
      out.write(tree.toString + "\n")
      out.close()
    }
  }

  def ckyBrackets(pots: Array[Potential], slen: Int): Array[Potential] = {
    //    System.err.println("%d pots in cky-bracks.".format(pots.size))
    assert(!pots.exists(!_.name.startsWith("brack")), "There is a potential in ckyBrackets that is not a bracket potential.")
    val beta  = Array.ofDim[Double](slen+1, slen+1)
    val split = Array.ofDim[Int](slen+1, slen+1)
    val brack = Array.ofDim[Boolean](slen+1, slen+1)
    val scores = pots.map(_.value)
    //    val sp = scores

    val groups = pots.filter(_.name.contains("brack")).groupBy{p =>
      val INDICES_PATTERN(start, end) = p.name
      (start.toInt, end.toInt)
    }
    //    System.err.println("GROUPS =\n" + groups.toArray.mkString("\n"))

    for (w <- 2 until slen; i <- 0 to slen-w) {
      val k = i + w
      var inside = Double.NegativeInfinity
      var best = 0
      for (j <- i+1 until k) {
        val s = beta(i)(j) + beta(j)(k)
        if (s > inside) {
          inside = s
          best = j
        }
      }
      val sp1 = groups((i, k))(0) //pots.filter(_.name == "brack(%d,%d)".format(i,k))(0)
      //			println("Brack(%d,%d) = " + sp)
      beta(i)(k) = inside + sp1.value
      split(i)(k) = best
    }
    var inside = Double.NegativeInfinity
    var best = 0
    for (j <- 1 until slen) {
      val s = beta(0)(j) + beta(j)(slen)
      if (s > inside) {
        inside = s
        best = j
      }
    }
    beta(0)(slen) = inside
    split(0)(slen) = best
    ckyBacktrace(0, slen, split, brack)
    //		var op = out // ???
    for (w <- 2 until slen; i <- 0 to slen-w) {
      val k = i + w
      var sp2 = groups((i, k))(0) //pots.filter(_.name == "brack(%d,%d)".format(i,k))(0)
      sp2.value = if (brack(i)(k)) 1.0 else 0.0
      //			op += 1
    }
    groups((0, slen))(0).value = 1
    //    pots.filter(_.name == "brack(%d,%d)".format(0,slen))(0).value = 1
    //		return beta(0)(slen)
    return pots
  }

  def ckyBacktrace(i: Int, k: Int, split: Array[Array[Int]], brack: Array[Array[Boolean]]): Int = {
    val j = split(i)(k)
    if (j > (i+1)) {
      brack(i)(j) = true
      //			println("backtrace-1(%d,%d)".format(i, j))
      ckyBacktrace(i, j, split, brack)
    }
    if (j < (k-1)) {
      brack(j)(k) = true
      //			println("backtrace-2(%d,%d)".format(j, k))
      ckyBacktrace(j, k, split, brack)
    }
    return 1
  }

  def maxLabel(pots: Array[Potential]): String = {
    // 		println("label potentials size = %d".format(pots.size) + "\n" + pots.mkString("\n"))
    var max = pots(0) //Double.NegativeInfinity
    for (pot <- pots) {
      if (pot.value > max.value) {
        max = pot
      }
    }
    //    println("max pot = " + max)
    if (max.name.contains("unary")) {
      val UNARY_LABEL_PATTERN1(label, start, end) = max.name
      return label
    }
    else {
      val LABEL_PATTERN1(label, start, end) = max.name
      return label
    }
  }


  def options = params
}

class ConstituentParserModelInstance(graph: FactorGraph, ex: PotentialExample) extends ModelInstance(graph, ex) with ParserInferenceOrder


trait ConstituentParserFeatures {}




trait ParserInferenceOrder extends InferenceOrder {

  override def messageOrder(graph: FactorGraph): Iterator[MessageNode] = {
    //  System.err.println("Using Parser Inference Order...")
    //    println(graph.toString)

    /*
    val idxpattern = new Regex(".*\\(([0-9]+)[^0-9].*")
    val mqueue = scala.collection.mutable.Queue[MessageNode]()
    //    graph.nodes.filter(n => n.isFactor && n.arity == 1 && n.name.contains("brack")).foreach(mqueue += _)
    //    graph.nodes.filter(n => n.isFactor && n.arity == 1 && n.name.contains("label")).foreach(mqueue += _)
    graph.nodes.filter(n => n.isVariable && n.name.contains("label")).foreach(mqueue += _)
    graph.nodes.filter(n => n.isFactor && n.arity > 1 && !n.name.contains("CKY")).sortBy(_.arity).foreach(mqueue += _)
    graph.nodes.filter(n => n.isVariable && n.name.contains("brack")).foreach(mqueue += _)
    graph.nodes.filter(n => n.isFactor && n.arity > 1 && n.name.contains("CKY"))
    mqueue.iterator ++ mqueue.reverseIterator
    */

    val mqueue = new Queue[MessageNode]()
    val q1 = new Queue[MessageNode]()
    val q2 = new Queue[MessageNode]()
    val q3 = new Queue[MessageNode]()
    val q4 = new Queue[MessageNode]()
    for (n <- graph.nodes) {
      if (n.name.startsWith("CKY")) {
        q4 += n
      }
      if (n.isFactor && n.arity > 1 && !n.name.contains("CKY")) {
        q2 += n
      }
      if (n.isVariable) {
        if (n.name.contains("Label")) {
          q1 += n
        }
        else {
          q3 += n // brack variables
        }
      }
    }

    mqueue ++= q1
    mqueue ++= q2
    mqueue ++= q3
    mqueue ++= q4
    mqueue.iterator ++ mqueue.reverseIterator
  }
}
























    /*
    //    graph.nodes.filter(n => n.isFactor && n.arity == 1 && n.name.contains("brack")).foreach(mqueue += _)
    //    graph.nodes.filter(n => n.isFactor && n.arity == 1 && n.name.contains("label")).foreach(mqueue += _)
    graph.nodes.filter(n => n.isVariable && n.name.contains("label")).foreach(mqueue += _)
    graph.nodes.filter(n => n.isFactor && n.arity > 1 && !n.name.contains("CKY")).sortBy(_.arity).foreach(mqueue += _)
    graph.nodes.filter(n => n.isVariable && n.name.contains("brack")).foreach(mqueue += _)
    graph.nodes.filter(n => n.isFactor && n.arity > 1 && n.name.contains("CKY"))
    mqueue.iterator ++ mqueue.reverseIterator
   */








    // cky
    // isatmost
    //
    /*
    graph.nodes.filter(_.name.contains("labelvar")).foreach(mqueue += _)
    graph.nodes.filter(_.name.contains("labelvar")).foreach(mqueue += _)

    val groups = graph.nodes.groupBy{node =>
      val idxpattern(idx) = node.name
      idx
    }
    val slen = groups.size

    //   val bigram = graph.factors.exists(_.name.contains("blabel"))
    for (i <- 1 to slen) {
      val inodes = groups(i.toString)
      inodes.foreach{n => if (n.isFactor && n.arity == 1) mqueue += n}
    }
    for (i <- 1 to slen) {
      val inodes = groups(i.toString)
      println("TEST: " + i + " = " + inodes.mkString)
      inodes.foreach{n => if (n.isVariable)  mqueue += n}
      inodes.foreach{n => if (n.isFactor && n.arity > 1) mqueue += n}
    }
    */
    //(mqueue ++ mqueue.reverse).iterator






/*
        if (pot.name.contains("brack")) {
          val LABEL_PATTERN1(plabel, pstart, pend) = pot.name
          label = plabel
        }
        else if (pot.name.contains("unary")) {
          val UNARY_LABEL_PATTERN(pstart, pend, plabel) = pot.name
          label = plabel
        }
 */










/*
package narad.nlp.parser.constituent
import narad.bp.structure._
import narad.nlp.trees.{Span, Token, ConstituentTree}
import narad.io.util.{SpanReader, TreeReader}
import scala.collection.mutable.ArrayBuffer
import scala.util.matching._
import narad.bp.util._
import narad.util.ArgParser
import java.io.FileWriter


class Parser(var graph: FactorGraph) extends FactorGraphModel {
	val indicesPattern    = """\(([0-9]+),([0-9]+)\) """.r
	val brackPattern      = """brack\(([0-9]+),([0-9]+)\)""".r
	val labelPattern      = """brackLabel\(([0-9]+),([0-9]+,(.+))\)""".r
	val unaryPattern      = """unary\(([0-9]+),([0-9]+)\)""".r
	val unaryLabelPattern = """unaryLabel\(([0-9]+),([0-9]+),(.+)\)""".r
	
	def indices(s: String): (Int, Int) = s match {
		case indicesPattern(start, end) => Tuple(start.toInt, end.toInt)
		case _ => Tuple(-1, -1)
	}
	
	def maxLabel(pots: Array[Potential]): String = {
//		println("label potentials size = %d".format(pots.size))
		var max = Double.NegativeInfinity
		var label = null.asInstanceOf[String]
		for (pot <- pots) {
			if (pot.value > max) {
				max = pot.value
				if (pot.name.contains("brack")) {
					val labelPattern(pstart, pend, plabel) = pot.name
					label = plabel					
				}
				else if (pot.name.contains("unary")) {
					val unaryLabelPattern(pstart, pend, plabel) = pot.name
					label = plabel										
				}
			}
		}
		return label
	}

/*
	def potentialBeliefs: Array[Potential] = {
		val beliefs = new ArrayBuffer[Potential]
		for (factor <- graph.factors) {
			// println("beliefs  in " + factor.name + " = " + factor.getBeliefs(graph).mkString(";"));
			 beliefs ++= factor.getBeliefs(graph) 
			}
		//println(beliefs.size)
		return beliefs.toArray
	}
	
		
	def variableBeliefs: Array[(String, Double)] = {
		val beliefs = new ArrayBuffer[(String, Double)]
//		for (factor <- graph.factors.filter(_.name != "CKY")) { beliefs ++= factor.getBeliefs(graph) }
		for (variable <- graph.variables) { beliefs ++= variable.getBeliefs(graph) }
		return beliefs.toArray
	}
*/
	
	def decode(words: Array[String], tags: Array[String]): ConstituentTree = {
//		println("Decoding with words: %s".format(words.mkString(" ")))
		val slen = words.size
		val beliefs = graph.potentialBeliefs
		val spans = new ArrayBuffer[Span]
		if (slen > 2) {
			try {
			val ckybeliefs = ckyBrackets(beliefs.filter(_.name.startsWith("brack(")), slen)
			val cspans = ckybeliefs.filter(_.value == 1).map { brack =>
				val brackPattern(start, end) = brack.name
				val labels = beliefs.filter(_.name.matches("brackLabel\\(%s,%s,.+\\)".format(start, end)))
				val label = if (labels.size == 0) {
					System.err.println("No label potentials found corresponding to bracket at (%d,%d), performing unlabeled decoding.")
					""
				}
				else {
					maxLabel(labels)					
				}
				new Span(start.toInt, end.toInt, label, false)
			}
			spans ++= cspans
		}
		catch {
		  case e: Exception => System.err.println("Could not decode tree; Using unary prediction only.")
		}
	//	if (!spans.exists(s => s.start == 0 && s.end == slen)) spans += Span(0, slen, "TOP", false)
		}
//		println("pot_beliefs:")
//		beliefs.foreach(println(_))
//		println("spans:")
//		spans.foreach(println(_))
		
		val uspans = graph.potentialBeliefs.filter(p => p.name.contains("unary(") && p.value > 0.5).map { unary =>
			val unaryPattern(start, end) = unary.name
			val label = maxLabel(beliefs.filter(_.name.matches("unaryLabel\\(%s,%s,.+\\)".format(start, end))))
			new Span(start.toInt, end.toInt, label, true)
		}
//		println("unaries:")
//		uspans.foreach(println(_))
		val tree = SpanReader.spansToTree(spans.toArray.filter(s => !s.label.contains("@") && s.width != slen) ++ uspans, slen)
		tree.annotateWithIndices(0)
		tree.setYield(words, tags)
		return tree
	}
//		ckybeliefs.foreach(println(_))
//		return narad.nlp.parse.TreebankReader.parseExpression("(TOP (NP foo) (NP bar))")
//	}
/*
		val groups = potentialBeliefs.groupBy(p => indices(p.name))
		for (group <- groups) {
			if (group._2(0).value > 0.5) {
				println(group._2(0).name + "\t" + maxLabel(group._2))
			}
		}
	}
*/

  def refineConstituents(slen: Int, brackThresh: Double = 0.0001, constitThresh: Double = 0.0001, top: Int = 25) = {
		val beliefs = graph.variableBeliefs
		val bracks = beliefs.filter(_._1.startsWith("brack"))
		val bchart = Array.ofDim[Double](slen+1, slen+1)
		for (brack <- bracks) {
			brack._2 match {
				case brackPattern(start, end) => bchart(start.toInt)(end.toInt) = brack._2
				case _ => None
			}
		}		

//		val nts = beliefs.map(b => { case labelPattern(l, s, e) => l case _ => None }).filter(_ != None).distinct
//		val nnt = nts.size
		val cchart = Array.ofDim[Double](slen+1, slen+1, slen+1)
		for (i <- 0 until slen+1; j <- 2 until slen+1; k <- 2 until slen+1 if i < j && j < k) {
			cchart(i)(j)(k) = bchart(i)(j) * bchart(j)(k) * bchart(i)(k)
		}	
	}
	
	def refineSplit(i: Int, j: Int, k: Int, cchart: Array[Array[Double]], thresh: Double = 0.0001, top: Int = 25) = {
		val w1 = j - i
		val w2 = k - j
//		if (w1 == 1 && w2 == 1) return Array()
		
		val leftNT  = if (w1 > 1) cchart(i)(j) else 0
		val rightNT = if (w2 > 1) cchart(j)(k) else 0
		val outerNT = cchart(i)(k)
	
		val op = if (w1 == 1) {
//			bestProducts(outerNT, rightNT, thresh=thresh, top=top)
		}
		else if (w2 == 1){
//			bestProducts(outerNT, leftNT, thresh=thresh, top=top)
		}
		else {
//			bestProducts(bestProducts(outerNT, leftNT, thresh=thresh, top=top), rightNT, thresh=thresh, top=top)
		}
		op
	}
	
	def bestProducts(dx: Array[Double], dy: Array[Double], top: Int = 25, thresh: Double = 0.0001) = {
		val xn = dx.size
		val yn = dy.size
//		if (xn == 0 || yn == 0) return Array[Double]
		
	}
	
	/*
	
	double cky_brackets(const double *scores, int slen, int *out) {
	  multi_array<double, 2> beta(extents[slen+1][slen+1]);
	  multi_array<int, 2> split(extents[slen+1][slen+1]);
	  multi_array<bool, 2> brack(extents[slen+1][slen+1]);
	  const double *sp = scores;

	  for ( int i = 0; i < slen; ++i ) beta[i][i+1] = 0;
	  for ( int w = 2; w < slen; ++w ) {
	    for ( int i = 0; i <= (slen - w); ++i ) {
	      int k = i + w;
	      double inside = R_NegInf;
	      int best = 0;
	      for ( int j = i + 1; j < k; ++j ) {
		double s = beta[i][j] + beta[j][k];
	//	cout << "s = " << s << endl;
		if ( s > inside ) {
		  inside = s;
		  best = j;
		}
	      }
		//		cout << "best = " << best << endl;

	      beta[i][k] = inside + *sp++;
	      split[i][k] = best;
	    }
	  }
	//	cout << endl;
	//	cout << "done in triplet loop" << endl;
	  double inside = R_NegInf;
	  int best = 0;
	-----------------------------------
	  for ( int j = 1; j < slen; ++j ) {
	    double s = beta[0][j] + beta[j][slen];
	//		cout << "s2 = " << s << endl;
	    if ( s > inside ) {
	      inside = s;
	      best = j;
	    }
	  }
	//	cout << "done with second loop" << endl;
	  beta[0][slen] = inside;
	  split[0][slen] = best;
	//	cout << "inside = " << inside << endl;
	//	cout << "best = " << best << endl;
	//	cout << "backtrace" << endl;
	  cky_backtrace(0, slen, split, brack);
	//	cout << "done with backtrace" << endl;
	  int *op = out;
	  for ( int w = 2; w < slen; ++w ) {
	    for ( int i = 0; i <= (slen - w); ++i ) {
	      int k = i + w;
	      *op = brack[i][k] ? 1 : 0;
	      ++op;
	    }
	  }
	//	cout << "done with final loop" << endl;
	  return beta[0][slen];
	}
	*/
	
	def ckyBrackets(potentials: Array[Potential], slen: Int): Array[Potential] = {
		System.err.println("%d pots in cky-bracks.".format(potentials.size))
		assert(!potentials.exists(!_.name.startsWith("brack")), "There is a potential in ckyBrackets that is not a bracket potential.")
		val beta  = Array.ofDim[Double](slen+1, slen+1)
		val split = Array.ofDim[Int](slen+1, slen+1)
		val brack = Array.ofDim[Boolean](slen+1, slen+1)
		val scores = potentials.map(_.value)
		val sp = scores

		for (w <- 2 until slen; i <- 0 to slen-w) {
			val k = i + w
			var inside = Double.NegativeInfinity
			var best = 0
			for (j <- i+1 until k) {
				val s = beta(i)(j) + beta(j)(k)
				if (s > inside) {
					inside = s
					best = j
				}
			}
			val sp = potentials.filter(_.name == "brack(%d,%d)".format(i,k))(0)
//			println("Brack(%d,%d) = " + sp)
			beta(i)(k) = inside + sp.value
			split(i)(k) = best
		}
		var inside = Double.NegativeInfinity
		var best = 0
		for (j <- 1 until slen) {
			val s = beta(0)(j) + beta(j)(slen)
			if (s > inside) {
				inside = s
				best = j
			}
		}
		beta(0)(slen) = inside
		split(0)(slen) = best
		ckyBacktrace(0, slen, split, brack)
//		var op = out // ???
		for (w <- 2 until slen; i <- 0 to slen-w) {
			val k = i + w
			val sp = potentials.filter(_.name == "brack(%d,%d)".format(i,k))(0)
			sp.value = if (brack(i)(k)) 1 else 0
//			op += 1 
		}
		potentials.filter(_.name == "brack(%d,%d)".format(0,slen))(0).value = 1
//		return beta(0)(slen)
		return potentials
	}

	def ckyBacktrace(i: Int, k: Int, split: Array[Array[Int]], brack: Array[Array[Boolean]]): Int = {
		val j = split(i)(k)
		if (j > (i+1)) {
			brack(i)(j) = true
//			println("backtrace-1(%d,%d)".format(i, j))
			ckyBacktrace(i, j, split, brack)
		}
		if (j < (k-1)) {
			brack(j)(k) = true
//			println("backtrace-2(%d,%d)".format(j, k))
			ckyBacktrace(j, k, split, brack)
		}
		return 1
	}
		
	override def toString = graph.toString
}

/*
void cky_backtrace(int i, int k,
		   const multi_array<int, 2>& split,
		   multi_array<bool, 2>& brack) {
  int j = split[i][k];
//	cout << endl;
//	cout << "i = " << i << endl;
//	cout << "j = " << j << endl;
//	cout << "k = " << k << endl;
  if ( j > (i+1) ) {
    brack[i][j] = true;
    cky_backtrace(i, j, split, brack);
  }
  if ( j < (k-1) ) {
    brack[j][k] = true;
    cky_backtrace(j, k, split, brack);
  }
}
*/

object Parser {
	val BRACK_PATTERN 			= """brack\(([0-9]+),([0-9]+)\)""".r
	val BRACK_LABEL_PATTERN = """brackLabel\(([0-9]+),([0-9]+),(.+)\)""".r
	val UNARY_PATTERN 			= """unary\(([0-9]+),([0-9]+)\)""".r
	val UNARY_LABEL_PATTERN = """unaryLabel\(([0-9]+),([0-9]+),(.+)\)""".r
	val INDICES_PATTERN     = """.+\(([0-9]+),([0-9]+).+""".r
	val STARTPOS = "START"
	val ENDPOS = "END"
	

	def addBracketPrediction(model: FactorGraphBuilder, pots: Array[Potential], slen: Int) = {
		for (i <- 0 until pots.size) {
			pots(i).name match {
				case BRACK_PATTERN(s, e) => {
					val start = s.toInt
					val end = e.toInt
					model.addVariable("brackvar(%d,%d)".format(start, end), 2)
					if (start == 0 && end == slen) {
						model.addTable1Factor("brackvar(%d,%d)".format(start, end), "brackfac(%d,%d)".format(start,end), Array(new Potential(0, "null", true), new Potential(1, "brack(%d,%d)".format(start, end), true)))  //Array(pots(i))) 
					}
					else {
						model.addUnaryFactor("brackvar(%d,%d)".format(start, end), "brackfac(%d,%d)".format(start,end), Array(pots(i)))				
					}
				}
				case _=> //System.err.println("Brack pattern not matched on %s".format(pots(i).name))			
			}
		}
//		println("SLEN = " + slen)
		model.addCKYFactor(new Regex("brackvar"), slen=slen)
	}


	def addLabelPrediction(model: FactorGraphBuilder, pots: Array[Potential], slen: Int) = {
		for (i <- 0 until pots.size) {
			pots(i).name match {
				case BRACK_LABEL_PATTERN(s, e, label) => {
					val start = s.toInt
					val end = e.toInt
					model.addVariable("labelvar(%d,%d,%s)".format(start, end, label), 2)
					model.addUnaryFactor("labelvar(%d,%d,%s)".format(start, end, label), "labelfac(%d,%d,%s)".format(start, end, label), Array(pots(i)))											
				}
				case _=> //System.err.println("Label pattern not matched on %s".format(pots(i).name))
			}
		}
		for (width <- 2 to slen; start <- 0 to (slen - width)) {
			val end = start + width
			if (start == 0 && end == slen) {
//				model.addIsAtMost1Factor(new Regex("labelvar\\(%d,%d,.+\\)".format(start, end)), "isAtMost(%d,%d)".format(start, end))								
				model.addAtMost1Factor(new Regex("labelvar\\(%d,%d,.+\\)".format(start, end)), "isAtMost(%d,%d)".format(start, end))								
			}
			else {
				model.addIsAtMost1Factor(new Regex("brackvar\\(%d,%d\\)".format(start, end)), new Regex("labelvar\\(%d,%d,.+\\)".format(start, end)), "isAtMost(%d,%d)".format(start, end))				
			}
		}
	}

	def addBracketAndLabelPrediction(model: FactorGraphBuilder, pots: Array[Potential], slen: Int) = {
//		System.err.println("In brackAndLabel adder")
		val groups = pots.filter(_.name.contains("brack")).groupBy{ p =>
			val INDICES_PATTERN(startstr, endstr) = p.name
			Tuple(startstr.toInt, endstr.toInt)
//			startstr + "_" + endstr
		}
//		println(groups.size)
		val ckyidxs = new ArrayBuffer[Int]
		for (g <- groups) {
			val start = g._1._1
			val end   = g._1._2
			val gpots = g._2
			val iidx = model.addVariable("brackvar(%d,%d)".format(start, end), 2)
			ckyidxs += iidx
			if (start == 0 && end == slen) {
				model.addTable1Factor("brackvar(%d,%d)".format(start, end), "brackfac(%d,%d)".format(start,end), Array(new Potential(0, "brackx2(%d,%d)".format(start, end), true), new Potential(1, "brack(%d,%d)".format(start, end), true)))
			}
			else {
				model.addUnaryFactor("brackvar(%d,%d)".format(start, end), "brackfac(%d,%d)".format(start,end), Array(gpots(0)))				
			}

			val fidx = model.addFactor(new IsAtMost1Factor(model.ncount, "parseIsAtMost(%d,%d)".format(start, end)))
			model.addEdge(iidx, fidx)
			for (i <- 1 until gpots.size) {
				val BRACK_LABEL_PATTERN(ss, es, label) = gpots(i).name
				val tidx = model.addVariable("labelvar(%d,%d,%s)".format(start, end, label), 2)
				val lidx = model.addFactor(FactorFactory.createUnaryFactor(model.ncount, "labelfac(%d,%d,%s)".format(start, end, label), gpots(i)))
//				model.addUnaryFactor("labelvar(%d,%d,%s)".format(start, end, label), "labelfac(%d,%d,%s)".format(start, end, label), Array(gpots(i)))												
				model.addEdge(tidx, lidx)
				model.addEdge(tidx, fidx)
			}			
//			println(g)
//			println
		}
		val fidx = model.addFactor(new CKYFactor(model.ncount, "CKY-Factor", slen))
		ckyidxs.foreach(model.addEdge(_, fidx))
//		model.addCKYFactor(new Regex("brackvar"), slen=slen)
	}
	
	def addAllPrediction(model: FactorGraphBuilder, pots: Array[Potential], slen: Int, 
											 useLabels: Boolean = true, useUnaries: Boolean = true) = {
		val groups = pots.groupBy{ p =>
			val INDICES_PATTERN(startstr, endstr) = p.name
			Tuple(startstr.toInt, endstr.toInt)
		}
		
		val ckyidxs = new ArrayBuffer[Int]
		for ( width <- 1 to slen; start <- 0 to (slen - width)) {
			val end = start + width
			val gpots = groups(Tuple(start, end))
//			println(width + ":")
//			println("----")
//			gpots.foreach(p => println(p.name))
			if (width > 1 && slen > 2) {
				val iidx = model.addVariable("brackvar(%d,%d)".format(start, end), 2)
				ckyidxs += iidx
				if (start == 0 && end == slen) {
					model.addTable1Factor("brackvar(%d,%d)".format(start, end), "brackfac(%d,%d)".format(start,end), Array(new Potential(0, "brackx2(%d,%d)".format(start, end), true), new Potential(1, "brack(%d,%d)".format(start, end), true)))
				}
				else {
					model.addUnaryFactor("brackvar(%d,%d)".format(start, end), "brackfac(%d,%d)".format(start,end), Array(gpots(0)))				
				}
				if (useLabels) {
					val fidx = model.addFactor(new IsAtMost1Factor(model.ncount, "parseIsAtMost(%d,%d)".format(start, end)))
					model.addEdge(iidx, fidx)
					for (i <- 1 until gpots.size) {
						val BRACK_LABEL_PATTERN(ss, es, label) = gpots(i).name
						val tidx = model.addVariable("labelvar(%d,%d,%s)".format(start, end, label), 2)
						val lidx = model.addFactor(FactorFactory.createUnaryFactor(model.ncount, "labelfac(%d,%d,%s)".format(start, end, label), gpots(i)))
						model.addEdge(tidx, lidx)
						model.addEdge(tidx, fidx)
					}								
				}				
			}
						
			if (width == 1 && useUnaries) {
				val iidx = model.addVariable("unaryvar(%d,%d)".format(start, end), 2)
				model.addUnaryFactor("unaryvar(%d,%d)".format(start, end), "unaryfac(%d,%d)".format(start,end), Array(gpots(0)))				
				if (useLabels) {
					val fidx = model.addFactor(new IsAtMost1Factor(model.ncount, "unaryIsAtMost(%d,%d)".format(start, end)))
					model.addEdge(iidx, fidx)
					for (i <- 1 until gpots.size) {
						val UNARY_LABEL_PATTERN(ss, es, label) = gpots(i).name
						val tidx = model.addVariable("unaryLabelvar(%d,%d,%s)".format(start, end, label), 2)
						val lidx = model.addFactor(FactorFactory.createUnaryFactor(model.ncount, "unaryLabelfac(%d,%d,%s)".format(start, end, label), gpots(i)))
						model.addEdge(tidx, lidx)
						model.addEdge(tidx, fidx)
					}								
				}				
			}
		}
		val fidx = model.addFactor(new CKYFactor(model.ncount, "CKY-Factor", slen))
		ckyidxs.foreach(model.addEdge(_, fidx))
	}

			
	def addUnaryPrediction(model: FactorGraphBuilder, pots: Array[Potential], slen: Int) = {
		for (i <- 0 until pots.size) {
			pots(i).name match {
				case UNARY_PATTERN(s, e) => {
					val start = s.toInt
					model.addVariable("unaryvar(%d,%d)".format(start, start+1), 2)
					model.addUnaryFactor("unaryvar(%d,%d)".format(start, start+1), "unaryfac(%d,%d)".format(start, start+1), Array(pots(i)))											
				}
				case UNARY_LABEL_PATTERN(s, e, label) => {
					val start = s.toInt
					model.addVariable("unaryLabelvar(%d,%d,%s)".format(start, start+1, label), 2)
					model.addUnaryFactor("unaryLabelvar(%d,%d,%s)".format(start, start+1, label), "unaryLabelfac(%d,%d,%s)".format(start, start+1, label), Array(pots(i)))											
					//					fg.addTable1Factor("unaryvar(%d,%d)".format(start, start+1), "unaryfac(%d,%d)".format(start, start+1), Array[String]("unary(%d,%d)".format(start, start+1)), Array[Double](0,1))				
					// WAS TABLE1FACTOR IN BPDP
				}
				case _=> // System.err.println("Unary pattern not matched on %s".format(pots(i).name))
			}
		}
		for (start <- 0 until slen) {
			val end = start+1
//				model.addIsAtMost1Factor(new Regex("unaryvar\\(%d,%d\\)".format(start, end)), new Regex("unaryLabelvar\\(%d,%d,.+\\)".format(start, end)), "isAtMost(%d,%d)".format(start, end))
				model.addIsAtMost1Factor(new Regex("unaryvar\\(%d,%d\\)".format(start, end)), new Regex("unaryLabelvar\\(%d,%d,.+\\)".format(start, end)), "isAtMost(%d,%d)".format(start, end))
		}
	}

	def constructPiecewise(pots: Array[Potential], slen: Int): Parser = {
//		System.err.println("Constructing traditional")
		val fg = new FactorGraphBuilder(pots)
		if (slen > 2) {
			addBracketPrediction(fg, pots, slen)
			addLabelPrediction(fg, pots, slen)			
		}
		addUnaryPrediction(fg, pots, slen)
		new Parser(fg.toFactorGraph)
	}
	
		def constructBracketModel(pots: Array[Potential], slen: Int): Parser = {
			val fg = new FactorGraphBuilder(pots)
			if (slen > 2) {
				addBracketPrediction(fg, pots, slen)
//				addLabelPrediction(fg, pots, slen)			
			}
//			addUnaryPrediction(fg, pots, slen)
			new Parser(fg.toFactorGraph)
		}
		
	def constructFast(pots: Array[Potential], slen: Int): Parser = {
//		System.err.println("Constructing fast")
		val fg = new FactorGraphBuilder(pots)
		addAllPrediction(fg, pots, slen, true, true)
		new Parser(fg.toFactorGraph)
	}
	
	def construct(ex: PotentialExample, pots: Array[Potential]): Parser = {
		val words     = ex.attributes("@words").split(" ")
		val slen      = words.size
		System.err.print("Constructing Parser (%d words, %d pots): ".format(slen, pots.size))
		var startTime = System.currentTimeMillis()
//		val model = Parser.constructFast(pots, slen)
		val model = Parser.constructBracketModel(pots, slen)
		System.err.println((System.currentTimeMillis() - startTime) / 1000.0 + "s.")		
		model
	}
	
	
	def featurizeSyntax(ctree: ConstituentTree, stats: ParserStatistics, prune: Boolean, out: FileWriter,
		                  bpdp: Boolean = false, options: ArgParser) = {
		var btree = ctree.binarize.removeUnaryChains
		btree.annotateWithIndices()
		val useLabels = options.getBoolean("--use.syntax.labels", true)
		val useUnaries = options.getBoolean("--use.syntax.unaries", true)
		val toks = btree.tokens
		if (options.getBoolean("--print.header", true)) {
			out.write("@slen\t%d\n".format(toks.size))
			out.write("@words\t%s\n".format(toks.map(_.word).mkString(" ")))
			out.write("@tags\t%s\n".format(toks.map(_.pos).mkString(" ")))
		}
		val labels = stats.constituentLabels.toArray
		val spanName   = options.getString("--span.name", "brack")
		val labelName  = options.getString("--label.name", "brackLabel")
		val uspanName  = options.getString("--unary.span.name", "unary")
		val ulabelName = options.getString("--unary.label.name", "unaryLabel")
		val testMode = false
			val window = 5
			val length = btree.tokens.length
			val btokens = pad(btree.tokens, window, window)
			for ( width <- 2 to length; start <- 0 to (length - width)) { //if (start > 0 || width <= length)) {
				val end = start + width
				val si = start + window
				val ei = end + window-1
				val spanFeatures = ConstituentFeatureFactory.syntaxSpanFeatures(btokens, si, ei)
				val labelFeatures = spanFeatures //++ ConstituentFeatureFactory.syntaxLabelFeatures(tokens, si, ei)
				val labelSet = btree.labels(start, end)
				out.write("%s(%d,%d)\t%s%s\n".format(spanName, start, end, if (labelSet.size > 0) "+" else "", spanFeatures.mkString(" ")))
				if (useLabels) {
					for (label <- labels) {
						val builder = new StringBuilder()
						for (f <- labelFeatures) builder.append(" " + label + "_" + f)
						if (bpdp) {
							out.write("%s%s(%d,%d)\t%s%s\n".format(labelName, label, start, end, if (labelSet contains label) "+" else "", builder.toString.trim))												
						}
						else {
							out.write("%s(%d,%d,%s)\t%s%s\n".format(labelName, start, end, label, if (labelSet contains label) "+" else "", builder.toString.trim))												
						}
					}													
				}
			}

			if (useUnaries) {
				val uterms = stats.unaryLabels.toArray //clabels.filter(!_.contains("@"))
				val tokens = ctree.tokens
				for (idx <- 0 until length) {
					val features = ConstituentFeatureFactory.unaryFeatures(ctree, idx) //tree.removeUnaryChains, idx)
					val hasUnary = ctree.containsUnarySpan(idx, idx+1)
					out.write("%s(%d,%d)\t%s%s\n".format(uspanName, idx, idx+1, if (hasUnary) "+" else "", features.map("U-%s".format(_)).mkString(" ")))
					var ccount = 0
					for (uterm <- uterms) {
						val correctLabel = ctree.containsUnarySpan(idx, idx+1, uterm)
						if (correctLabel) {
							ccount += 1
						}
						val builder = new StringBuilder()
						for (f <- features) {
							builder.append(" [unary-" + uterm + "]-" + f)
						}
						if (bpdp) {
							out.write("%s%s(%d,%d)\t%s%s\n".format(ulabelName, uterm, idx, idx+1, if (correctLabel) "+" else "", builder.toString.trim))							
						}
						else {
							out.write("%s(%d,%d,%s)\t%s%s\n".format(ulabelName, idx, idx+1, uterm, if (correctLabel) "+" else "", builder.toString.trim))							
						}
					}
				}				
			}
			out.write("\n")
		}

		def pad(array: Array[Token], spad: Int, epad: Int): Array[Token] = {
			val buffer = new ArrayBuffer[Token]
			val end = spad + epad + array.size
			for (i <- 0 until end){
				if (i < spad){
					buffer += Token("[START%d]".format(spad-i), STARTPOS)
				}
				else if (i >= array.size + spad){
					buffer += Token("[END%d]".format(1 + i - (array.size + spad)), ENDPOS)
				}
				else{
					buffer += array(i-spad)	
				}	
			}
			return buffer.toArray
		}	
}
		
		
/*	
	def construct(pots: Array[Potential], slen: Int, 
								 labels: Array[String] = Array[String](), grammar: Array[String] = Array[String]()): Parser = {
//		if (true) return construct3(pots, slen)
		val fg = new FactorGraphBuilder(pots)
		for (i <- 0 until pots.size) {
			pots(i).name match {
				case brackPattern(s, e) => {
					val start = s.toInt
					val end = e.toInt
					fg.addVariable("brackvar(%d,%d)".format(start, end), 2)
					if (start == 0 && end == slen) {
						fg.addTable1Factor("brackvar(%d,%d)".format(start, end), "brackfac(%d,%d)".format(start,end), Array(pots(i))) 
						//Array[Potential](new Potential(0.0, "+brack(0,%d)".format(end), true)))				
					}
					else {
						fg.addUnaryFactor("brackvar(%d,%d)".format(start, end), "brackfac(%d,%d)".format(start,end), Array(pots(i)))				
					}
				}
				case brackLabelPattern(label, s, e) => {
					val start = s.toInt
					val end = e.toInt
					fg.addVariable("label%svar(%d,%d)".format(label, start, end), 2)
					fg.addUnaryFactor("label%svar(%d,%d)".format(label, start, end), "label%sfac(%d,%d)".format(label, start, end), Array(pots(i)))											
				}
				case unaryPattern(s, e) => {
					val start = s.toInt
					fg.addVariable("unaryvar(%d,%d)".format(start, start+1), 2)
					fg.addUnaryFactor("unaryvar(%d,%d)".format(start, start+1), "unaryfac(%d,%d)".format(start, start+1), Array(pots(i)))											
				}
				case unaryLabelPattern(label, s, e) => {
					val start = s.toInt
					fg.addVariable("unaryLabel%svar(%d,%d)".format(label, start, start+1), 2)
					fg.addUnaryFactor("unaryLabel%svar(%d,%d)".format(label, start, start+1), "unaryLabel%sfac(%d,%d)".format(label, start, start+1), Array(pots(i)))											
//					fg.addTable1Factor("unaryvar(%d,%d)".format(start, start+1), "unaryfac(%d,%d)".format(start, start+1), Array[String]("unary(%d,%d)".format(start, start+1)), Array[Double](0,1))				
// WAS TABLE1FACTOR IN BPDP
				}
				case _=> System.err.println("Pattern not matched on %s".format(pots(i).name))
			}
		}
		fg.addCKYFactor(new Regex("brackvar"), slen=slen)		
		for (width <- 2 to slen; start <- 0 to (slen - width)) {
			val end = start + width
			if (start == 0 && end == slen) {
				fg.addAtMost1Factor(new Regex("label.+var\\(%d,%d\\)".format(start, end)), "isAtMost(%d,%d)".format(start, end))								
			}
			else {
				fg.addIsAtMost1Factor(new Regex("brackvar\\(%d,%d\\)".format(start, end)), new Regex("label.+var\\(%d,%d\\)".format(start, end)), "isAtMost(%d,%d)".format(start, end))				
			}
		}
		for (start <- 0 until slen) {
			val end = start+1
				fg.addIsAtMost1Factor(new Regex("unaryvar\\(%d,%d\\)".format(start, end)), new Regex("unaryLabel.+var\\(%d,%d\\)".format(start, end)), "isAtMost(%d,%d)".format(start, end))
		}
		new Parser(fg.toFactorGraph)
	}

	//			fg.addIsAtMost1Factor("unaryvar\\(%d,%d\\)".format(start, end), "unaryLabel.+var\\(%d,%d\\)".format(start, end), "isAtMost(%d,%d)".format(start, end))
	//			fg.addIsAtMost1Factor("brackvar\\(%d,%d\\)".format(start, end), "label.+var\\(%d,%d\\)".format(start, end), "isAtMost(%d,%d)".format(start, end))

	def construct2(pots: Array[Potential], slen: Int, 
		labels: Array[String] = Array[String](), grammar: Array[String] = Array[String]()): Parser = {
			val fg = new FactorGraphBuilder(pots)
			val bvars = Array.ofDim[Int](slen+1, slen+1)
			val cky   = new ArrayBuffer[Int]
			for (i <- 0 until pots.size) {
				pots(i).name match {
					case brackPattern(s, e) => {
						val start = s.toInt
						val end = e.toInt
						val vidx = fg.addVariable("brackvar(%d,%d)".format(start, end), 2)
						val fidx = fg.addUnaryFactor("brackvar(%d,%d)".format(start, end), "brackfac(%d,%d)".format(start,end), Array(pots(i)))				
						bvars(start)(end) = vidx
					}
					case brackLabelPattern(label, s, e) => {
						val start = s.toInt
						val end = e.toInt
						fg.addVariable("label%svar(%d,%d)".format(label, start, end), 2)
						fg.addUnaryFactor("label%svar(%d,%d)".format(label, start, end), "label%sfac(%d,%d)".format(label, start, end), Array(pots(i)))											
					}
					case unaryPattern(s, e) => {
						val start = s.toInt
						fg.addVariable("unaryvar(%d,%d)".format(start, start+1), 2)
						fg.addUnaryFactor("unaryvar(%d,%d)".format(start, start+1), "unaryfac(%d,%d)".format(start, start+1), Array(pots(i)))											
					}
					case unaryLabelPattern(label, s, e) => {
						val start = s.toInt
						fg.addVariable("unaryLabel%svar(%d,%d)".format(label, start, start+1), 2)
						fg.addUnaryFactor("unaryLabel%svar(%d,%d)".format(label, start, start+1), "unaryLabel%sfac(%d,%d)".format(label, start, start+1), Array(pots(i)))											
					}
					case _=> System.err.println("Pattern not matched on %s".format(pots(i).name))
			}
		}
		fg.addCKYFactor(new Regex("brackvar"), slen=slen)		
		for (width <- 2 to slen; start <- 0 to (slen - width)) {
			val end = start + width
			fg.addIsAtMost1Factor(new Regex("brackvar\\(%d,%d\\)".format(start, end)), new Regex("label.+var\\(%d,%d\\)".format(start, end)), "isAtMost(%d,%d)".format(start, end))
		}
		for (start <- 0 until slen) {
			val end = start+1
			fg.addIsAtMost1Factor(new Regex("unaryvar\\(%d,%d\\)".format(start, end)), new Regex("unaryLabel.+var\\(%d,%d\\)".format(start, end)), "isAtMost(%d,%d)".format(start, end))
		}
		new Parser(fg.toFactorGraph)		
	}
	
	
	def construct3(pots: Array[Potential], slen: Int): Parser = {
		val indexPattern    = """.+\(([0-9]+),([0-9]+).+""".r
		val labelPattern    = """brackLabel(.+)\(([0-9]+),([0-9]+)\)""".r
		
		val fg = new FactorGraphBuilder(pots)
		val sets = pots.filter(!_.name.contains("unary")).groupBy { pot =>
			val indexPattern(start, end) = pot.name
			start + end * 1000
		}
		for (set <- sets) {
			val spots = set._2
			val indexPattern(s, e) = spots(0).name
			val start = s.toInt
			val end = e.toInt
			val vidx = fg.addVariable("brackvar(%d,%d)".format(start, end), 2)
			val fidx = fg.addUnaryFactorByIndex(vidx, "brackfac(%d,%d)".format(start,end), Array(spots(0)))
			val lvidxes = new ArrayBuffer[Int]
			for (lidx <- 1 until spots.size) {
				val labelPattern(label, ss, ee) = spots(lidx).name
				val lvidx = fg.addVariable("label%svar(%d,%d)".format(label, start, end), 2)
				fg.addUnaryFactorByIndex(lvidx, "label%sfac(%d,%d)".format(label, start, end), Array(spots(lidx)))															
				lvidxes += lvidx
			}
			fg.addIsAtMost1FactorByIndex(vidx, lvidxes.toArray, "isAtMost(%d,%d)".format(start, end))
		}
		fg.addCKYFactor(new Regex("brackvar"), slen=slen)		
		new Parser(fg.toFactorGraph)		
	}
}

*/

/*
val ckyLinks = new ArrayBuffer[Int]
for (i <- 0 to slen; j <- 0 to slen) {
	val vidx = fg.addVariable("brackvar(%d,%d)".format(i, j), 2)
	ckyLinks += vidx
}

*/

/*
for ( i <- 0 through slen )
 for ( j <- 1 through slen )
   create bracket variables and factors
   push brack variable onto list of neighbors of the tree factor
    // Figure out some may to make to following lookup fast.
    // With hashing of potentials, you might be able to take a
generate and test approach, but maybe you need to have a list for each
span
   for ( lab in list of labels valid for (i,j) )
       create label variable and factor
       push label variable on list of neighbors of atmost1
   create atmost1 factor from list constructed in inner loop
create tree factor from list constructed in inner loop

*/











































/*
	def construct(pots: Array[Potential], slen: Int, labels: Array[String] = Array[String]()): Parser = {
		val prune = true
		var btime = System.currentTimeMillis()
		val fg = new FactorGraphBuilder(pots)
		println("SLEN = " + slen)
		println("LABELS = " + labels.mkString(", "))

		//		println("------------------------")
		var count = 0
		for (width <- 2 to slen; start <- 0 to (slen - width)) {
			val end = start + width
			fg.addVariable("brackvar(%d,%d)".format(start, end), 2)
			if (start == 0 && end == slen) {  // Top span prediction done with a table1 factor instead
				fg.addTable1Factor("brackvar(%d,%d)".format(start, end), "brackfac(%d,%d)".format(start,end), Array[String]("brack(%d,%d)".format(start,end)), Array[Double](0,1))				
			}
			else {
				fg.addUnaryFactor("brack\\(%d,%d\\)".format(start, end), "brackvar(%d,%d)".format(start, end), "brackfac(%d,%d)".format(start,end))				
			}
		}
		fg.addCKYFactor("brackvar(", "CKY", slen)		
		
		
		// Add Label model
		for (width <- 2 to slen; start <- 0 to (slen - width)) {
			val end = start + width
			if (labels.size > 1) {
				for (label <- labels) {
					fg.addVariable("label%svar(%d,%d)".format(label, start, end), 2)
					if (!prune || pots.view.map(_.name).contains("spanLabel%s\\(%d,%d\\)".format(label, start, end))) {
						fg.addUnaryFactor("spanLabel%s\\(%d,%d\\)".format(label, start, end), "label%svar(%d,%d)".format(label, start, end), "label%sfac(%d,%d)".format(label, start, end))										
					}
				}
				fg.addIsAtMost1Factor("brackvar\\(%d,%d\\)".format(start, end), "label.+var\\(%d,%d\\)".format(start, end), "isAtMost(%d,%d)".format(start, end))
			}
		}
		println("Added nodes in " +  (System.currentTimeMillis() - btime))
//		println("Done adding nodes. Added %d".format(count))
		new Parser(fg.toFactorGraph)
	}
*/	

//}


/*	
SEXP do_best_products(SEXP dx, SEXP dy, SEXP itop, SEXP dthresh) {
  SEXP ans, names;
  double *x = REAL(dx);
  double *y = REAL(dy);
  int top = asInteger(itop);
  double thresh = asReal(dthresh);

  int xn = length(dx);
  int yn = length(dy);

  if ( xn == 0 || yn == 0 ) {
    PROTECT(ans = allocVector(REALSXP, 0));
    UNPROTECT(1);
    return ans;
  }

  SEXP xnames = getAttrib(dx, R_NamesSymbol);
  SEXP ynames = getAttrib(dy, R_NamesSymbol);

  vector<deriv> xs;
  vector<deriv> ys;

  for ( int i = 0; i < xn; ++i ) xs.push_back(deriv(x[i], i, 0));
  for ( int i = 0; i < yn; ++i ) ys.push_back(deriv(y[i], i, 0));

  if ( top < xn ) partial_sort(xs.begin(), xs.begin() + top, xs.end(), std::greater<deriv>());
  else sort(xs.begin(), xs.end(), std::greater<deriv>());

  if ( top < yn ) partial_sort(ys.begin(), ys.begin() + top, ys.end(), std::greater<deriv>());
  else sort(ys.begin(), ys.end(), std::greater<deriv>());

  std::priority_queue<deriv> q;
  vector<deriv> res;
  std::tr1::unordered_set< pair<int, int> > enqueued;

  q.push(deriv(xs[0].weight_ * ys[0].weight_, 0, 0));
  enqueued.insert(std::make_pair(0, 0));

  while ( res.size() < top && q.size() > 0 ) {
    const deriv& d = q.top();
    if ( d.weight_ < thresh ) break;
    res.push_back(d);
    int L = d.left_;
    int R = d.right_;

    q.pop();			// pop only after copying d
    if ( L + 1 < xn && !enqueued.count(std::make_pair(L + 1, R)) ) {
      q.push(deriv(xs[L + 1].weight_ * ys[R].weight_, L + 1, R));
      enqueued.insert(std::make_pair(L+1, R));
    }
    if ( R + 1 < yn && !enqueued.count(std::make_pair(L, R + 1)) ) {
      q.push(deriv(xs[L].weight_ * ys[R + 1].weight_, L, R + 1));
      enqueued.insert(std::make_pair(L, R+1));
    }
  }
*/

/*
refine.constits <- function(p, pots, bracket.threshold=0.0001, constituent.threshold=0.0001, top=50) {
  slen <- attr(pots, "slen")
  b <- variable.beliefs(p)
  bracks <- sapply(b[grep("^B", names(b))], `[[`, 2)
  bchart <- matrix(1, slen+1, slen+1)
  idx <- subset(expand.grid(start=1:(slen-1), width=2:(slen-1)), start+width <= slen+1)
  bchart[cbind(idx$start, idx$start + idx$width)] <- bracks

  splits <- subset(expand.grid(i=as.integer(1:slen+1), j=as.integer(2:slen+1), k=as.integer(2:slen+1)), i < j & j < k)
  splits.scores <-
    bchart[cbind(splits$i, splits$j)] * bchart[cbind(splits$j, splits$k)] *
      bchart[cbind(splits$i, splits$k)]

  nts <- sub("^Constit(.*)\\(0,2\\)$", "\\1", grep("^Constit.*\\(0,2\\)$", names(b), value=TRUE))
  nnt <- length(nts)
  consts <- sapply(b[grepl("^Constit", names(b)) & !grepl(sprintf("\\(0,%d\\)", slen), names(b))], `[[`, 2)

  cchart <- array(1, c(nnt, slen+1, slen+1), dimnames=list(nts, NULL, NULL))
  cidx <- subset(expand.grid(nt=1:nnt, start=1:(slen-1), width=2:(slen-1)), start+width <= slen+1)
  cchart[cbind(cidx$nt, cidx$start, cidx$start + cidx$width)] <- consts

  good.splits <- which(splits.scores >= bracket.threshold)

  do.call(rbind,
          mapply(refine.split,
                 splits$i[good.splits],
                 splits$j[good.splits],
                 splits$k[good.splits],
                 MoreArgs=list(cchart=cchart, thresh=constituent.threshold, top=top)))
}



refine.split <- function(i, j, k, cchart, thresh=0.0001, top=100) {
  w1 <- j - i
  w2 <- k - j
  if ( w1 == 1 && w2 == 1 ) return(NULL)

  if ( w1 > 1 ) left.nt <- cchart[, i, j]
  if ( w2 > 1 ) right.nt <- cchart[, j, k]
  outer.nt <- cchart[, i, k]

  if ( w1 == 1 ) {
    op <- best.products(outer.nt, right.nt, thresh=thresh, top=top)
  } else if ( w2 == 1 ) {
    op <- best.products(outer.nt, left.nt, thresh=thresh, top=top)
  } else {
    op <- best.products(best.products(outer.nt, left.nt, thresh=thresh, top=top),
                        right.nt, thresh=thresh, top=top)
  }
  if ( length(op) == 0 ) return(NULL)
  data.frame(mu=as.vector(op), i=i-1, j=j-1, k=k-1, labels=names(op), stringsAsFactors=FALSE)
}











val nts = beliefs.map { b =>
	b._1 match {
		case labelPattern(label, start, end) => label
		case _ => None
	}
}.filter(_ != None).distinct

*/


/*
		val beliefs = potentialBeliefs
		for(belief <- beliefs.filter(_.name.contains("brack"))) {
			if (belief._2 > 0.5) {
				println(belief._1)
				val labels = p
			}
		}
  }
*/
/*
		beliefs.foreach(println(_))
		println
//		println("graph:")
//		println(graph.toString)
//		println("DECODING:")

	println("\n" + words.mkString(" "))
		val slen = words.size
		for (width <- 2 to slen; start <- 0 to slen - width) {
			val end = start + width
//			for (belief <- beliefs)
			val bracks = beliefs.filter(_._1.startsWith("brack(%d,%d)".format(start, end)))
//			println(bracks.size)
//			assert(bracks.size == 2, "Expected 2 potentials for each unary factor, and did not find one for %d,%d".format(start, end))
			println(bracks(0)._1 + " = " + bracks(0)._2)
//			println(bracks(1)._1 + " = " + bracks(1)._2)
			if (bracks(0)._2 > 0.5) {
				println(bracks(0)._1) + " <---------"
			}
		}
	}
*/

/*
			if (start == 0 && end == slen) {
				fg.addTable1Factor("brackvar(%d,%d)".format(start, end), "topbrackfac(%d,%d)".format(start,end), Array[Double](0,1))				
				if (labels.size > 1) {
					for (label <- labels) {
						fg.addVariable("label%svar(%d,%d)".format(label, start, end), 2)
						fg.addUnaryFactor("spanLabel%s\\(%d,%d\\)".format(label, start, end), "label%svar(%d,%d)".format(label, start, end), "label%sfac(%d,%d)".format(label, start, end))				
					}
					fg.addAtMost1Factor("spanLabel[^\\(]\\(%d,%d\\)".format(start, end), "isAtMost(%d,%d)".format(start, end))
				}
			}
			else {
				fg.addUnaryFactor("brack\\(%d,%d\\)".format(start, end), "brackvar(%d,%d)".format(start, end), "brackfac(%d,%d)".format(start,end))				
				if (labels.size > 1) {
					for (label <- labels) {
						fg.addVariable("label%svar(%d,%d)".format(label, start, end), 2)
						fg.addUnaryFactor("spanLabel%s\\(%d,%d\\)".format(label, start, end), "label%svar(%d,%d)".format(label, start, end), "label%sfac(%d,%d)".format(label, start, end))				
					}
					fg.addAtMost1Factor("label.+var\\(%d,%d\\)".format(start, end), "isAtMost(%d,%d)".format(start, end))
				}
			}
		}
		fg.addCKYFactor("brackvar(", "CKY", slen)		
		println("Done adding nodes. Added %d".format(count))
		new Parser(fg.toFactorGraph)
	}
}
*/



*/








/*
package narad.nlp.parser.constituent
import narad.bp.structure._
import narad.bp.train._
import narad.bp.util._
import narad.nlp.trees.Tree
import narad.io.tree.TreeReader
import narad.util._
import scala.collection.mutable.{ArrayBuffer, HashMap}

object ParserRunner {


  def main(args: Array[String]) = {
    val options  = new ArgParser(args)
    val fidxFile = options.getString("--fidx.file", "null")
    val initFile = options.getString("--init.file", "null")
    val outFile  = options.getString("--out.file", "parser.model")
    val pvsize     = options.getInt("--pv.size", -1)
    val maxEx      = options.getInt("--max.examples", -1)
    val iterations = options.getInt("--iterations", 10)
    val rate       = options.getDouble("--rate", 0.3)
    val initRate   = options.getDouble("--init.rate", 1)
    val prune = options.getBoolean("--prune", false)
    val verbose = options.getBoolean("--verbose", false)
    var btime = System.currentTimeMillis()

    if (options.getBoolean("--train", false)) {
      train(fidxFile, initFile, outFile, pvsize, iterations, maxEx, verbose=verbose)
    }
    else if (options.getBoolean("--test", false)) {
      test(fidxFile, initFile, verbose=verbose) //, prune)
    }
    System.err.println("Elapsed Time: " + (System.currentTimeMillis() - btime) / 1000.0 + "s.")
  }


  def train(fidxFile: String, initFile: String, outFile: String, pvsize: Int, iterations: Int = 10, maxEx: Int = -1, verbose: Boolean = false) = {
    var params = init(initFile, pvsize)
    val nrExamples = if (maxEx == -1) countExamples(fidxFile) else maxEx
    for (i <- 0 until iterations) {
      val nparams = SGDTrainer.train(params, fidxFile, Parser.construct, maxExamples = nrExamples, bpIters = 10, verbose = verbose)
      if (i+1 == iterations) {
        writeToFile(nparams.tail, outFile + ".pv")
      }
      else{
        writeToFile(nparams.tail, outFile + "." + (i+1) + ".pv")
      }
      params = nparams
    }
  }

  def test(fidxFile: String, initFile: String, maxEx: Int = -1, verbose: Boolean = false) = {
    var params = init(initFile)
    val maxExamples = if (maxEx == -1) countExamples(fidxFile) else maxEx
    var count = 0
    for (ex <- PotentialReader.read(fidxFile) if count < maxExamples) {
      val words     = ex.attributes("@words").split(" ")
      val tags      = ex.attributes("@tags").split(" ")
      val slen      = words.size
      val model = SGDTrainer.test(params, Parser.construct, ex, bpIters = 10, verbose = verbose).asInstanceOf[Parser]
      val tree = model.decode(words, tags)
      println(tree)
      count += 1
    }
  }

/*
  def construct(ex: PotentialExample, pots: Array[Potential]): Parser = {
    val words     = ex.attributes("@words").split(" ")
//		val nonterms  = ex.attributes.getOrElse("@grammar", "").split(" ").distinct
    val slen      = words.size
    System.err.print("Constructing Parser (%d words, %d pots): ".format(slen, pots.size))
    var startTime = System.currentTimeMillis()
    val model = Parser.construct(pots, slen) //, nonterms)
    System.err.println((System.currentTimeMillis() - startTime) / 1000.0 + "s.")
    model
  }
*/

  def init(initFile: String, pvsize: Int = 0): Array[Double] = {
    assert(initFile != "null" || pvsize > 0, "No init.file or pv.size specified.")
    if (initFile == "null") {
      return Array.fill(pvsize+1)(0.0)
    }
    else {
      val params1 = io.Source.fromFile(initFile).getLines().map(_.toDouble).toArray
      val params = Array[Double](0.0) ++ params1
      if (pvsize > params.size) {
        return params ++ Array.fill(pvsize - (params.size+1))(0.0)
      }
      else {
        return params
      }
    }
  }

  def writeToFile(array: Array[Double], filename: String) {
    val fw = new java.io.FileWriter(filename)
    array.foreach{ p =>
      fw.write(p + "\n")
    }
    fw.close
  }

  def countExamples(filename: String): Int = {
    print("Determining number of examples in fidx file...")
    val endPattern   = """[ \n\t]*""".r
    var count = 0
    for (line <- scala.io.Source.fromFile(filename).getLines if line == "") count += 1
    println(count + ".")
    return count
  }
}



























//	def computePotentials(params: Array[Double], feats: Array[Array[Feature]]): Array[Double] = {
//		println("Params = %d".format(params.size))
//		feats.map(x => x.foldLeft(0.0)((sum, feat) => sum + params(feat.idx) * feat.value))
//	}

  //			var nparams = sgdtrain(params, fidxFile, maxExamples = nrExamples, iteration = i)

//		val pots = ex.potentials
/*
  var btime = System.currentTimeMillis()

    val names   = pots.map(_.name)
    val correct = pots.map(_.isCorrect)
    val feats   = ex.features // pots.map(_.features)
    println("timed extracted example in " +  (System.currentTimeMillis() - btime))
    btime = System.currentTimeMillis()

    val pscores  = computePotentials(params, feats)
    println("timed potentials in " +  (System.currentTimeMillis() - btime))
     btime = System.currentTimeMillis()

    val scores = pscores.map(Math.exp(_))
    println("timed constructed node arry in " +  (System.currentTimeMillis() - btime))
    btime = System.currentTimeMillis()

    for (i <- 0 until pscores.size) {
      println("O = %f;\tE = %f".format(pscores(i), scores(i)))
    }
*/


  // def train(params: Array[Double],
  // 	fidxFile: String, constructor: (PotentialExample, Array[Potential]) => Model,
  //  maxExamples: Int = Int.MaxValue,
  //	rate: Double = -0.01,
  //  variance: Double = 0.0,
  //  bpIters: Int = 5,
  //  verbose: Boolean = false): Array[Double] = {


    //}, prune: Boolean) = {
/*
    var params = init(initFile)
    for (ex <- PotentialReader.read(fidxFile)) {
      val words  = ex.attributes("@words").split(" ")
      val nonterms = ex.attributes.getOrElse("@grammar", "").split(" ").distinct
      var btime = System.currentTimeMillis()

      val pots = ex.potentials
      val names   = pots.map(_.name)
      val correct = pots.map(_.isCorrect)
      val feats   = pots.map(_.features)
      println("timed extracted example in " +  (System.currentTimeMillis() - btime))
      btime = System.currentTimeMillis()

      val pscores  = computePotentials(params, feats)
      println("timed potentials in " +  (System.currentTimeMillis() - btime))
       btime = System.currentTimeMillis()

      val scores = pscores.map(Math.exp(_))
      println("timed constructed node arry in " +  (System.currentTimeMillis() - btime))
      btime = System.currentTimeMillis()

      for (i <- 0 until pscores.size) {
        println("O = %f;\tE = %f".format(pscores(i), scores(i)))
      }
      val model = Parser.construct2(names, scores, words.size, nonterms)
      println("timed parser creation in " +  (System.currentTimeMillis() - btime))
      btime = System.currentTimeMillis()

      var conv = runBP(model.graph, 5, 1, 0.1)
      println("timed BP in " +  (System.currentTimeMillis() - btime))
      btime = System.currentTimeMillis()
      model.decode(words)
      println
    }
  */

//	def sgdtrain(params: Array[Double], fidxFile: String, maxExamples: Int = Int.MaxValue, rate: Double = 0.1, iteration: Int = 0): Array[Double] = { //pots: Array[Potential], scores: Array[Double], maxExamples: Int = 1): Array[Double] = {
//		var count = 0
//		var pvv = params.clone
//		val damp = 0.099
    /*
    for (ex <- PotentialReader.read(fidxFile) if count < maxExamples) {
//			val rate = -1.0 * (0.01 / (1.0 + ((count+1.0) + (iteration * maxExamples))/(maxExamples*1.0)))
      println("\nPVV:")
        for (i <- 0 until pvv.size) {
          println("[%d]\t%f".format(i, pvv(i)))
        }
      val rate = -0.01
      val pots = ex.potentials
      val names   = pots.map(_.name)
      val correct = pots.map(_.isCorrect)
      val feats   = pots.map(_.features)
      val scores  = computePotentials(pvv, feats).map(Math.exp(_))
      println("\nSCORES:")
        for (i <- 0 until scores.size) {
          println("[%d]\t%s\t%f".format(i, names(i), scores(i)))
        }


      val words  = ex.attributes("@words").split(" ")
      val tagset = ex.attributes.getOrElse("@grammar", "").split(" ").distinct

      val model = Parser.construct2(names, scores, words.size, tagset)
      println("Model")
      println(model.toString)
      var conv = runBP(model.graph, 5, damp, 0.1)

//			for (e <- model.graph.edges) {
//				println()
//			}
      val beliefs = model.potentialBeliefs.distinct
//			var bcount = 0
//			for (ob <- beliefs1) { println(bcount + "\t" + ob); bcount += 1 }
//			println("---------")
//			println(beliefs1.size)
//			println(names.size)

/*
      var beliefs = beliefs1.zipWithIndex.map { case(v, idx) =>
        Tuple(names(idx), v._2)
      }
*/
//			beliefs(beliefs.size-1) = Tuple("brack(0,16)", 1.0)

//			for (i <- 0 until beliefs.size) {
//				println("%s vs %s".format(names(i), beliefs1(i)._1))
//			}

      val margs = beliefs.map { belief =>
        val idx = names.indexOf(belief._1)
        assert(idx != -1, "Potential %s from marg function does not match any name from the potential file.")
        println("  name = " + belief._1 + "\t" + idx + "\t" + belief._2)
        if (correct(idx))
          belief._2 - 1.0
        else
          belief._2
      }
      println("\nBELIEFS")
      for (i <- 0 until beliefs.size) {
        println("%s\t%f\t%f".format(beliefs(i)._1, beliefs(i)._2, margs(i)))
      }
/*
      println("Pots not used?")
      val n2 = beliefs.map(_._1)
      for (pot <- names.filter(!n2.contains(_))) {
        println("NOT FOUND = " + pot)
      }
*/
      pvv = margupdate(pvv, feats, margs, rate=rate)
      println("\n\nPV:")
      for (i <- 0 until pvv.size) {
        println("[%d]\t%f".format(i, pvv(i)))
      }
      count += 1
    }
    */
//		return pvv
//	}


/*

  def margupdate(oldpv: Array[Double], feats: Array[Array[Feature]], margs: Array[Double], rate: Double = 1.0): Array[Double] = {
    println("rate = " + rate)
    var newpv = oldpv
    assert(feats.size == margs.size, "feats and marg arguments to margupdate were not identically sized (%d to %d).".format(feats.size, margs.size))
    for (i <- 0 until margs.size) {
      val grad = margs(i) * rate
      for (j <- 0 until feats(i).size) {
        val feat = feats(i)(j)
        newpv(feat.idx) += grad * feat.value
      }
    }
    println("\nMarg Update:")
    for (i <- 0 until 20) {
      //			println(newpv(i) + " : " + margs(i) + "\t" + feats(i).mkString(","))
    }

    return newpv
  }


  def runBP(graph: FactorGraph, bpiters: Int = 5, drate: Double = 0.99, dinit: Double, threshold: Double = .001): Boolean = {
    val model = graph  // Should pass in a model and not need to do this, but tagger does not subclass FG yet
    var maxDiff = -1.0
    var damp = dinit
    val mqueue = scala.collection.mutable.Queue[MessageNode]() // queue for everything not in uqueue
    val uqueue = scala.collection.mutable.Queue[MessageNode]() // queue for unary factors, will probably rearrange and make this obsolete
    // Load the queue, could add support for user pre-loading of the queue
    for (fac <- model.factors) {
      if (model.edgesFrom(fac).size == 1) {
        uqueue += fac
      }
      else {
        mqueue += fac
      }
    }
    for (v <- model.variables) {
      mqueue += v
    }

//		mqueue ++= mqueue.reverse
    println("M QUEUE: ")
    for (m <- mqueue) {
      println("\t" + m.toString)
    }

    // Do the unary facs first with no damping
    for (u <- uqueue) {
      u.computeMessages(model, damp=1)
    }
    // Do the others with successive damping
    for (i <- 0 until bpiters) {
      println("\n\nBP ITER %d".format(i))
      for (v <- mqueue) {
        maxDiff = -1.0
        var diff = v.computeMessages(model, 1) //damp=damp)
        if (diff > maxDiff) maxDiff = diff
      }
      println(maxDiff + " vs threshold of " + threshold)
      if (i > 0 && maxDiff < threshold) {  // Converged??
        return true
      }
      if (i > 1) damp *= drate
    }
    return false
  }
  */



/*


def train(fidxFile: String, initFile: String, outFile: String, pvsize: Int, iterations: Int = 10) = {
var params = init(initFile, pvsize)
val nrExamples = countExamples(fidxFile)
for (i <- 0 until iterations) {
var nparams = sgdtrain(params, fidxFile, maxExamples = nrExamples, iteration = i)
if (i+1 == iterations) {
writeToFile(nparams.tail, outFile + ".pv")
}
else{
writeToFile(nparams.tail, outFile + "." + (i+1) + ".pv")
}
params = nparams
}
}

def test(fidxFile: String, initFile: String) = {
var params = init(initFile)
for (ex <- PotentialReader.read(fidxFile)) {
val words  = ex.attributes("@words").split(" ")
//			val tags = ex.attributes("@tagset").split(" ")
var pots = ex.potentials
val scoredpots = computePotentials(params, pots)
val names = scoredpots.map(_._1.name)
val ppots = scoredpots.map(_._2)
val parser = Parser.construct(names, ppots, words.size)
var conv = runBP(parser.graph, 5, 1, 0.1)
parser.decode(words)
println
}
}

def sgdtrain(params: Array[Double], fidxFile: String, maxExamples: Int = Int.MaxValue, rate: Double = 0.1, iteration: Int = 0): Array[Double] = { //pots: Array[Potential], scores: Array[Double], maxExamples: Int = 1): Array[Double] = {
var count = 0
var pvv = params.clone
for (ex <- PotentialReader.read(fidxFile) if count < maxExamples) {
val rate = -1.0 * (0.01 / (1.0 + ((count+1.0) + (iteration * maxExamples))/(maxExamples*1.0)))
var pots = ex.potentials
println("computing potentials")
val scoredpots = computePotentials(pvv, pots) //.map(_._2)
val words  = ex.attributes("@words").split(" ")
val corrects = pots.filter(_.isCorrect).map(_.name)
println("getting marges")
val marg = margfun(scoredpots, damp=0.099, words).map(p => if (corrects.contains(p._1)) Tuple(p._1, p._2 - 1.0) else p  )
println("marg update")
pvv = margupdate(pvv, pots, marg, rate=rate)
println("done")
count += 1
}
return pvv
}

def margfun(scoredpots: Array[(Potential, Double)], damp: Double = 0.99, words: Array[String]): Array[(String, Double)] = {
val names = scoredpots.map(_._1.name)
val ppots = scoredpots.map(_._2)
println("Constructing parser")
val model = Parser.construct(names, ppots, words.size)
println(model.toString)
println("Running BP")
var conv = runBP(model.graph, 5, damp, 0.1)
println("Getting beliefs")
val beliefs = model.potentialBeliefs
return beliefs
}

def margupdate(oldpv: Array[Double], pots: Array[Potential], margs: Array[(String, Double)], rate: Double = 1.0): Array[Double] = {
var newpv = oldpv
pots.zip(margs.map(_._2)).foreach { case(pot,gradient) =>
val grad = gradient * rate
for (feat <- pot.features) {
newpv(feat.idx) += grad * feat.value
}
}
return newpv
}

def runBP(graph: FactorGraph, bpiters: Int = 5, drate: Double = 0.99, dinit: Double, threshold: Double = 0): Boolean = {
val model = graph  // Should pass in a model and not need to do this, but tagger does not subclass FG yet
var maxDiff = -1.0
var damp = dinit
val mqueue = scala.collection.mutable.Queue[MessageNode]() // queue for everything not in uqueue
val uqueue = scala.collection.mutable.Queue[MessageNode]() // queue for unary factors, will probably rearrange and make this obsolete
// Load the queue, could add support for user pre-loading of the queue
for (fac <- model.factors) {
if (model.edgesFrom(fac).size == 1) {
uqueue += fac
}
else {
mqueue += fac
}
}
for (v <- model.variables) {
mqueue += v
}
// Do the unary facs first with no damping
for (u <- uqueue) {
u.computeMessages(model, damp=1)
}
// Do the others with successive damping
for (i <- 0 until bpiters) {
for (v <- mqueue) {
maxDiff = -1.0
var diff = v.computeMessages(model, damp=damp)
if (diff > maxDiff) maxDiff = diff
}
if (i > 0 && maxDiff < threshold) {  // Converged??
return true
}
if (i > 1) damp *= drate
}
return false
}

def init(initFile: String, pvsize: Int = 0): Array[Double] = {
assert(initFile != "null" || pvsize > 0, "Both init.file and pv.size are not specified correctly.")
if (initFile == "null") {
return Array.fill(pvsize+1)(0.0)
}
else {
val params1 = io.Source.fromFile(initFile).getLines().map(_.toDouble).toArray
val params = Array[Double](0.0) ++ params1
if (pvsize > params.size) {
return params ++ Array.fill(pvsize - (params.size+1))(0.0)
}
else {
return params
}
}
}

def computePotentials(params: Array[Double], pots: Array[Potential]): Array[(Potential, Double)] = {
var correctScore = 0.0
val scores = new ArrayBuffer[(Potential, Double)]
for (pot <- pots) {
var score = 0.0
for (feat <- pot.features) {
score += params(feat.idx) * feat.value
}
if (pot.isCorrect) correctScore += score
scores += Tuple(pot, score)
}
scores.toArray
}

def writeToFile(array: Array[Double], filename: String) {
val fw = new java.io.FileWriter(filename)
array.foreach{ p =>
fw.write(p + "")
fw.write("\n")
}
fw.close
}

def countExamples(filename: String): Int = {
print("Determining number of examples in fidx file...")
val endPattern   = """[ \n\t]*""".r
var count = 0
for (line <- scala.io.Source.fromFile(filename).getLines if line == "") count += 1
println(count + ".")
return count
}
}
*/
*/