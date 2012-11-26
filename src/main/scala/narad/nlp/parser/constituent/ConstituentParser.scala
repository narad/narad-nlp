/*
package narad.nlp.parser.constituent
import narad.bp.structure._
import narad.nlp.trees.{Span, Token, Tree}
import narad.io.reader.{SpanReader, TreeReader}
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
	
	def decode(words: Array[String], tags: Array[String]): Tree = {
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
	
	
	def featurizeSyntax(ctree: Tree, stats: ParserStatistics, prune: Boolean, out: FileWriter, 
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








