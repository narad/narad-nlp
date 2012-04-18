package narad.projects.cparser
import narad.bp.structure._
import narad.nlp.parse.{Span, SpanReader, Tree}
import narad.projects.bpdp._
import scala.collection.mutable.ArrayBuffer
import scala.util.matching._

class Parser(var graph: FactorGraph) extends Model {
	val indicesPattern    = """\(([0-9]+),([0-9]+)\) """.r
	val brackPattern      = """brack\(([0-9]+),([0-9]+)\)""".r
	val labelPattern      = """brackLabel(.+)\(([0-9]+),([0-9]+)\)""".r
	val unaryPattern      = """unary\(([0-9]+),([0-9]+)\)""".r
	val unaryLabelPattern = """unaryLabel(.+)\(([0-9]+),([0-9]+)\)""".r
	
	def indices(s: String): (Int, Int) = s match {
		case indicesPattern(start, end) => Tuple(start.toInt, end.toInt)
		case _ => Tuple(-1, -1)
	}
	
	def maxLabel(pots: Array[Potential]): String = {
		println("label potentials size = %d".format(pots.size))
		var max = Double.NegativeInfinity
		var label = null.asInstanceOf[String]
		for (pot <- pots) {
			if (pot.value > max) {
				max = pot.value
				if (pot.name.contains("brack")) {
					val labelPattern(plabel, pstart, pend) = pot.name
					label = plabel					
				}
				else if (pot.name.contains("unary")) {
					val unaryLabelPattern(plabel, pstart, pend) = pot.name
					label = plabel										
				}
			}
		}
		return label
	}

	def potentialBeliefs: Array[Potential] = {
		val beliefs = new ArrayBuffer[Potential]
		for (factor <- graph.factors) { println("beliefs  in " + factor.name + " = " + factor.getBeliefs(graph).mkString(";")); beliefs ++= factor.getBeliefs(graph) }
		println(beliefs.size)
		return beliefs.toArray
	}
	
		
	def variableBeliefs: Array[(String, Double)] = {
		val beliefs = new ArrayBuffer[(String, Double)]
//		for (factor <- graph.factors.filter(_.name != "CKY")) { beliefs ++= factor.getBeliefs(graph) }
		for (variable <- graph.variables) { beliefs ++= variable.getBeliefs(graph) }
		return beliefs.toArray
	}
	
	def decode(words: Array[String], tags: Array[String]): Tree = {
//		println("Decoding with words: %s".format(words.mkString(" ")))
		val slen = words.size
		val beliefs = potentialBeliefs
//		println("pot_beliefs:")
//		beliefs.foreach(println(_))
		val ckybeliefs = ckyBrackets(beliefs.filter(_.name.startsWith("brack")), slen)
		val spans = ckybeliefs.filter(_.value == 1).map { brack =>
			val brackPattern(start, end) = brack.name
			val label = maxLabel(beliefs.filter(_.name.matches("brackLabel.+\\(%s,%s\\)".format(start, end))))
			new Span(start.toInt, end.toInt, label, false)
		}
//		println("spans:")
//		spans.foreach(println(_))
		
		val uspans = potentialBeliefs.filter(p => p.name.contains("unary(") && p.value > 0.5).map { unary =>
			val unaryPattern(start, end) = unary.name
			val label = maxLabel(beliefs.filter(_.name.matches("unaryLabel.+\\(%s,%s\\)".format(start, end))))
			new Span(start.toInt, end.toInt, label, true)
		}
//		println("unaries:")
		uspans.foreach(println(_))
		val tree = SpanReader.spansToTree(spans.filter(!_.label.contains("@")) ++ uspans, slen)
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
	
	def ckyBrackets(potentials: Array[Potential], slen: Int): Array[Potential] = {
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
			beta(i)(k) = inside + sp.value  // WTF?
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
			ckyBacktrace(i, j, split, brack)
		}
		if (j < (k-1)) {
			brack(j)(k) = true
			ckyBacktrace(j, k, split, brack)
		}
		return 1
	}
		
	override def toString = graph.toString
}

object Parser {
	val brackPattern 			= """brack\(([0-9]+),([0-9]+)\)""".r
	val brackLabelPattern = """brackLabel(.+)\(([0-9]+),([0-9]+)\)""".r
	val unaryPattern 			= """unary\(([0-9]+),([0-9]+)\)""".r
	val unaryLabelPattern = """unaryLabel(.+)\(([0-9]+),([0-9]+)\)""".r
	 
	
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
//					if (start == 0 && end == slen) {
//						fg.addTable1Factor("brackvar(%d,%d)".format(start, end), "brackfac(%d,%d)".format(start,end), Array[Potential](new Potential(0.0, "-brack(%d,%d)".format(start, end), false), Array[Double](0,1))				
//					}
//					else {
						fg.addUnaryFactor("brackvar(%d,%d)".format(start, end), "brackfac(%d,%d)".format(start,end), Array(pots(i)))				
//					}
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
//			fg.addIsAtMost1Factor("brackvar\\(%d,%d\\)".format(start, end), "label.+var\\(%d,%d\\)".format(start, end), "isAtMost(%d,%d)".format(start, end))
				fg.addIsAtMost1Factor(new Regex("brackvar\\(%d,%d\\)".format(start, end)), new Regex("label.+var\\(%d,%d\\)".format(start, end)), "isAtMost(%d,%d)".format(start, end))
		}
		for (start <- 0 until slen) {
			val end = start+1
//			fg.addIsAtMost1Factor("unaryvar\\(%d,%d\\)".format(start, end), "unaryLabel.+var\\(%d,%d\\)".format(start, end), "isAtMost(%d,%d)".format(start, end))
				fg.addIsAtMost1Factor(new Regex("unaryvar\\(%d,%d\\)".format(start, end)), new Regex("unaryLabel.+var\\(%d,%d\\)".format(start, end)), "isAtMost(%d,%d)".format(start, end))
		}
		new Parser(fg.toFactorGraph)
	}

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












