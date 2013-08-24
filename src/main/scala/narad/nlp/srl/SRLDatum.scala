package narad.nlp.srl

import narad.util.ArgParser
import narad.bp.structure.Potential
import scala.collection.mutable.{ArrayBuffer, HashSet}
import narad.bp.optimize.Scorable
import narad.nlp.parser.metrics.EvalBContainer


class SRLDatum(grid: Array[Array[String]]) extends Scorable {
	private val preds = constructPreds(grid)
	private val args  = constructArgs(grid)

	def constructPreds(grid: Array[Array[String]]): Array[Int] = {
		grid.filter(_(12) == "Y").map(_(0).toInt)
	}
	
	def constructArgs(grid: Array[Array[String]]): Array[Array[String]] = {
		var args = Array.ofDim[String](slen+1, slen+1)
		val poffset = 13 //if (format == "CoNLL09") 13 else 10
		for (i <- 0 until grid.size) {		
			for (k <- poffset+1 until grid(i).size) {
				val label = grid(i)(k)
				val head  = preds(k-(poffset+1))
				args(head)(i+1) = label
			}
		}
		args
	}
	
	def getLabel(pidx: Int, aidx: Int) = args(pidx)(aidx)

	def hasArg(pidx: Int, aidx: Int) = hasPredicate(pidx) && args(pidx)(aidx) != "_" 

	def hasArgLabel(pidx: Int, aidx: Int, label: String) = args(pidx)(aidx) == label

	def hasPred(pidx: Int) = preds.contains(pidx) 

	def hasPredicate(pidx: Int) = preds.contains(pidx)
	
	def hasSense(pidx: Int, str: String) = grid(pidx-1)(13) == str
	
	def predicates: Array[Int] = preds 
	
	def form(i: Int) = grid(i-1)(1)
	
	def forms = (for (i <- 0 until slen) yield grid(i)(1))
	
	def frames: Array[SRLFrame] = {
		predicates.map { pidx =>
			val is = (1 to slen).toArray
			val fargs = is.filter{aidx => hasArg(pidx,aidx)}.map{aidx => new SRLArg(aidx, form(aidx), getLabel(pidx, aidx))}
			new SRLFrame(pidx, sense(pidx), fargs)
		}
	}

  def size = slen

	def labels = frames.map(_.args.map(_.label)).flatten.distinct.filter(_ != "_")

  def length = slen

	def lemma(i: Int) = grid(i-1)(2)

	def lemmas = (for (i <- 0 until slen) yield grid(i)(2))

	def plemma(i: Int) = grid(i-1)(3)
	
	def plemmas = (for (i <- 0 until slen) yield grid(i)(3))

	def postag(i: Int) = grid(i-1)(4)
	
	def postags = (for (i <- 0 until slen) yield grid(i)(4))

	def ppostag(i: Int) = grid(i-1)(5)
	
	def ppostags = (for (i <- 0 until slen) yield grid(i)(5))

	def feat(i: Int) = grid(i-1)(6)
	
	def feats = (for (i <- 0 until slen) yield grid(i)(6))

	def pfeats(i: Int) = grid(i-1)(7)

	def pfeats = (for (i <- 0 until slen) yield grid(i)(7))
	
	def head(i: Int) = grid(i-1)(8).toInt
	
	def heads = (for (i <- 0 until slen) yield grid(i)(8).toInt)

	def phead(i: Int) = grid(i-1)(9).toInt

	def pheads = (for (i <- 0 until slen) yield grid(i)(9).toInt)
	
	def deprel(i: Int) = grid(i-1)(10)
	
	def deprels = (for (i <- 0 until slen) yield grid(i)(10))

	def pdeprel(i: Int) = grid(i-1)(11)
	
	def pdeprels = (for (i <- 0 until slen) yield grid(i)(11))
	
	def sense(i: Int) = grid(i-1)(13)
	
	def slen = grid.size

  def words = forms

  def score(other: Scorable): SRLEvalContainer = {
    other match {
      case that: SRLDatum => SRLEvalContainer.construct(goldSRL=that, testSRL=this)
      case _=> new SRLEvalContainer()
    }
  }
	
	def tokens: Array[SRLToken] = {
		(1 to slen).toArray.map{i => new SRLToken(form(i), lemma(i), postag(i), ppostag(i), feat(i))}
	}
	
	override def toString = grid.map(_.mkString("\t")).mkString("\n")
}

object SRLDatum {
	
	// ID FORM LEMMA PLEMMA POS PPOS FEAT PFEAT HEAD PHEAD DEPREL PDEPREL FILLPRED PRED APREDs
	def constructFromCoNLL(lines: Array[String], format: String = "CoNLL09", coarsenLabels: Boolean=false): SRLDatum = {
		return new SRLDatum(lines.map(_.split("\t")))
	}
	
	def main(args: Array[String]) = {
		val ex = """1	The	the	the	DT	DT	_	_	2	2	NMOD	NMOD	_	_	_	_	_	_
		2	economy	economy	economy	NN	NN	_	_	4	4	NMOD	NMOD	_	_	A1	_	_	_
		3	's	's	's	POS	POS	_	_	2	2	SUFFIX	SUFFIX	_	_	_	_	_	_
		4	temperature	temperature	temperature	NN	NN	_	_	5	5	SBJ	SBJ	Y	temperature.01	A2	A1	_	_
		5	will	will	will	MD	MD	_	_	0	0	ROOT	ROOT	_	_	_	AM-MOD	_	_
		6	be	be	be	VB	VB	_	_	5	5	VC	VC	_	_	_	_	_	_
		7	taken	take	take	VBN	VBN	_	_	6	6	VC	VC	Y	take.01	_	_	_	_
		8	from	from	from	IN	IN	_	_	7	7	ADV	ADV	_	_	_	A2	_	_
		9	several	several	several	DT	DT	_	_	11	11	NMOD	NMOD	_	_	_	_	_	_
		10	vantage	vantage	vantage	NN	NN	_	_	11	11	NMOD	NMOD	_	_	_	_	A1	_
		11	points	point	point	NNS	NNS	_	_	8	8	PMOD	PMOD	Y	point.02	_	_	_	_
		12	this	this	this	DT	DT	_	_	13	13	NMOD	NMOD	_	_	_	_	_	_
		13	week	week	week	NN	NN	_	_	7	7	TMP	TMP	_	_	_	AM-TMP	_	_
		14	,	,	,	,	,	_	_	7	7	P	P	_	_	_	_	_	_
		15	with	with	with	IN	IN	_	_	7	7	ADV	ADV	_	_	_	AM-ADV	_	_
		16	readings	reading	reading	NNS	NNS	_	_	15	15	PMOD	PMOD	Y	reading.01	_	_	_	_
		17	on	on	on	IN	IN	_	_	16	16	NMOD	NMOD	_	_	_	_	_	A1
		18	trade	trade	trade	NN	NN	_	_	17	17	PMOD	PMOD	_	_	_	_	_	_
		19	,	,	,	,	,	_	_	18	18	P	P	_	_	_	_	_	_
		20	output	output	output	NN	NN	_	_	18	18	COORD	COORD	_	_	_	_	_	_
		21	,	,	,	,	,	_	_	20	20	P	P	_	_	_	_	_	_
		22	housing	housing	housing	NN	NN	_	_	20	20	COORD	COORD	_	_	_	_	_	_
		23	and	and	and	CC	CC	_	_	22	22	COORD	COORD	_	_	_	_	_	_
		24	inflation	inflation	inflation	NN	NN	_	_	23	23	CONJ	CONJ	_	_	_	_	_	_
		25	.	.	.	.	.	_	_	5	5	P	P	_	_	_	_	_	_"""
		val datum = constructFromCoNLL(ex.split("\n").map(_.trim))
		println(datum)
		assert(datum.predicates.toList == List(4,7,11,16), "Predicate array should be [4, 7, 11, 16], but is instead [%s]".format(datum.predicates.mkString(", ")))
		assert(datum.labels.size == 5, "Datum should have 5 labels (A1, A2, AM-MOD, AM-TMP, AM-ADV), has %d instead (%s)".format(datum.labels.size, datum.labels.mkString(", ")))
		assert(datum.hasArg(16,17), "Datum should have arg(16, 17)")
		println("Frames:")
		println(datum.frames.mkString("\n"))
	}
}


































































/*

class SRLDatum(val slen: Int) {
	var forms 		= Array.fill[String](slen)("form")
	var lemmas  	= Array.fill[String](slen)("lemma")
	var plemmas  	= Array.fill[String](slen)("plemma")
	var postags 	= Array.fill[String](slen)("POS")
	var ppostags  = Array.fill[String](slen)("PPOS")
	var feats 		= Array.fill[String](slen)("feats")
	var pfeats  	= Array.fill[String](slen)("pfeats")
	var heads  		= Array.fill[Int](slen)(1)
	var pheads  	= Array.fill[Int](slen)(1)
	var deprels  	= Array.fill[String](slen)("REL")
	var pdeprels  = Array.fill[String](slen)("PREL")

	var predicates = Array[String]()
	var dependencies = Array[Array[String]]()
	
	def args: Array[Int] = {
		val abuff = new ArrayBuffer[Int]
		for (aidx <- 1 to slen; pidx <- predicateIndices if hasArg(pidx, aidx)) abuff += aidx
		return abuff.toArray.distinct 
	}
	
	def containsPredicates = predicates.filter(_ != "_").size > 0
	
	def getLabel(pidx: Int, aidx: Int) = dependencies(aidx-1)(pidx-1)

	def hasArg(pidx: Int, aidx: Int) = dependencies(aidx-1)(pidx-1) != "_"

	def hasArgLabel(pidx: Int, aidx: Int, label: String) = dependencies(aidx-1)(pidx-1) == label

	def hasPred(pidx: Int) = if (pidx > 0 && pidx <= predicates.size) predicates(pidx-1) != "_" else false

	def hasPredLabel(pidx: Int, label: String) = predicates(pidx-1) == label

	def hasSense(pidx: Int, str: String) = predicates(pidx-1) == str

	def labels = {
		val labels = new HashSet[String]
		for (i <- 0 until dependencies.size; j <- 0 until dependencies(i).size) {
			if (dependencies(i)(j) != "_") labels += dependencies(i)(j)
		}
		labels.toArray
	}
	
	def labelArray = {
		val labels = new ArrayBuffer[String]
		for (i <- 0 until dependencies.size; j <- 0 until dependencies(i).size) {
			if (dependencies(i)(j) != "_") labels += dependencies(i)(j)
		}
	 labels.toArray
	}

	def predicateIndices: Iterator[Int] = predicates.zipWithIndex.filter{case(p,i) => p != "_"}.map{case(p,i) => i+1}.iterator

	override def toString = toCoNLLString(format = "CoNLL09")

	def fillPred(pidx: Int) = predicates(pidx-1) != "_"
	
	def size = slen

	def toCoNLLString(format: String = "CoNLL09"): String = {
		val lines = new ArrayBuffer[String]
		for (i <- 1 to slen) {
			format match {
				case "CoNLL08" => { 
					System.err.println("2008 conll tostring not implemented.")
				}
				case "CoNLL09" => {
					val cells = Array[String](
						(i).toString,		// ID
						forms(i-1),			// form
						lemmas(i-1),		// lemma
						plemmas(i-1),		// plemma
						postags(i-1),		// pos
						ppostags(i-1),	// ppos
						feats(i-1),			// feat
						pfeats(i-1),		// pfeat
						heads(i-1).toString,			// head
						pheads(i-1).toString,			// phead
						deprels(i-1),		// deprel
						pdeprels(i-1),	// pdeprel
						if (fillPred(i)) "Y" else "_",
						predicates(i-1),
						predicateIndices.map{ pidx => if (hasArg(pidx, i)) getLabel(pidx, i) else "_" }.mkString("\t"))
						lines += cells.mkString("\t")					
				}
			}
		}
		return lines.mkString("\n")	
	}
	
	def toFrames: Array[SRLFrame] = {
		predicateIndices.map { pidx =>
			val pred = predicates(pidx)
			"%s: %s".format(args(pidx) )
		}
		for (pidx <- predicateIndices) {
			val pred = predicates(pidx)
			
			
		}
	}
	
	def tags = postags
	
	def tokens: Array[SRLToken] = (0 until words.size).toArray.map(i => new SRLToken(words(i), lemmas(i), tags(i), ppostags(i), pfeats(i)))
	
	def words = forms
	
}


object SRLDatum {

	def coarsen(str: String): String = {
		if (str.startsWith("C-") || str.startsWith("R-")) 
		return  str.substring(2, str.size)
		else
		return str
	}

	def constructFromCoNLL(lines: Array[String], format: String = "CoNLL09", coarsenLabels: Boolean=false): SRLDatum = {
		assert(format == "CoNLL08" || format == "CoNLL09", "Format <%s> is invalid in SRLDatum.constructFromCoNLL".format(format))
		val slen = lines.size
		val predbuff = new ArrayBuffer[String]

		var forms 		= new ArrayBuffer[String]
		var lemmas  	= new ArrayBuffer[String]
		var plemmas  	= new ArrayBuffer[String]
		var postags 	= new ArrayBuffer[String]
		var ppostags  = new ArrayBuffer[String]
		var feats 		= new ArrayBuffer[String]
		var pfeats  	= new ArrayBuffer[String]
		var heads  		= new ArrayBuffer[Int]
		var pheads  	= new ArrayBuffer[Int]
		var deprels  	= new ArrayBuffer[String]
		var pdeprels  = new ArrayBuffer[String]

		var predicates = new ArrayBuffer[String]
		var dependencies = Array[Array[String]]()

		val poffset = if (format == "CoNLL09") 13 else 10
		val preds = lines.zipWithIndex.filter{case(l,i) => 
			val cells = l.split("\t")
//			println(l)
//			println(cells.size)
			cells(poffset) != "_"
		}.map{case(l,i) => i}
		val alabels = Array.fill(slen, slen)("_")
		for (i <- 0 until slen) {
//			println(lines(i))
			val cols = lines(i).split("\t")
			//			println(cols.mkString("||"))
			format match {
				case "CoNLL08" => {
					System.err.println("No util present for CoNLL08")
					System.exit(1)
				}				
				// ID FORM LEMMA PLEMMA POS PPOS FEAT PFEAT HEAD PHEAD DEPREL PDEPREL FILLPRED PRED APREDs
				case "CoNLL09" => {
					forms 	+= cols(1)
					lemmas 	+= cols(2)
					plemmas += cols(3)
					postags	+= cols(4)
					ppostags += cols(5)
					feats		+= cols(6)
					pfeats	+= cols(7)
					heads		+= cols(8).toInt
					pheads	+= cols(9).toInt
					deprels	+= cols(10)
					pdeprels += cols(11)
					predbuff += cols(poffset)
				}
				for (k <- poffset+1 until cols.size) {
					if (cols(k) != "_") {
						val label = if (coarsenLabels) coarsen(cols(k)) else cols(k)
						alabels(i)(preds(k-(poffset+1))) = label						
					}
				}
			}
		}
		val datum = new SRLDatum(slen)
		datum.forms = forms.toArray
		datum.lemmas = lemmas.toArray
		datum.plemmas = plemmas.toArray
		datum.postags = postags.toArray
		datum.ppostags = ppostags.toArray
		datum.feats = feats.toArray
		datum.pfeats = pfeats.toArray
		datum.heads = heads.toArray
		datum.pheads = pheads.toArray
		datum.deprels = deprels.toArray
		datum.pdeprels = pdeprels.toArray
		
		datum.predicates = predbuff.toArray
		datum.dependencies = alabels
		return datum
	}	
}

*/










































			/*
			case class SRLDatum(predicates: Array[String], dependencies: Array[Array[String]], words: Array[String], lemmas: Array[String], tags: Array[String], ctags: Array[String], heads: Array[Int]) {  //Array[(Int, (Int, String))]) {

				val toks: Array[SRLToken] = (0 until words.size).toArray.map(i => new SRLToken(words(i), lemmas(i), tags(i), ctags(i)))


				def args: Array[Array[String]] = dependencies

				def getLabel(pidx: Int, aidx: Int) = dependencies(aidx-1)(pidx-1)

				def hasArg(pidx: Int, aidx: Int) = dependencies(aidx-1)(pidx-1) != "_"

				def hasArgLabel(pidx: Int, aidx: Int, label: String) = dependencies(aidx-1)(pidx-1) == label

				def hasPred(pidx: Int) = if (pidx > 0 && pidx <= predicates.size) predicates(pidx-1) != "_" else false //predicates.contains(pidx-1)

				def hasPredLabel(pidx: Int, label: String) = predicates(pidx-1) == label

				def hasSense(pidx: Int, str: String) = predicates(pidx-1) == str

				//	def heads = val heads = Array(-1) ++ lines.map(_.split("\t")(8).toInt)

				def slen = words.size

				def size = words.size

				def labels = {
					val labels = new HashSet[String]
					for (i <- 0 until dependencies.size; j <- 0 until dependencies(i).size) {
						if (dependencies(i)(j) != "_") labels += dependencies(i)(j)
					}
					labels.toArray
				}

				//	def preds: Iterator[(Int, String)] = predicates.iterator

				def predicateIndices: Iterator[Int] = predicates.zipWithIndex.filter{case(p,i) => p != "_"}.map{case(p,i) => i+1}.iterator

				//	def predStrings: Iterator[String] = (predicates.foreach {pidx => yield words(pidx)})

				def tokens = toks

				override def toString = toCoNLLString(format = "CoNLL09")

				def fillPred(pidx: Int) = predicates(pidx-1) != "_"

				def toCoNLLString(format: String = "CoNLL09"): String = {
					//		println(predicates.mkString(", "))
					//		println(predicateIndices.mkString(", "))
					val lines = new ArrayBuffer[String]
					for (i <- 1 to slen) {
						format match {
							case "CoNLL08" => {  // Havent updated this to do proper predicate sense
								val cells = Array[String](
									(i).toString,
									words(i-1),
									words(i-1), //lemmas(i),
									tags(i-1), //tags(i),
									tags(i-1), //ctags(i),
									"_", //words(i),
									"_", //words(i).toLowerCase,
									"_", //tags(i),
									"1", //heads(i),
									"SREL", //rels(i),
									predicates(i-1), //if (preds.contains(i-1)) words(i-1) else "_",
									predicateIndices.map{ pidx => if (hasArg(pidx, i)) getLabel(pidx, i) else "_" }.mkString("\t"))
									lines += cells.mkString("\t")					
								}
								case "CoNLL09" => {
									val cells = Array[String](
										(i).toString,  // ID
										words(i-1),    // form
										words(i-1),    // lemma
										words(i-1),    // plemma
										tags(i-1),     // pos
										tags(i-1),     // ppos
										"_", 					 // feat
										"_",           // pfeat
										"1",           // head
										"1",           // phead
										"SREL",        // deprel
										"SREL",        // pdeprel
										if (fillPred(i)) "Y" else "_",
										predicates(i-1), //if (fillpred) words(i-1) else "_",
										predicateIndices.map{ pidx => if (hasArg(pidx, i)) getLabel(pidx, i) else "_" }.mkString("\t"))
										lines += cells.mkString("\t")					
									}
								}
							}
							return lines.mkString("\n")	
						}

						def toInternalString: String = {
							val sb = new ArrayBuffer[String]
							sb += "Argument Matrix\n"
							for (i <- 0 until args.size) {
								for (j <- 0 until args(i).size) {
									sb += args(i)(j) + "\t"
								}
								sb += "\n"
							}
							return sb.mkString("")
						}

					}

					object SRLDatum {
						val predPattern  = """pred\(([0-9]+)\)""".r
						val argPattern   = """hasArg\(([0-9]+),([0-9]+)\)""".r
						val labelPattern = """hasLabel\(([0-9]+),([0-9]+),(.+)\)""".r
						val sensePattern = """predSense\(([0-9]+),(.+)\)""".r
						val argPartPattern = """.*([A-Za-z0-9]+)\-([A-Za-z0-9]+)$""".r

						val oargPattern = """A([0-9]+)""".r


						def coarsen(str: String): String = {
							if (str.startsWith("C-") || str.startsWith("R-")) 
							return  str.substring(2, str.size)
							else
							return str
						}

						def constructFromCoNLL(lines: Array[String], format: String = "CoNLL09", coarsenLabels: Boolean=true): SRLDatum = {
							assert(format == "CoNLL08" || format == "CoNLL09", "Format <%s> is invalid in SRLDatum.constructFromCoNLL".format(format))
							val slen = lines.size
							val words = new ArrayBuffer[String]
							val lemmas = new ArrayBuffer[String]
							val tags  = new ArrayBuffer[String]
							val ctags = new ArrayBuffer[String]
							val heads = new ArrayBuffer[Int]
							val predbuff = new ArrayBuffer[String]

							val poffset = if (format == "CoNLL09") 13 else 10
							val preds = lines.zipWithIndex.filter{case(l,i) => l.split("\t")(poffset) != "_"}.map{case(l,i) => i}
							val alabels = Array.fill(slen, slen)("_")
							for (i <- 0 until slen) {
								val cols = lines(i).split("\t")
								//			println(cols.mkString("||"))
								format match {
									case "CoNLL08" => {
										words += cols(1)
										lemmas += cols(2)
										tags  += cols(3)			
										ctags += cols(4)
										heads += cols(8).toInt
										predbuff += cols(poffset)
									}				
									// ID FORM LEMMA PLEMMA POS PPOS FEAT PFEAT HEAD PHEAD DEPREL PDEPREL FILLPRED PRED APREDs
									case "CoNLL09" => {
										words += cols(1)
										lemmas += cols(2)
										tags  += cols(4)
										ctags += cols(5)
										heads += cols(8).toInt
										predbuff += cols(poffset)
									}
									for (k <- poffset+1 until cols.size) {
										if (cols(k) != "_") {
											val label = if (coarsenLabels) coarsen(cols(k)) else cols(k)
											alabels(i)(preds(k-(poffset+1))) = label						
										}
									}
								}
							}
							return new SRLDatum(predbuff.toArray, alabels, words.toArray, lemmas.toArray, tags.toArray, ctags.toArray, heads.toArray)	
						}

						def constructFromBeliefs(beliefs: Array[Potential], words: Array[String], tags: Array[String]): SRLDatum = {
							val abeliefs = beliefs.filter(_.name.startsWith("hasArg"))
							val lbeliefs = beliefs.filter(_.name.startsWith("hasLabel"))

							val preds = new Array[String](words.size)
							for (i <- 0 until preds.size) preds(i) = "_"
							beliefs.filter(b => b.name.contains("pred(")).foreach { p => // && b.value > 0.5).foreach { p =>
								val predPattern(pidx) = p.name
								if (p.value > 0.5) {
									preds(pidx.toInt-1) = sense(pidx.toInt, beliefs)				
								}
								else {
									preds(pidx.toInt-1) = "_"
								}
							}

							//		val preds = beliefs.filter(b => b.name.contains("pred") && b.value > 0.5).map { p =>
								//val predPattern(pidx) = p.name; pidx.toInt-1
								//		}
								/*
								val preds = beliefs.filter(b => b.name.contains("pred") && b.value > 0.5).map { p =>
									val predPattern(pidx) = p.name; pidx.toInt-1
								}
								*/
								val args = Array.ofDim[String](words.size, words.size)
								for (i <- 0 until words.size; j <- 0 until words.size) args(i)(j) = "_"
								for (belief <- abeliefs) {
									val argPattern(pred, dep) = belief.name
									if (belief.value > 0.5) { 
										args(dep.toInt-1)(pred.toInt-1) = label(pred.toInt, dep.toInt, lbeliefs)   //(preds.indexOf(pred.toInt)) = label(dep.toInt, pred.toInt, lbeliefs)
									}
								}
								// NEEDS TO CHANGE THIS TO BE RELEVANT
								return new SRLDatum(preds, args, words, words, tags, tags, Array[Int](words.size))
							}


							def label(pidx: Int, aidx: Int, beliefs: Array[Potential]): String = {
								val lbeliefs = beliefs.filter(_.name.matches("hasLabel\\(%s,%s,(.+)\\)".format(pidx, aidx)))
								var maxv = lbeliefs.map(_.value).max
								val labels = lbeliefs.filter(_.value == maxv).map { b =>
									val labelPattern(pi, ai, l) = b.name; l
								}
								assert(labels.size > 0, "No labels found in SRL Decoding for %d, %d.".format(aidx, pidx))
								labels.first
							}

							def sense(pidx: Int, beliefs: Array[Potential]): String = {
								//			val lbeliefs = beliefs.filter(_.name.matches("predSense\\(%d,(.+)\\)".format(pidx)))
								//			println("pidx = " + pidx)
								//			println("beliefs size = " + beliefs.size)
								//			val pat = 
								//			println(pat)
								//			for (b <- beliefs) {
									//				println(b.name + "\t" + b.name.contains(pat)) 
									//			}
									val lbeliefs = beliefs.filter(_.name.contains("predSense(%d,".format(pidx)))
									//			println(lbeliefs.size)
									var maxv = lbeliefs.map(_.value).max
									val labels = lbeliefs.filter(_.value == maxv).map { b =>
										val sensePattern(pi, l) = b.name; l
									}
									assert(labels.size > 0, "No sense label found in SRL Decoding for %d.".format(pidx))
									labels.first
								}		
							}




							*/


							/*
							val slen = words.size
							words = words
							lemmas = words
							tags = tags
							ctags = tags
							heads = Array.ofDim(slen)(1)
							rels = Array.ofDim(slen)("SREL")
						}
						*/

						//		str match {
							//			case argPartPattern(l1, l2) => "%s-%s".format(l1, l2)
							//			case _ => str
							//		}
							/*
							val cells = str.split("-")
							if (cells.size > 2) {
								return "%s-%s".format(cells(cells.size-2), cells(cells.size-1))
							}
							str match {
								case oargPattern(digit) => return "A%s".format(digit)
								case _ => return str
							}
							*/


							/*
							println("PRED: %s".format(preds.mkString(", ")))
							println("ARGS: \n%s".format(args.map(_.mkString(", ")).mkString("\n")))
							println("words: %s".format(words.mkString(", ")))
							println("tags: %s".format(tags.mkString(", ")))
							*/
							/*
							var words  = Array[String]
							var lemmas = Array[String]
							var tags   = Array[String]
							var ctags  = Array[String]
							var heads  = Array[Int]
							var rels   = Array[String]
							*/	
