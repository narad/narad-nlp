package narad.nlp.srl
import java.io.{File, PrintWriter}
import java.text.DecimalFormat
import narad.util.ArgParser
import narad.io.util.{ChunkReader, ZippedReader}
import narad.bp.structure._
import scala.collection.mutable.{ArrayBuffer, HashMap}

object SRLEval {


	def main(args: Array[String]) = {
		val options = new narad.util.ArgParser(args)
		val bpdp = options.getString("--bpdp.file")
		val dict = options.getString("--dict", "srl.senses")
		val iformat = options.getString("--input.format", "ISO-8859-1")
		val oformat = options.getString("--output.format", "ISO-8859-1")
		val outputFile = options.getString("--output.file", "srl.out")
		val suffixFile = options.getString("--suffix.file", "srl.suffixes")
    val trainFile  = options.getString("--train.file")
		val threshold = options.getDouble("--decoding.threshold", 0.5)
//		val suffixes = io.Source.fromFile(suffixFile).getLines().toArray
//		val maxsuffix = suffixes(0)
  val maxsuffix = "A0"
		val maxmode = options.getString("--max.mode", "APPEND") // otherwise "SAME"

		if (bpdp != null) {
/*
			val senseDict = new HashMap[String, Array[String]]
			for (line <- io.Source.fromFile(dict).getLines()) {
//			for (line <- io.Source.fromFile(new File(dict), iformat).getLines) {
				val cols = line.split("\t"); senseDict(cols(0)) = cols(1).split(" ")
			}
*/
       val dict = SRLDictionary.construct(trainFile)
			var out = new java.io.PrintWriter(new File(outputFile), oformat);
			var i = 1
			for (datum <- convertBPDPtoScala(bpdp, dict, iformat, maxmode=maxmode, maxsense=maxsuffix, threshold=threshold)) {
				out.println(datum)
				out.println()
				if (i % 10 == 0) System.err.println("Processing datum %d...".format(i))
				i += 1
			}
			out.close()
		}
		else {
			val gold = options.getString("--gold.file")
			val test = options.getString("--test.file")
			assert(gold != null, "Missing argument for gold file (--gold.file).")
			assert(test != null, "Missing argument for test file (--test.file).")
			eval(gold, test, options)			
		}
	}
	

	def eval(goldFile: String, testFile: String, options: ArgParser) = {
		var numPredicates = 0
		var numCorrectSenses = 0
		
		val goldValencies = new Array[Int](20)
		val testValencies = new Array[Int](20)
		
		val senseDict = new HashMap[String, Array[String]]
		val verbose = options.getBoolean("--verbose", false)
		if (options.contains("--sense.file")) {
			val senseFile = new File(options.getString("--sense.file", "srl.senses"))
			for (line <- io.Source.fromFile(senseFile).getLines()) {val cols = line.split("\t"); senseDict(cols(0)) = cols(1).split(" ")}
		}
		for (t <- ZippedReader.read(goldFile, testFile)) {
			val gdatum = SRLDatum.constructFromCoNLL(t._1.split("\n"))
			val tdatum = SRLDatum.constructFromCoNLL(t._2.split("\n"))
			for (i <- 1 to gdatum.slen) {
				val lemma = gdatum.lemma(i)
				if (gdatum.hasPredicate(i) && tdatum.hasPredicate(i)) {
					if (gdatum.sense(i) == tdatum.sense(i)) {
						numCorrectSenses += 1
					}
					else if (verbose) {
						if (senseDict.size > 0 && !senseDict.contains(lemma)) {
							System.err.println("Word %s (unseen) incorrectly given sense %s instead of %s.".format(gdatum.form(i), tdatum.sense(i), gdatum.sense(i)))							
						}
						else {
							System.err.println("Word %s incorrectly given sense %s instead of %s.".format(gdatum.form(i), tdatum.sense(i), gdatum.sense(i)))														
						}
					}
					numPredicates += 1
				}
				if (gdatum.hasPredicate(i)) {
					val acount = (1 to gdatum.slen).filter(gdatum.hasArg(i,_)).size
					goldValencies(acount) += 1
				}
				if (tdatum.hasPredicate(i)) {
					val acount = (1 to tdatum.slen).filter(tdatum.hasArg(i,_)).size
					testValencies(acount) += 1
				}
			}
		}
		val formatter = new DecimalFormat("0.00")
		println("Predicate Accuracy:   (%d / %d) = %s".format(numCorrectSenses, numPredicates, formatter.format(numCorrectSenses / numPredicates.toDouble)))	
		println("Valencies:")
		println("\tGOLD\tTEST")
		for (i <- 0 until testValencies.size) {
			println("%d\t%d\t%d".format(i, goldValencies(i), testValencies(i)))
		}

//		println("Empty Preds:   (%d / %d) = %s".format(emptyPreds, numPredicates, (emptyPreds.toDouble / numPredicates.toDouble)))
	}

  val ATTRIBUTE_PATTERN = """@([^\t]+)\t *(.+)""".r
  val POT_PATTERN  = """(.*)\t(.+)""".r

  def convertBPDPtoScala(filename: String, dict: SRLDictionary, iformat: String = "UTF-8", maxmode: String="APPEND", maxsense: String = "01", threshold: Double = 0.5): Array[SRLDatum] = {
    val reader = new ChunkReader(filename)
    val datums = new ArrayBuffer[SRLDatum]()
    reader.iterator.foreach { chunk =>
      val pots = new ArrayBuffer[Potential]
      val attributes = new HashMap[String, String]
      for (line <- chunk.split("\n")) {
        line match {
          case ATTRIBUTE_PATTERN(attrName, attrStr) => {
            attributes(attrName.trim) = attrStr.trim
          }
          case POT_PATTERN(potName, potWeight) => {
            try {
              pots += new Potential(potWeight.toDouble, potName.trim, false)
            }
            catch { case e: Exception => }
          }
        }
      }
      val words   = attributes.getOrElse("words", "").split(" ")
      val tags    = attributes.getOrElse("tags", "").split(" ")
      val lemmas  = attributes.getOrElse("lemmas", "").split(" ")
      val roles   = attributes.getOrElse("roles", "").split(" ")
      val gpreds  = attributes.getOrElse("gpreds", "").split(" ").map(_.toInt).filter(_>0)
      gpreds.foreach { p =>
        if (words.size > 0) pots += new Potential(1.0, "pred(%s)".format(p), false)
      }
      datums += constructFromBeliefs(pots.toArray, words, tags, lemmas, roles, dict, maxsense=maxsense, maxmode=maxmode, threshold=threshold)
    }
    datums.toArray
  }

/*
		val data = new ArrayBuffer[SRLDatum]
			val attrPattern = """@([^\t]+)\t *(.+)""".r
			val potPattern  = """(.*) (.+)""".r
			val pots = new ArrayBuffer[Potential]
			val attributes = new HashMap[String, String]
			val lines = io.Source.fromFile(filename).getLines()
//			val lines = io.Source.fromFile(new File(filename), iformat).getLines
			for (line <- lines) {
		//			println(line)
				line match {		
					case attrPattern(attrName, attrStr) => {
						attributes(attrName.trim) = attrStr.trim
					}
					case potPattern(potName, potWeight) => {
							println("POTS MATCH!")
						try {
							pots += new Potential(potWeight.toDouble, potName.trim, false)						
						}
						catch { case e: Exception => }
					}
					case "" => {
						if (attributes.contains("words")) {
							val words = attributes.getOrElse("words", "").split(" ")
							val tags  = attributes.getOrElse("tags", "").split(" ")
							val lemmas = attributes.getOrElse("lemmas", "").split(" ")
							val roles = attributes.getOrElse("roles", "").split(" ")
							val gpreds = attributes.getOrElse("gpreds", "").split(" ").map(_.toInt).filter(_>0)
							gpreds.foreach { p =>
								if (words.size > 0) pots += new Potential(1.0, "pred(%s)".format(p), false)														
							}
							data += constructFromBeliefs(pots.toArray, words, tags, lemmas, roles, dict, maxsense=maxsense, maxmode=maxmode, threshold=threshold)
							pots.clear()
							attributes.clear()
						}
					}
					case _ =>
				}			
			}
			if (attributes.contains("words")) { 
				val words = attributes.getOrElse("words", "").split(" ")
				val tags  = attributes.getOrElse("tags", "").split(" ")
				val lemmas = attributes.getOrElse("lemmas", "").split(" ")
				val roles = attributes.getOrElse("roles", "").split(" ")
				val gpreds = attributes.getOrElse("gpreds", "").split(" ").map(_.toInt).filter(_>0)
				gpreds.foreach { p =>
					if (words.size > 0) pots += new Potential(1.0, "pred(%s)".format(p), false)														
				}
				data += constructFromBeliefs(pots.toArray, words, tags, lemmas, roles, dict, maxsense=maxsense, maxmode=maxmode, threshold=threshold)
			}
		return data.toArray
	}
  */





	def constructFromBeliefs(beliefs: Array[Potential], words: Array[String], tags: Array[String], lemmas: Array[String], roles: Array[String],
			dict: SRLDictionary, maxmode: String = "APPEND", maxsense: String = "01", threshold: Double = 0.5): SRLDatum = {

			val predPattern  = """pred\(([0-9]+)\)""".r
			val argPattern   = """hasArg\(([0-9]+),([0-9]+)\)""".r


			val slen = words.size
			val heads = new Array[Int](slen)
			val abeliefs = beliefs.filter(_.name.startsWith("hasArg"))
			val lbeliefs = beliefs.filter(_.name.startsWith("hasLabel"))
			val sbeliefs = beliefs.filter(_.name.startsWith("sense"))
			val preds = new Array[String](slen)
			for (i <- 0 until preds.size) preds(i) = "_"

			beliefs.filter(b => b.name.contains("pred(")).foreach { p => 
				val predPattern(pidx) = p.name
				if (p.value > threshold) {
					preds(pidx.toInt-1) = sense(pidx.toInt, lemmas(pidx.toInt-1), sbeliefs, dict, maxsense=maxsense, maxmode=maxmode)
				}
				else {
					preds(pidx.toInt-1) = "_"
				}
			}

			val args = Array.ofDim[String](words.size, words.size)
			for (i <- 0 until words.size; j <- 0 until words.size) args(i)(j) = "_"
			for (belief <- abeliefs) {
				val argPattern(pred, dep) = belief.name
				if (belief.value > threshold) { 
					args(dep.toInt-1)(pred.toInt-1) = roles(label(pred.toInt, dep.toInt, lbeliefs).toInt)   //(preds.indexOf(pred.toInt)) = label(dep.toInt, pred.toInt, lbeliefs)
				}
			}

			for (i <- 1 to slen) {
				val lbeliefs = beliefs.filter(b => b.name.matches("un\\([0-9]+,%d\\)".format(i)))
				var mbelief = null.asInstanceOf[Potential]
				var mb = Double.NegativeInfinity
				for (b <- lbeliefs) {
					if (b.value >= mb) {
						mb = b.value
						mbelief = b
					}
				}
				if (mbelief != null) {
					val mname = mbelief.name
					val head = mname.substring(mname.indexOf("(")+1, mname.indexOf(",")).toInt
					heads(i-1) = head					
				}
				else {
					heads(i-1) = -1
				}
			}


			// ID FORM LEMMA PLEMMA POS PPOS FEAT PFEAT HEAD PHEAD DEPREL PDEPREL FILLPRED PRED APREDs
			val pidxs = preds.zipWithIndex.filter(p => p._1 != "_").map(p => p._2)

			val grid = Array.ofDim[String](slen, 14 + pidxs.size)
			for (i <- 1 to slen) grid(i-1)(0) = i.toString
			for (i <- 0 until slen) grid(i)(1) = words(i)
			for (i <- 0 until slen) grid(i)(2) = lemmas(i)
			for (i <- 0 until slen) grid(i)(3) = lemmas(i)
			for (i <- 0 until slen) grid(i)(4) = tags(i)
			for (i <- 0 until slen) grid(i)(5) = tags(i)
			for (i <- 0 until slen) grid(i)(6) = "_"
			for (i <- 0 until slen) grid(i)(7) = "_"
			for (i <- 0 until slen) grid(i)(8) = heads(i).toString
			for (i <- 0 until slen) grid(i)(9) = heads(i).toString
			for (i <- 0 until slen) grid(i)(10) = "DEPREL"
			for (i <- 0 until slen) grid(i)(11) = "PDEPREL"
			for (i <- 0 until slen) grid(i)(12) = if (preds(i) == "_") "_" else "Y"
			for (i <- 0 until slen) grid(i)(13) = preds(i)
//			for (i <- 0 until slen) grid(i)(14) = "_"
			
			var pcount = 1
			for (j <- pidxs) {
				for (i <- 0 until slen) {
//				val pidx = preds(j)
					grid(i)(13 + pcount) = args(i)(j)
				}
				pcount += 1
			}
			val datum = new SRLDatum(grid)
			return datum
			//		return new SRLDatum(preds, args, words, words, tags, tags, Array[Int](words.size))
		}


		def label(pidx: Int, aidx: Int, beliefs: Array[Potential]): String = {
			val labelPattern = """hasLabel\(([0-9]+),([0-9]+),(.+)\)""".r
			val lbeliefs = beliefs.filter(_.name.matches("hasLabel\\(%s,%s,(.+)\\)".format(pidx, aidx)))
			var maxv = lbeliefs.map(_.value).max
			val labels = lbeliefs.filter(_.value == maxv).map { b =>
				val labelPattern(pi, ai, l) = b.name; l
			}
			assert(labels.size > 0, "No labels found in SRL Decoding for %d, %d.".format(aidx, pidx))
			println("labels = " + labels.mkString(", "))
			labels.head
		}

		def sense(pidx: Int, lemma: String, beliefs: Array[Potential], dict: SRLDictionary, maxmode: String = "APPEND", maxsense: String = "01"): String = {
			try {
//				println("Pidx = " + pidx)
//				println("Lemma = " + lemma)
//				println("Beliefs:\n" + beliefs.mkString("\n"))

				val sensePattern = """sense\(([0-9]+),([0-9]+)\)""".r
				val lbeliefs = beliefs.filter(_.name.contains("sense(%d,".format(pidx)))
        println("LBELIEFS\n" + lbeliefs.mkString("\n"))
				var maxv = lbeliefs.map(_.value).max
				val sidxs = lbeliefs.filter(_.value == maxv).map { b =>
					val sensePattern(pi, sidx) = b.name; sidx.toInt
				}
        System.err.println("Best sidx = " + sidxs.mkString(", "))
				assert(sidxs.size > 0, "No sense label found in SRL Decoding for %d.".format(pidx))
				val sense = if (dict.containsSense(lemma)) {
					dict.senses(lemma)(sidxs.head)
				}
				else if (maxmode == "APPEND") {
					lemma + "." + maxsense
				}
				else {
					lemma
				}
				return sense				
			}
			catch {
				case e: Exception => {
					System.err.println("Error decoding sense <%s> of pixd %d: %s".format(lemma, pidx, e.getStackTrace().mkString("\n")))
					return lemma + "." + maxsense
				}
			}
		}
	}

//				dict.getOrElse(lemma, Array[String]()) ++ Array[String](lemma + "." + maxsense)
//			return senses(sidxs.first)
//		}
//	}
















	
/*		
	var pccount = 0.0
	var ppcount = 0.0
	var prcount = 0.0
	var account = 0.0
	var apcount = 0.0
	var arcount = 0.0
	var laccount = 0.0
	for (t <- ZippedReader.read(goldFile, testFile)) {
		val gdatum = SRLDatum.constructFromCoNLL(t._1.split("\n"))
		val tdatum = SRLDatum.constructFromCoNLL(t._2.split("\n"))
		val gpreds = gdatum.predicateIndices.toArray
		val tpreds = tdatum.predicateIndices.toArray

		pccount += gpreds.foldLeft(0.0){ case(csum, idx) => if (tpreds.contains(idx)) csum+1 else csum }
		prcount += gpreds.size
		ppcount += tpreds.size

		val gargs = gdatum.dependencies
		val targs = tdatum.dependencies
		println("SSIZE: " + gargs.size + " vs. " + targs.size)
		println(gdatum.tokens.mkString(", "))
		println(tdatum.tokens.mkString(", "))
		for (i <- 0 until gargs.size) {
			for (j <- 0 until gargs(i).size) {
				if (gargs(i)(j) != "_" && targs(i)(j) != "_") account += 1
				if (gargs(i)(j) != "_" && gargs(i)(j) == targs(i)(j)) laccount += 1
			}
			arcount += gargs(i).filter(_ != "_").size
			apcount += targs(i).filter(_ != "_").size
		}
	}

	val pp = if (ppcount == 0) 0 else pccount / ppcount
	val pr = if (pccount == 0) 0 else pccount / prcount
	val pf1 = 2 * ((pp * pr) / (pp + pr))
	println("Predicates:")
	println("Precision: (%f / %f) =  %f".format(pccount, ppcount, pp))
	println("Recall: (%f / %f) = %f".format(pccount, prcount, pr))
	println("F1: %f".format(pf1))

	val ap  = if (apcount == 0) 0 else account / apcount
	val ar  = if (arcount == 0) 0 else account / arcount
	val af1 = if ((ap + ar) == 0) 0 else 2 * ((ap * ar) / (ap + ar))
	println("Arguments:")
	println("Precision: (%f / %f) = %f".format(account, apcount, ap))
	println("Recall: (%f / %f) = %f".format(account, arcount, ar))
	println("F1: %f".format(af1))

	val lap  = if (apcount == 0) 0 else laccount / apcount
	val lar  = if (arcount == 0) 0 else laccount / arcount
	val laf1 = if ((lap + lar) == 0) 0 else 2 * ((lap * lar) / (lap + lar))
	println("Labeled Precision: (%f / %f) = %f".format(laccount, apcount, lap))
	println("Labeled Recall: (%f / %f) = %f".format(laccount, arcount, lar))
	println("Labeled F1: %f".format(laf1))

	println("Valency:")
	println("Avg valency %f (gold) vs. %f (test)")
	*/
	
/*
	val attrPattern = """@([^\t]+)\t *(.+)""".r
	val potPattern  = """(.*) (.+)""".r
	val pots = new ArrayBuffer[Potential]
	val attributes = new HashMap[String, String]
	for (line <- io.Source.fromFile(new File(filename), oformat).getLines) {
//			println(line)
		line match {		
			case attrPattern(attrName, attrStr) => {
				attributes(attrName.trim) = attrStr.trim
			}
			case potPattern(potName, potWeight) => {
//					println("POTS MATCH!")
				try {
					pots += new Potential(potWeight.toDouble, potName.trim, false)						
				}
				catch { case e: Exception => }
			}
			case "" => {
				if (attributes.contains("words")) {
					val words = attributes.getOrElse("words", "").split(" ")
					val tags  = attributes.getOrElse("tags", "").split(" ")
					val lemmas = attributes.getOrElse("lemmas", "").split(" ")
					val roles = attributes.getOrElse("roles", "").split(" ")
					val gpreds = attributes.getOrElse("gpreds", "").split(" ").map(_.toInt).filter(_>0)
					gpreds.foreach { p =>
						if (words.size > 0) pots += new Potential(1.0, "pred(%s)".format(p), false)														
					}
					data += constructFromBeliefs(pots.toArray, words, tags, lemmas, dict, maxsense)
					pots.clear
					attributes.clear
				}
			}
			case _ =>
		}			
	}
	if (attributes.contains("words")) { 
		val words = attributes.getOrElse("words", "").split(" ")
		val tags  = attributes.getOrElse("tags", "").split(" ")
		val lemmas = attributes.getOrElse("lemmas", "").split(" ")
		val roles = attributes.getOrElse("roles", "").split(" ")
		val gpreds = attributes.getOrElse("gpreds", "").split(" ").map(_.toInt).filter(_>0)
		gpreds.foreach { p =>
			if (words.size > 0) pots += new Potential(1.0, "pred(%s)".format(p), false)														
		}
		data += constructFromBeliefs(pots.toArray, words, tags, lemmas, dict, maxsense)
	}
	*/
	
	

	/*

val predPattern  = """pred\(([0-9]+)\)(.+)""".r
val sensePattern = """sense\(([0-9]+),(.+)\)(.+)""".r
val argPattern   = """hasArg\(([0-9]+),([0-9]+)\)(.+)""".r
val labelPattern = """hasLabel\(([0-9]+),([0-9]+),(.+)\)(.*)""".r
val unPattern = """un\(([0-9]+),([0-9]+)\)(.*)""".r
val wordsPattern = """@words\t *(.+)""".r
val lemmasPattern = """@lemma\t *(.+)""".r
val tagsPattern  = """@tags\t *(.+)""".r
val rolesPattern = """@roles\t *(.+)""".r
*/
/*
var words = Array[String]()
var lemmas = Array[String]()
var tags  = Array[String]()
var roles = Array[String]()
*/
//			case g if (g.contains("@gpreds")) {
	//				val gpreds = g.split("\t")(1).trim.split(" ").map(_.toInt).filter(_>0).foreach { p =>
		//					if (words.size > 0) pots += new Potential(1.0, "pred(%s)".format(p), false)					
		//				}
		//			}
		//		val gpredsPattern = """@gpreds\t *(.+)""".r

/*			
if (line.startsWith("@words")) {  words  = line.split("\t")(1).trim.split(" ") }
if (line.startsWith("@lemma")) { lemmas = line.split("\t")(1).trim.split(" ") }
if (line.startsWith("@tags")) {   tags   = line.split("\t")(1).trim.split(" ") }
if (line.startsWith("@roles")) {  roles  = line.split("\t")(1).trim.split(" ") }
line match {
//				case w if w.startsWith("@words") => {
//					words = w.substring(6).replaceAll("\t", "").split(" ").filter(_.size > 0)
//				}
//				case wordsPattern(w) => words = w.split(" ")
//				case lemmasPattern(l) => lemmas = l.split(" ")
//				case tagsPattern(t)  => tags = t.split(" ")
//				case rolesPattern(r) => roles = r.split(" ")
case predPattern(idx, weight) => { 
if (words.size > 0) pots += new Potential(weight.toDouble, "pred(%s)".format(idx), false)
}
//				case gpredsPattern(gpreds) => {  // A hacky solution for when no pred vars are explicit in the model
//					gpreds.split(" ").map(_.toInt).filter(_ > 0).foreach { p =>
//						if (words.size > 0) pots += new Potential(1.0, "pred(%s)".format(p), false)					
//					}
//				}
case sensePattern(idx, sense, weight) => { 
if (words.size > 0) pots += new Potential(weight.toDouble, "sense(%s,%s)".format(idx, sense), false)
}
case argPattern(aidx, pidx, weight) => { 
if (words.size > 0) pots += new Potential(weight.toDouble, "hasArg(%s,%s)".format(aidx, pidx), false)
}
case labelPattern(aidx, pidx, lidx, weight) => { 
if (words.size > 0) pots += new Potential(weight.toDouble, "hasLabel(%s,%s,%s)".format(aidx, pidx, roles(lidx.toInt)), false)
}
case unPattern(pidx, cidx, weight) => {
if (words.size > 0) pots += new Potential(weight.toDouble, "un(%s,%s)".format(pidx, cidx), false)
}
case "" => {
if (words.size > 0) {
data += constructFromBeliefs(pots.toArray, words, tags, lemmas, dict, maxsense)
pots.clear
words = Array[String]()
tags  = Array[String]()
roles = Array[String]()
lemmas = Array[String]()
//						println(datum.toString)
//						println
}
}
case _ => 
}
}
*/

