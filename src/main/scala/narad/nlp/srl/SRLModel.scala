package narad.nlp.srl
import narad.bp.structure._
import narad.bp.inference._
import narad.bp.util.{GZipWriter, PotentialExample}
import narad.bp.util.index.Index
import narad.io.srl.SRLReader
import narad.nlp.parser.dependency.DependencyParserPrediction

class SRLModel(dict: SRLDictionary, params: SRLParams) extends FactorGraphModel[SRLDatum] with BeliefPropagation
                             with SRLFeatures with SRLPrediction with SRLDecoding with DependencyParserPrediction {

  def extractFeatures(trainFile: String, trainFeatureFile: String, dict: SRLDictionary,
                      index: Index[String], params: SRLParams) = {
    val in = trainFile
    val out = new GZipWriter(trainFeatureFile + ".gz")
    val reader = new SRLReader(in)
    var startTime = System.currentTimeMillis()
    val maxDist = params.MAX_DIST
    reader.zipWithIndex.grouped(params.BATCH_SIZE).foreach { batch =>
      val batchArray = batch.toArray
      val pexs = new Array[PotentialExample](batchArray.size)
      batchArray.par.map { case(datum, i) =>
        if (i % params.PRINT_INTERVAL == 0) System.err.print("\r  example %d...[index contains %d elements].".format(i, index.size))
        val slen = datum.slen
        val gpreds = datum.predicates
        val ex = new PotentialExample
        ex.attributes("slen") = slen.toString
        ex.attributes("maxdist") = maxDist.toString
        ex.attributes("roles") = dict.roles.mkString(" ") + " " + params.PRUNED_ROLE_LABEL
        ex.attributes("model") = params.MODEL
        ex.attributes("gpreds") = gpreds.mkString(" ")
        ex.attributes("words") = datum.words.mkString(" ")
        ex.attributes("tags") = datum.postags.mkString(" ")
        ex.attributes("lemmas") = datum.lemmas.mkString(" ")
        ex.attributes("role-valency") = params.MODEL_ROLE_VALENCY.toString()
        ex.attributes("arg-valency") = params.MODEL_ARG_VALENCY.toString()

        val ex1 = extractSRLFeatures(datum, dict, index, params=params)
        var ex2 = new PotentialExample()
        var ex3 = new PotentialExample()
        if (params.MODEL != "BASELINE") {
          ex2 = extractSyntacticFeatures(datum, index, params)
          ex3 = extractConnectionFeatures(datum, dict, index, gpreds, maxDist, params)
        }
        ex.features ++= ex1.features ++ ex2.features ++ ex3.features
        ex.potentials ++= ex1.potentials ++ ex2.potentials ++ ex3.potentials
        pexs(i % params.BATCH_SIZE) = ex
      }
      pexs.foreach { pex =>
        pex.writeToFile(out)
        out.write("\n")
      }
    }
    out.close()
  }

  def constructFromExample(ex: PotentialExample, pv: Array[Double]): ModelInstance = {
    val slen = ex.attributes.getOrElse("slen", "-1")
    val pots = ex.exponentiated(pv)
    val fg = new FactorGraphBuilder(pots)
    addSRLPrediction(fg, pots, slen, params)
    if (params.MODEL == "ORACLE") {
      addDependencySyntaxPrediction(fg, pots, slen, observedSyntax=true)
      addConnectionPrediction(fg, pots)
      val mi = new SRLModelInstance(fg.toFactorGraph, ex)
      mi.graph.factors.foreach { f => if (f.name.startsWith("linkFac")) f.clamp() }
      mi
    }
    else if (params.MODEL == "JOINT") {
      addDependencySyntaxPrediction(fg, pots, slen, observedSyntax=false)
      addConnectionPrediction(fg, pots)
      new SRLModelInstance(fg.toFactorGraph, ex)
    }
    else if (params.MODEL == "HIDDEN") {
      addDependencySyntaxPrediction(fg, pots, slen, observedSyntax=false)
      addConnectionPrediction(fg, pots)
      new SRLHiddenModelInstance(fg.toFactorGraph, ex)
    }
    else {   // Baseline
      new SRLModelInstance(fg.toFactorGraph, ex)
    }
  }

  def decode(instance: ModelInstance): SRLDatum = {
    decode(instance, dict, params)
  }

  def options = params

}





















//System.err.println("Adding syntax %d,%d".format(hidx, kidx))
//          graph.addVariable("linkvar(%d,%d)".format(hidx, kidx), 2)
//          graph.addUnaryFactor("linkvar(%d,%d)".format(hidx, kidx), "linkfac(%d,%d)".format(hidx, kidx), pot)



/*    if (params.MODEL == "ORACLE") {
  pots.foreach { p =>
    if (p.name.startsWith("un")) {
      if (p.isCorrect) p.value = 1.0 else p.value = 0.0
    }
  }
}
*/


/*
def constructFromBeliefs(beliefs: Array[Potential], words: Array[String], tags: Array[String],
                         lemmas: Array[String], roles: Array[String], dict: SRLDictionary,
                         maxmode: String = "APPEND", maxsense: String = "01", threshold: Double = 0.5): SRLDatum = {
  val slen = words.size
  val heads = new Array[Int](slen)
  val preds = new Array[String](slen)
  val args = Array.ofDim[String](slen+1, slen+1)

  for (i <- 0 until slen) preds(i) = "_"
  for (i <- 0 to slen; j <- 0 to slen) args(i)(j) = "_"

  val groups = beliefs.groupBy { b =>
    b.name match {
      case SENSE_PATTERN(pidx, sidx) => ("SENSE", pidx.toInt, -1)
      case ARG_PATTERN(pidx, aidx) => ("ARG", pidx.toInt, aidx.toInt)
      case LABEL_PATTERN(pidx, aidx, label) => ("LABEL", pidx.toInt, aidx.toInt)
      case _=> -1
    }
  }

  for (i <- 0 to slen if groups.contains("SENSE", i, -1)) {
    println("lemma = " + lemmas(i-1))
    val senses = dict.senses(lemmas(i-1))
    if (senses.isEmpty) {
      preds(i-1) = lemmas(i-1) + ".1"
    }
    else {
      val SENSE_PATTERN(pidx, sidx) = groups("SENSE", i, -1).maxBy(_.value).name
      preds(i-1) = senses(sidx.toInt)
    }
    for (j <- 0 to slen if groups.contains("ARG", i, j)) {
      val agroup = groups("ARG", i, j)
      if (agroup.size > 0 && agroup(0).value > threshold) {
        println("labels: " + groups("LABEL", i, j).mkString(" "))
        println("roles: " + roles.mkString(" "))
        val LABEL_PATTERN(lp, li, ll) = groups("LABEL", i, j).maxBy(_.value).name
        println("lidx = " + ll.toInt)
        println("slen = " + slen)
        args(i)(j) = roles(ll.toInt)
      }
    }
  }

  // ID FORM LEMMA PLEMMA POS PPOS FEAT PFEAT HEAD PHEAD DEPREL PDEPREL FILLPRED PRED APREDs
  val pidxs = preds.zipWithIndex.filter(p => p._1 != "_").map(p => p._2 + 1)
  println("pidxs = " + pidxs.mkString(" "))
  for (i <- 0 until slen; j <- 0 until slen) {
    println("aidx " + i + " " + j + " = " + args(i)(j))
  }

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
  var pcount = 1
  for (i <- pidxs) {
    for (j <- 1 to slen) {
      grid(j-1)(13 + pcount) = args(i)(j)
    }
    pcount += 1
  }

  val datum = new SRLDatum(grid)
/*
  for (f <- datum.frames) {
    println(f)
  }
  println("9 1? " + datum.hasArg(9, 1))
  println("10 5? " + datum.hasArg(10, 5))
  println("10 1? " + datum.hasArg(10, 1))
  println("10 1 A0? " + datum.hasArgLabel(10, 1, "A0"))
  println("10 1 A1? " + datum.hasArgLabel(10, 1, "A1"))
  println("role 10 1 = " + datum.getLabel(10, 1))
  println("role 10 5 = " + datum.getLabel(10, 5))
  println("10 14? " + datum.hasArg(10, 14))
  */
  return datum
  //		return new SRLDatum(preds, args, words, words, tags, tags, Array[Int](words.size))
}
*/












/*
    reader.zipWithIndex.foreach { case(datum, i) =>
      if (datum.predicates.size > 0) {
        if (i % params.PRINT_INTERVAL == 0) System.err.print("\r  example %d...".format(i))
        val slen = datum.slen
        val gpreds = datum.predicates
        val ex = new PotentialExample
        ex.attributes("slen") = slen.toString
        ex.attributes("maxdist") = 1000.toString
        ex.attributes("roles") = dict.roles.mkString(" ") + " " + params.PRUNED_ROLE_LABEL
        ex.attributes("model") = params.MODEL
        ex.attributes("gpreds") = "0 " + gpreds.mkString(" ")
        ex.attributes("words") = datum.words.mkString(" ")
        ex.attributes("tags") = datum.postags.mkString(" ")
        ex.attributes("lemmas") = datum.lemmas.mkString(" ")

        val ex1 = extractSRLFeatures(datum, dict, index, labelCorrect=true, prune=false, maxdist=1000, srlmode=1)
        var ex2 = new PotentialExample()
        var ex3 = new PotentialExample()
        if (params.MODEL != "BASELINE") {
          ex2 = extractSyntacticFeatures(datum, index, labelHidden=params.MODEL=="ORACLE", skip=params.MODEL=="ORACLE")
          ex3 = extractConnectionFeatures(datum, index, labelHidden=params.MODEL=="ORACLE", gpreds=gpreds, abound=1000)
        }
        ex.features ++= ex1.features ++ ex2.features ++ ex3.features
        ex.potentials ++= ex1.potentials ++ ex2.potentials ++ ex3.potentials
        ex.writeToFile(out)
        out.write("\n")
      }
    }
 */


/*

class BaselineSRL(params: SRLParams) extends SRLModel(params) {

  override   def extractFeatures(trainFile: String, trainFeatureFile: String, dict: SRLDictionary,
                                 index: Index[String], markCorrect: Boolean, params: SRLParams) = {
    val in = trainFile
    val out = new FileWriter(trainFeatureFile)
    val rout = new FileWriter(trainFeatureFile + ".bpdp")
    val reader = new SRLReader(in)
    var startTime = System.currentTimeMillis()
    reader.zipWithIndex.foreach { case(datum, i) =>
      if (datum.predicates.size > 0) {
        if (i % params.PRINT_INTERVAL == 0) System.err.print("\r  example %d...".format(i))
        val slen = datum.slen
        val gpreds = datum.predicates
        out.write("@slen\t%d\n".format(slen))
        out.write("@maxdist\t%d\n".format(1000))
        out.write("@roles\t%s\n".format(dict.roles.mkString(" ") + " A-DUMMY"))
        out.write("@mode\tBASELINE\n")
        out.write("@gpreds\t0 %s\n".format(gpreds.mkString(" ")))
        out.write("@words\t%s\n".format(datum.words.mkString(" ")))
        out.write("@tags\t%s\n".format(datum.postags.mkString(" ")))
        out.write("@lemmas\t%s\n".format(datum.lemmas.mkString(" ")))

        extractSRLFeatures(datum, dict, out, labelCorrect=true, prune=false, maxdist=1000, srlmode=1)
        out.write("\n")
        rout.write("\n")
      }
    }
    println
    out.close()
    rout.close()
    if (params.TIME) System.err.println("Finished Feature Extraction [%fs.]".format((System.currentTimeMillis() - startTime) / 1000.0))
  }
}

class OracleSRL(params: SRLParams) extends SRLModel(params) {
  override   def extractFeatures(trainFile: String, trainFeatureFile: String, dict: SRLDictionary,
                                 index: Index[String], markCorrect: Boolean, params: SRLParams) = {
    val in = trainFile
    val out = new FileWriter(trainFeatureFile)
    val rout = new FileWriter(trainFeatureFile + ".bpdp")
    val reader = new SRLReader(in)
    var startTime = System.currentTimeMillis()
    reader.zipWithIndex.foreach { case(datum, i) =>
      if (datum.predicates.size > 0) {
        if (i % params.PRINT_INTERVAL == 0) System.err.println("  example %d...".format(i))
        val slen = datum.slen
        val gpreds = datum.predicates
        out.write("@slen\t%d\n".format(slen))
        out.write("@maxdist\t%d\n".format(1000))
        out.write("@roles\t%s\n".format(dict.roles.mkString(" ") + " A-DUMMY"))
        out.write("@gpreds\t0 %s\n".format(gpreds.mkString(" ")))
        out.write("@mode\tORACLE\n")
        out.write("@words\t%s\n".format(datum.words.mkString(" ")))
        out.write("@tags\t%s\n".format(datum.postags.mkString(" ")))
        out.write("@lemmas\t%s\n".format(datum.lemmas.mkString(" ")))

        extractSRLFeatures(datum, dict, out, labelCorrect=true, prune=false, maxdist=1000, srlmode=1)

        extractSyntacticFeatures(datum, out, labelHidden=true, mode=0)

        extractConnectionFeatures(datum, out, labelHidden=true, gpreds=gpreds, abound=1000)

        out.write("\n")
        rout.write("\n")
      }
    }
    out.close()
    rout.close()
    if (params.TIME) System.err.println("Finished Feature Extraction [%fs.]".format((System.currentTimeMillis() - startTime) / 1000.0))
  }
}


class HiddenSyntaxSRL(params: SRLParams) extends SRLModel(params)  {
  override   def extractFeatures(trainFile: String, trainFeatureFile: String, dict: SRLDictionary,
                                 index: Index[String], markCorrect: Boolean, params: SRLParams) = {
    val in = trainFile
    val out = new FileWriter(trainFeatureFile)
    val rout = new FileWriter(trainFeatureFile + ".bpdp")
    val reader = new SRLReader(in)
    var startTime = System.currentTimeMillis()
    reader.zipWithIndex.foreach { case(datum, i) =>
      if (datum.predicates.size > 0) {
        if (i % params.PRINT_INTERVAL == 0) System.err.println("  example %d...".format(i))
        val ex =



        val slen = datum.slen
        val gpreds = datum.predicates
        out.write("@slen\t%d\n".format(slen))
        out.write("@maxdist\t%d\n".format(1000))
        out.write("@roles\t%s\n".format(dict.roles.mkString(" ") + " A-DUMMY"))
        out.write("@gpreds\t0 %s\n".format(gpreds.mkString(" ")))
        out.write("@mode\tHIDDEN\n")
        out.write("@words\t%s\n".format(datum.words.mkString(" ")))
        out.write("@tags\t%s\n".format(datum.postags.mkString(" ")))
        out.write("@lemmas\t%s\n".format(datum.lemmas.mkString(" ")))

        extractSRLFeatures(datum, dict, out, labelCorrect=true, prune=false, maxdist=1000, srlmode=1)

        extractSyntacticFeatures(datum, out, labelHidden=false, mode=1)

        extractConnectionFeatures(datum, out, labelHidden=false, gpreds=gpreds, abound=1000)

        out.write("\n")
        rout.write("\n")
      }
    }
    out.close()
    rout.close()
    if (params.TIME) System.err.println("Finished Feature Extraction [%fs.]".format((System.currentTimeMillis() - startTime) / 1000.0))
  }

  override def observedVariableFactors(factors: Array[Factor]): Array[Factor] = {
    factors.filter { f =>
      !f.name.startsWith("linkfac") && !f.name.startsWith("sslink")
    }
  }
}

*/


























/*
   if (valency) {
   for (pidx <- pidxs) {
     for (aidx <- 1 to slen) {
       for (ridx <- 0 to numRoles) {
         val offpots = Array[Potential](new Potential(1.0, "null", false),
           new Potential(0.0, "null", false))
         graph.addVariable("%s(%d,%d,%d)".format("C", pidx, aidx, ridx), arity=2)
         graph.addTable1Factor("%s(%d,%d,%d)".format("C", pidx, aidx, ridx),
           "%s(%d,%d,%d)".format("c", pidx, aidx, ridx), offpots)

       }
     }
   }
 }
*/

/*
      for (pidx <- pidxs; aidx <- 1 to slen) {
        for (ridx <- roles) {
          // Add arity "C" variables and their unary table factors
          val offpots = Array[Potential](new Potential(1.0, "null", false),
                                          new Potential(0.0, "null", false))
          graph.addVariable("%s(%d,%d,%d)".format("C", pidx, aidx, ridx), arity=2)
          graph.addTable1Factor("%s(%d,%d,%d)".format("C", pidx, aidx, ridx),
                                "%s(%d,%d,%d)".format("c", pidx, aidx, ridx), offpots)
          if (aidx > 1) {
            // create pots for table3 transition factor
            val dim1 = 2
            val dim2 = 2
            val dim3 = roles.size
            val trans = Array.ofDim[Potential](dim1, dim2, dim3)
            val r = 1  // In a multinomial configuration the ridx is the idx for the value, here 1 is the index for "on"
            for (i <- 0 until dim1; j <- 0 until dim2; k <- 0 until dim3) {
              trans(i)(j)(k) = new Potential(0.0, "null", false)
              if (i == j && k != r) {
                trans(i)(j)(k) = new Potential(1.0, "null", false)
              }
            }
            trans(0)(1)(r) = new Potential(1.0, "null", false)

            // connect factor to label and adjacent arity chain variables
            graph.addTable3FactorByMatrix("%s(%d,%d)".format("C", pidx, aidx),
              "%s(%d,%d)".format("C", pidx, aidx-1),
              "%s(%d,%d,%d)".format("labelVar", pidx, aidx-1, ridx),
              "trans-factor(%d,%d)".format(pidx, aidx), trans)
          }
        }
      }
    }
  }
  */



/*
    def constructFromExample(ex: PotentialExample, pv: Array[Double]): ModelInstance = {
    val slen = ex.attributes.getOrElse("slen", "-1").toInt
    val maxDist = slen+1
    val pots = ex.exponentiated(pv)

    /*
    for (i <- 0 until pots.size) {
      if (pots(i).name.startsWith("un")) {
        if (pots(i).isCorrect) {
          pots(i) = new Potential(10000, pots(i).name, true)       // This is awful!
        }
        else {
          pots(i) = new Potential(0, pots(i).name, false)
        }
      }
    }
    */

    val fg = new FactorGraphBuilder(pots)
    val pidxs = new HashSet[Int]
    for (i <- 0 until pots.size) {
      pots(i).name match {
        case SENSE_PATTERN(spidx, ssidx) => {
          val pidx = spidx.toInt
          val sidx = ssidx.toInt
          fg.addVariable("senseVar(%d,%d)".format(pidx, sidx), 2)
          fg.addUnaryFactor("senseVar(%d,%d)".format(pidx, sidx), "senseFac(%d,%d)".format(pidx, sidx), pots(i))
          pidxs += pidx
        }
        case ARG_PATTERN(spidx, saidx) => {
          val pidx = spidx.toInt
          val aidx = saidx.toInt
          System.err.println("Adding arg %d,%d".format(pidx, aidx))
          fg.addVariable("argVar(%d,%d)".format(pidx, aidx), 2)
          fg.addUnaryFactor("argVar(%d,%d)".format(pidx, aidx), "argFac(%d,%d)".format(pidx, aidx), pots(i))
        }
        case LABEL_PATTERN(spidx, saidx, slidx) => {
          val pidx = spidx.toInt
          val aidx = saidx.toInt
          val lidx = slidx.toInt
          fg.addVariable("labelVar(%d,%d,%d)".format(pidx, aidx, lidx), 2)
          fg.addUnaryFactor("labelVar(%d,%d,%d)".format(pidx, aidx, lidx), "labelFac(%d,%d,%d)".format(pidx, aidx, lidx), pots(i))
        }
        case SYNTAX_PATTERN(shidx, skidx) => {
          val hidx = shidx.toInt
          val kidx = skidx.toInt
          System.err.println("Adding syntax %d,%d".format(hidx, kidx))
          fg.addVariable("linkvar(%d,%d)".format(hidx, kidx), 2)
          fg.addUnaryFactor("linkvar(%d,%d)".format(hidx, kidx), "linkfac(%d,%d)".format(hidx, kidx), pots(i))
        }
        case CONNECT_PATTERN(shidx, skidx) => {
          val hidx = shidx.toInt
          val kidx = skidx.toInt
          fg.addNandFactor(new Regex("argVar\\(%d,%d\\)".format(hidx, kidx)), new Regex("linkvar\\(%d,%d\\)".format(hidx, kidx)), "sslink(%d,%d)".format(hidx, kidx), pots(i))
        }
        case _=> {
          System.err.println("Line not match: %s".format(pots(i).name))
        }
      }
    }
    for (pidx <- pidxs) {
      fg.addAtMost1Factor(new Regex("senseVar\\(%d,.+\\)".format(pidx)), "senseAtMost(%d)".format(pidx))
      for (aidx <- 1 to slen if abs(pidx-aidx) <= maxDist) {
        fg.addIsAtMost1Factor(new Regex("argVar\\(%d,%d\\)".format(pidx, aidx)), new Regex("labelVar\\(%d,%d,.+\\)".format(pidx, aidx)), "labelAtMost(%d,%d)".format(pidx, aidx))
      }
    }
    fg.addProjectiveTreeFactor(new Regex("linkvar\\("), "PTREE", slen)
    new SRLModelInstance(fg.toFactorGraph, ex)
  }
  */


/*
package narad.nlp.srl
import narad.bp.structure._
import scala.collection.mutable.{ArrayBuffer, HashSet}
import scala.util.matching._


class SRLModel(var graph: FactorGraph) extends FactorGraphModel {
	val predPattern  = pred\(([0-9]+)\)""".r
	val argPattern   = """argOf\(([0-9]+),([0-9]+)\)""".r
	val labelPattern = """labelOf\(([0-9]+),([0-9]+),(.+)\)""".r

	def label(aidx: Int, pidx: Int, beliefs: Array[Potential]): String = {
		val lbeliefs = beliefs.filter(_.name.matches("labelOf\\(%s,%s,(.+)\\)".format(aidx, pidx)))
		var maxv = lbeliefs.map(_.value).max
		val labels = lbeliefs.filter(_.value == maxv).map { b =>
			val labelPattern(ai, pi, l) = b.name; l
		}
		assert(labels.size > 0, "No labels found in SRL Decoding for %d, %d.".format(aidx, pidx))
		labels.first
	}

	def decode(words: Array[String], tags: Array[String]) = {
		val beliefs = graph.potentialBeliefs
		for (b <- beliefs) {
			println("%s %s".format(b.name, b.value.toString))			
		}
		//		val datum = SRLEval.constructFromBeliefs(beliefs, words, tags)
		//		println(datum)
		println
	}

	override def toString = graph.toString
}



object SRLModel {
	val sensePattern  = """sense\(([0-9]+),([0-9]+)\)""".r
	val argPattern    = """hasArg\(([0-9]+),([0-9]+)\)""".r
	val labelPattern  = """hasLabel\(([0-9]+),([0-9]+),(.+)\)""".r

	def construct(pots: Array[Potential], slen: Int, maxDist: Int = 1000, syntax: Boolean = false): SRLModel = {
			val fg = new FactorGraphBuilder(pots)
			val pidxs = new HashSet[Int]
			for (i <- 0 until pots.size) {
				pots(i).name match {
					case sensePattern(spidx, ssidx) => {
						val pidx = spidx.toInt
						val sidx = ssidx.toInt
						fg.addVariable("senseVar(%d,%d)".format(pidx, sidx), 2)
						fg.addUnaryFactor("senseVar(%d,%d)".format(pidx, sidx), "senseFac(%d,%d)".format(pidx, sidx), Array(pots(i)))				
						pidxs += pidx
					}
					case argPattern(spidx, saidx) => {
						val pidx = spidx.toInt
						val aidx = saidx.toInt
						fg.addVariable("argVar(%d,%d)".format(pidx, aidx), 2)
						fg.addUnaryFactor("argVar(%d,%d)".format(pidx, aidx), "argFac(%d,%d)".format(pidx, aidx), Array(pots(i)))				
					}
					case labelPattern(spidx, saidx, slidx) => {
						val pidx = spidx.toInt
						val aidx = saidx.toInt
						val lidx = slidx.toInt
						fg.addVariable("labelVar(%d,%d,%d)".format(pidx, aidx, lidx), 2)
						fg.addUnaryFactor("labelVar(%d,%d,%d)".format(pidx, aidx, lidx), "labelFac(%d,%d,%d)".format(pidx, aidx, lidx), Array(pots(i)))										
					}
					case _=> {
						System.err.println("Line not match: %s".format(pots(i).name))
					}
				}
			}
			for (pidx <- pidxs) {
				fg.addAtMost1Factor(new Regex("senseVar\\(%d,.+\\)".format(pidx)), "senseAtMost(%d)".format(pidx))				
				for (aidx <- 1 to slen if Math.abs(pidx-aidx) <= maxDist) {
//					System.err.println("Creating IsAtMost for %d vs %d".format(pidx, aidx))
					fg.addIsAtMost1Factor(new Regex("argVar\\(%d,%d\\)".format(pidx, aidx)), new Regex("labelVar\\(%d,%d,.+\\)".format(pidx, aidx)), "labelAtMost(%d,%d)".format(pidx, aidx))													
				}
			}
			new SRLModel(fg.toFactorGraph)
		}
		
//		def construct(pots: Array[Potential], slen: Int, maxDist: Int = 1000, syntax: Boolean = false): SRLModel = {
//				val fg = new FactorGraphBuilder(pots)
//				val pidxs = new HashSet[Int]	
//		}
}


/*
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


*/
/*
	def construct(pots: Array[Potential], slen: Int, syntax: Boolean = false): SRLModel = {
		val fg = new FactorGraphBuilder(pots)
		val groups = pots.filter(_.name.contains("sense")).groupBy{pot => 
			val sensePattern(pidx, sidx) = pot.name
			sidx
		}
		for (g <- groups) {
			val pidx = g._1
			fg.addVariable("senseVar(%s,%s)".format(pidx, sidx), 2)
			fg.addUnaryFactor("senseVar(%s,%s)".format(pidx, sidx), "senseFac(%s,%s)".format(pidx, sidx), Array(pot))																
			fg.addAtMost1Factor(new Regex("senseVar\\(%s,.*\\)".format(pidx)), "senseAtMost(%s)".format(pidx))			
			for (aidx <- 1 to slen) {
				fg.addVariable("argVar(%s,%s)".format(pidx, aidx), 2)
				fg.addUnaryFactor("argVar(%s,%s)".format(pidx, aidx), "argFac(%s,%s)".format(pidx, sidx), Array(pot))																

			}
		}
		new SRLModel(fg.toFactorGraph)
	}
}
*/

/*
fg.addVariable("predVar(%s)".format(pidx), 2)
for (pot <- g._2) {
val sensePattern(pidx, sidx) = pot.name
fg.addVariable("senseVar(%s,%s)".format(pidx, sidx), 2)
fg.addUnaryFactor("senseVar(%s,%s)".format(pidx, sidx), "senseFac(%s,%s)".format(pidx, sidx), Array(pot))																
}
*/




















/*
for (pidx <- 0 until pots.size) {
val sensePattern = new Regex("sense\\(([0-9]+,%d)\\)".format(pidx))
pots(pidx).name match {
case sensePattern(sidx) => {
println(pots(pidx).name)
}
case _=> 
}
}
*/
/*
for (pi <- 0 until pots.size) {
pots(pi).name match {
case predPattern(start) => {
val i = start.toInt
fg.addVariable("predVar(%d)".format(i), 2)
fg.addUnaryFactor2("predVar(%d)".format(i), "predFac(%d)".format(i), Array(pots(pi)))											
}
case argPattern(start, end) => {
val i = start.toInt
val j = end.toInt
fg.addVariable("argVar(%d,%d)".format(i, j), 2)
fg.addUnaryFactor2("argVar(%d,%d)".format(i, j), "argFac(%d,%d)".format(i, j), Array(pots(pi)))																
}
case labelPattern(start, end, l) => {
val i = start.toInt
val j = end.toInt
fg.addVariable("labelVar(%d,%d,%s)".format(i, j, l), 2)
fg.addUnaryFactor2("labelVar(%d,%d,%s)".format(i, j, l), "labelFac(%d,%d,%s)".format(i, j, l), Array(pots(pi)))																
}
}
}
for (i <- 0 until slen; j <- 0 until slen) {
fg.addIsAtMost1Factor("argVar\\(%d,%d\\)".format(i, j), "labelVar\\(%d,%d,.*\\)".format(i, j), "isAtMost(%d,%d)".format(i, j))			
}
for (i <- 0 until slen; j <- 0 until slen) {
fg.addYouShallNotPassFactor("predVar\\(%d\\)".format(j), "argVar\\(%d,%d\\)".format(i, j), "YSNP(%d,%d)".format(i, j))			
}
if (syntax) {
// Augment the graph with syntactic links
for (dep <- 1 to slen; head <- 0 to slen if dep != head) {
fg.addVariable("%s(%d,%d)".format("linkvar", head, dep), 2)
fg.addUnaryFactor("link\\(%d,%d\\)".format(head, dep), "linkvar(%d,%d)".format(head, dep), "linkfac(%d,%d)".format(head, dep))				
// bpdp code kept a matrix for link vars, would have links[dep][head] = the variable
//			fg.addNandFactor
}			
fg.addProjectiveTreeFactor("linkvar(", "PTREE", slen)
}
*/

*/