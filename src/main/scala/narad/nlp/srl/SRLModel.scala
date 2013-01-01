package narad.nlp.srl
import narad.bp.structure._
import narad.bp.inference._
import narad.bp.util.PotentialExample
import scala.collection.mutable.HashSet
import scala.util.matching.Regex
import scala.math._

class SRLModel(params: SRLParams) extends FactorGraphModel with SRLFeatures with BeliefPropagation {
  val SENSE_PATTERN  = """sense\(([0-9]+),([0-9]+)\)""".r
  val ARG_PATTERN    = """hasArg\(([0-9]+),([0-9]+)\)""".r
  val LABEL_PATTERN  = """hasLabel\(([0-9]+),([0-9]+),(.+)\)""".r
  val SYNTAX_PATTERN = """un\(([0-9]+),([0-9]+)\)""".r
  val CONNECT_PATTERN = """sslink\(([0-9]+),([0-9]+)\)""".r

  def constructFromExample(ex: PotentialExample, pv: Array[Double]): ModelInstance = {
    val slen = ex.attributes.getOrElse("slen", "-1").toInt
    val maxDist = slen+1
    val pots = ex.exponentiated(pv)
/*
    for (i <- 0 until pots.size) {
      if (pots(i).name.startsWith("un")) {
        if (pots(i).isCorrect) {
          pots(i) = new Potential(scala.math.exp(5), pots(i).name, true)
        }
        else {
          pots(i) = new Potential(scala.math.exp(5), pots(i).name, false)
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
    new SRLModelInstance(fg.toFactorGraph, ex)
  }

  def label(aidx: Int, pidx: Int, beliefs: Array[Potential]): String = {
    val lbeliefs = beliefs.filter(_.name.matches("labelOf\\(%s,%s,(.+)\\)".format(aidx, pidx)))
    var maxv = lbeliefs.map(_.value).max
    val labels = lbeliefs.filter(_.value == maxv).map { b =>
      val LABEL_PATTERN(ai, pi, l) = b.name; l
    }
    assert(labels.size > 0, "No labels found in SRL Decoding for %d, %d.".format(aidx, pidx))
    labels.first
  }

  def decode(instance: ModelInstance) = {

  }

  def options = params

}

class SRLModelInstance(fg: FactorGraph, ex: PotentialExample) extends ModelInstance(fg, ex)














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