package narad.projects.relmarg
import narad.bp.structure._
import narad.projects.bpdp._
import scala.collection.mutable.ArrayBuffer
import scala.util.matching._


class SRLModel(var graph: FactorGraph) extends Model {
	val predPattern  = """pred\(([0-9]+)\)""".r
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
//		val datum = SRLDatum.constructFromBeliefs(beliefs, words, tags)
//		println(datum)
		println
	}
	
	override def toString = graph.toString
}



object SRLModel {
	val predPattern  = """pred\(([0-9]+)\)""".r
	val argPattern   = """argOf\(([0-9]+),([0-9]+)\)""".r
	val labelPattern = """labelOf\(([0-9]+),([0-9]+),(.+)\)""".r
	
	def construct(pots: Array[Potential], slen: Int, syntax: Boolean = false): SRLModel = {
		val fg = new FactorGraphBuilder(pots)
		System.err.println("SRL Model currently not implement!")
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
		new SRLModel(fg.toFactorGraph)
	}
}
