package narad.nlp.parse

import narad.bp.structure._
import narad.bp.train._
import narad.bp.util._
import narad.projects.bpdp._
import narad.projects.cparser._
import scala.collection.mutable.ArrayBuffer
import scala.util.matching._

class DependencyParser(var graph: FactorGraph) extends Model {
	val indicesPattern = new Regex(".*\\(([0-9]+)\\,([0-9]+)\\).*")

	def decode(words: Array[String]) = {
/*
		val beliefs = graph.potentialBeliefs.sortBy{ b =>
			val indicesPattern(start, end) = b._1
			start.toInt * 10000 + end.toInt
		}
		println("Decoding!")
		for (belief <- beliefs) {
			println(belief)
			if (belief._2 > 0.5) {
				println(" ===> " + belief._1)
			}
		}
*/
		val vars = graph.variables.toArray.sortBy { v =>
				val indicesPattern(start, end) = v.name
				start.toInt * 10000 + end.toInt
		}			
		println("DECODING")
		for (v <- vars) {
			println(v.name + "\t" + v.logOdds(graph))
		}
		
	}

	override def toString = graph.toString
}

object DependencyParser {

	def construct(pots: Array[Potential], slen: Int): DependencyParser = {
		val fg = new FactorGraphBuilder(pots)
		System.err.println("ERROR - PARSER CODE HAS BEEN COMMENTED OUT!")
		for (dep <- 1 to slen; head <- 0 to slen if dep != head) {
//			fg.addVariable("%s(%d,%d)".format("linkvar", head, dep), 2)
//			fg.addUnaryFactor("link\\(%d,%d\\)".format(head, dep), "linkvar(%d,%d)".format(head, dep), "linkfac(%d,%d)".format(head, dep))				
			// bpdp code kept a matrix for link vars, would have links[dep][head] = the variable
		}
//	 	fg.addProjectiveTreeFactor("linkvar(", "PTREE", slen)
		new DependencyParser(fg.toFactorGraph)
	}
}