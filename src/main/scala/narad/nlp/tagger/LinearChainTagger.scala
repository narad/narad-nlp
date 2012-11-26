package narad.projects.bpdp

import narad.bp.structure._
//import narad.bp.train._
import narad.bp.util._
import scala.util.matching._

/*
class LinearChainTagger(scoredpots: Array[(Potential, Double)], words: Array[String], tagset: Array[String]) {
	val potPattern = """pos\([0-9]+,([0-9]+)\)""".r
	val fg = new FactorGraphBuilder(scoredpots)
	
	val vars = (0 until words.size).toList.map("posvar%d".format(_)).toArray
	println("slen = " + words.size)
	println("tags = " + tagset.size)
//	println(pots.map(_.name).mkString(", "))
	fg.addVariables(vars, tagset.size)
	val slen = words.size
	for (i <- 0 until slen) {
		fg.addNamedFactor("pos\\(%d,[0-9]+\\)".format(i), "posvar%d".format(i))
		if (i < slen-1) fg.addNamed2Factor(new Regex("bpos\\(%d,([0-9]+),([0-9]+)\\)".format(i)), "posvar%d".format(i), "posvar%d".format(i+1))
	}
	val tagger = fg.toFactorGraph
	println(tagger)
	
	def graph: FactorGraph = tagger
	
	def potentialBeliefs: Array[(Potential, Double)] = {
		val buf = new collection.mutable.ArrayBuffer[(Potential, Double)]
		for (factor <- graph.factors) {
			println("computing beliefs, looking at factor %s".format(factor))
			val beliefs = factor.getBeliefs(graph)
			println("BELIEFS == " + beliefs.size)
			buf ++= beliefs //factor.getBeliefs(graph)
		}
		buf.toArray
//		graph.factors.map(_.potentials(fg)).toArray.flatten
	}
	
	def decode = {
		val beliefs = potentialBeliefs
		for (i <- 0 until words.size) {
			var mtag = ""
			var mscore = -10000.0
			for (pot <- beliefs.filter(_._1.name.startsWith("pos(%d,".format(i)))) {
				println(pot._1.name +":\t" + pot._2)
				if (pot._2 > mscore) {
					mscore = pot._2
					mtag = pot._1.name
				}
			}
			val potPattern(tidx) = mtag
			println("%s\t%s".format(words(i), tagset(tidx.toInt)))
		}
	}
}


*/























/*
package narad.projects.bpdp

import narad.inference._
import scala.util.matching._

class LinearChainTagger(pots: Array[Potential], scores: Array[Double], words: Array[String], tagset: Array[String]) {
	val potPattern = """pos\([0-9]+,([0-9]+)\)""".r
	val fg = new FactorGraphBuilder(pots, scores)
	
	val vars = (0 until words.size).toList.map("posvar%d".format(_)).toArray
	println("slen = " + words.size)
	println("tags = " + tagset.size)
//	println(pots.map(_.name).mkString(", "))
	fg.addVariables(vars, tagset.size)
	val slen = words.size
	for (i <- 0 until slen) {
		fg.addNamedFactor("pos\\(%d,[0-9]+\\)".format(i), "posvar%d".format(i))
		fg.addNamed2Factor(new Regex("bpos\\(%d,([0-9]+),([0-9]+)\\)".format(i)), "posvar%d".format(i), "posvar%d".format(i+1))
	}
	val tagger = fg.toFactorGraph
	println(tagger)
	
	def graph: FactorGraph = tagger
	
	def potentialBeliefs: Array[(Potential, Double)] = {
		val buf = new collection.mutable.ArrayBuffer[(Potential, Double)]
		for (factor <- graph.factors) {
			println("computing beliefs, looking at factor %s".format(factor))
			val beliefs = factor.getBeliefs(graph)
			println("BELIEFS == " + beliefs.size)
			buf ++= beliefs //factor.getBeliefs(graph)
		}
		buf.toArray
//		graph.factors.map(_.potentials(fg)).toArray.flatten
	}
	
	def decode = {
		val beliefs = potentialBeliefs
		for (i <- 0 until words.size) {
			var mtag = ""
			var mscore = -10000.0
			for (pot <- beliefs.filter(_._1.name.contains("pos(%d,".format(i)))) {
				println(pot._1.name +":\t" + pot._2)
				if (pot._2 > mscore) {
					mscore = pot._2
					mtag = pot._1.name
				}
			}
			val potPattern(tidx) = mtag
			println("%s\t%s".format(words(i), tagset(tidx.toInt)))
		}
	}
}
*/