package narad.nlp.parser.dependency

import narad.io.conll.CoNLLReader
import narad.util.ArgParser
import narad.bp.optimize.{L1Regularizer, Optimizer}
import narad.bp.util.{PotentialExample, PotentialReader}
import narad.bp.structure._
import narad.bp.inference.BeliefPropagation
import collection.mutable.{ArrayBuffer, HashSet}
import narad.bp.structure.Potential
import narad.bp.util.PotentialExample
import scala.util.matching.Regex

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 11/25/12
 * Time: 12:47 AM
 * To change this template use File | Settings | File Templates.
 */

object DependencyParser {

  def main(args: Array[String]) {
    val params = new DependencyParserParams(args)
    val parser = new ProjectiveDependencyParser(params)
    if (params.getBoolean("--extract.features")) {
      parser.extractFeatures(params.TRAIN_FILE, params.TRAIN_FEATURE_FILE, params)
      parser.extractFeatures(params.TEST_FILE, params.TEST_FEATURE_FILE, params)
    }
    else if (params.getBoolean("--train")) {
      val optimizer = new Optimizer(parser) with L1Regularizer
      val data = PotentialReader.read(params.TRAIN_FIDX_FILE).toArray
      optimizer.train(data, params)
    }
    else if (params.getBoolean("--test")) {
      val optimizer = new Optimizer(parser)
      val data = PotentialReader.read(params.TEST_FIDX_FILE).toArray
      optimizer.test(data, params)
    }
  }
}

class ProjectiveDependencyParser(params: DependencyParserParams) extends FactorGraphModel with DependencyParseFeatures with BeliefPropagation {
  val linkPattern  = """un\(([0-9]+),([0-9]+)\)""".r

  def constructFromExample(ex: PotentialExample, pv: Array[Double]): ModelInstance = {
     val pots = ex.exponentiated(pv)
    System.err.println("%d pots found.".format(pots.size))
    val fg = new FactorGraphBuilder(pots)
    val pothash = pots.groupBy { p => val linkPattern(start, end) = p.name; (start.toInt, end.toInt) }
    val slen = ex.attributes.getOrElse("slen", "-1").toInt
    for (dep <- 1 to slen; head <- 0 to slen if dep != head) {
      fg.addVariable("%s(%d,%d)".format("linkvar", head, dep), 2)
      fg.addUnaryFactor("linkvar(%d,%d)".format(head, dep), "link\\(%d,%d\\)".format(head, dep), pothash((head, dep))(0))
      // bpdp code kept a matrix for link vars, would have links[dep][head] = the variable
    }
    fg.addProjectiveTreeFactor(new Regex("linkvar\\("), "PTREE", slen)
    return new DependencyParserModelInstance(fg.toFactorGraph, ex)
  }

  def decode(instance: ModelInstance) = {
    val beliefs = instance.marginals
    println
  }

  def options = params
}

class DependencyParserModelInstance(graph: FactorGraph, ex: PotentialExample) extends ModelInstance(graph, ex)







/*
package narad.nlp.parse

import narad.bp.structure._
import narad.bp.train._
import narad.bp.util._
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
*/