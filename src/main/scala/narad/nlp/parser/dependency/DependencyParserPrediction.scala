package narad.nlp.parser.dependency

import narad.bp.structure.{Potential, FactorGraphBuilder}
import util.matching.Regex

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 10/9/13
 * Time: 4:14 PM
 */
trait DependencyParserPrediction {
  private val DP_SYNTAX_PATTERN  = """un\(([0-9]+),([0-9]+)\)""".r

  def addDependencySyntaxPrediction(graph: FactorGraphBuilder, pots: Iterable[Potential], slen: Int, observedSyntax: Boolean= false) {
    for (pot <- pots) {
      pot.name match {
        case DP_SYNTAX_PATTERN(hidx, kidx) => {
          graph.addUnaryVariable("linkVar(%s,%s)".format(hidx, kidx), "linkFac(%s,%s)".format(hidx, kidx), pot)
        }
        case _=> {}
      }
    }
    if (!observedSyntax) {
      graph.addProjectiveTreeFactor(new Regex("linkVar\\("), "PTREE", slen)
    }
  }
}


   /*
def addDependencySyntax(fg: FactorGraphBuilder, slen: Int, pots: Array[Potential], ) = {
val pothash = pots.groupBy { p => val LINK_PATTERN(start, end) = p.name; (start.toInt, end.toInt) }
for (dep <- 1 to slen; head <- 0 to slen if dep != head) {
fg.addVariable("%s(%d,%d)".format("linkvar", head, dep), 2)
fg.addUnaryFactor("linkvar(%d,%d)".format(head, dep), "link\\(%d,%d\\)".format(head, dep), pothash((head, dep))(0))
}
fg.addProjectiveTreeFactor(new Regex("linkvar\\("), "PTREE", slen)
}

         */



// bpdp code kept a matrix for link vars, would have links[dep][head] = the variable
