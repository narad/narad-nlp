package narad.io.onto

import narad.io.ner.{F1Container, NamedEntityDatum}
import narad.nlp.trees.ConstituentTree
import narad.bp.optimize.Scorable
import narad.nlp.parser.metrics.EvalBContainer

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 9/24/13
 * Time: 10:22 PM
 */
case class OntoDatum(ner: NamedEntityDatum, tree: ConstituentTree) extends Scorable {

  def tokens = tree.tokens

  def slen = tree.length

  def score(other: Scorable) = {
    other match {
      case x: OntoDatum => {
        val oe = new OntoEvalContainer
        val evalb = tree.score(x.tree) //EvalBContainer.construct(tree, x.tree)
        val f1c = ner.score(x.ner)
        oe ++= f1c
        oe ++= evalb
        oe
      }
      case _ => {
        println("Entered OntoDatum scorer with <other> not of class OntoDatum")
        ner.score(other)
      }
    }
  }

  override def toString = ner.toString + "\n" + tree.toString
}

//    println("onto scorer")
//    val evalb = tree.score(other)
//    println("Tree: " + tree)
//    println("evalb keys:\n" + evalb.keys.mkString("\n"))
//    println("EVALB:\n " + evalb)
//    val nf1 = ner.score(other)
//    evalb.combine(nf1)
//    evalb.combine()
//    ner.score(other).combine(tree.score(other))


class OntoEvalContainer extends F1Container {

  override def toString = {
    val up  = count("ner unlabeled correct") / count("ner test comps")
    val ur  = count("ner unlabeled correct") / count("ner gold comps")
    val uf1 = if ((up + ur) == 0) 0 else 2 * ((up * ur) / (up + ur))
    val lp  = count("ner labeled correct") / count("ner test comps")
    val lr  = count("ner labeled correct") / count("ner gold comps")
    val lf1 = if ((lp + lr) == 0) 0 else 2 * ((lp * lr) / (lp + lr))
    val sb = new StringBuilder
//    sb.append("NER ACCURACY:\n")
    sb.append("NER ACCURACY:    Labeled - (P=%f/R=%f) = %f\n".format(lp, lr, lf1))
    sb.append("NER ACCURACY:  Unlabeled - (P=%f/R=%f) = %f\n".format(up, ur, uf1))
    val plp  = count("labeled correct") / count("test comps")
    val plr  = count("labeled correct") / count("gold comps")
    val pup  = count("unlabeled correct") / count("test comps")
    val pur  = count("unlabeled correct") / count("gold comps")
    val plf1 = if ((plp + plr) == 0) 0 else 2 * ((plp * plr) / (plp + plr))
    val puf1 = if ((pup + pur) == 0) 0 else 2 * ((pup * pur) / (pup + pur))
//    sb.append("PARSE ACCURACY:\n")
    sb.append("PARSE ACCURACY:    Labeled - (P=%f/R=f) = %f\n".format(plp, plr, plf1))
    sb.append("PARSE ACCURACY:  UnLabeled - (P=%f/R=f) = %f\n".format(pup, pur, puf1))
    sb.toString
  }
}
