package narad.io.ner

import narad.nlp.ner.NamedEntity
import narad.bp.optimize.Scorable
import narad.nlp.parser.metrics.EvalBContainer
import narad.util.eval.AccuracyContainer
import narad.util.HashCounter
import narad.bp.optimize.EvalContainer
import narad.io.onto.{OntoDatum, OntoEvalContainer}


/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 7/20/13
 * Time: 2:18 AM
 */
case class NamedEntityDatum(words: Array[String], entities: Array[NamedEntity]) extends Scorable {

  def containsSpan(start: Int, end: Int): Boolean = entities.exists(e => e.start == start && e.end == end)

  def containsSpanLabel(start: Int, end: Int, label: String): Boolean = entities.exists(e => e.start == start && e.end == end && e.label == label)

  def coversSpan(start: Int, end: Int): Boolean = entities.exists(e => e.start <= start && e.end >= end && e.width > 1)

  def labels = entities.map(_.label)

  def size = words.size

  def entityAt(i: Int, j: Int): NamedEntity = {
    entities.find(e => e.start == i && e.end == j) match {
      case Some(e) => e
      case _ => {
        assert(false, "Entity not found in Entity At")
        null.asInstanceOf[NamedEntity]
      }
    }
  }

  def labelOf(i: Int, j: Int): String = {
    entityAt(i,j).label
  }

  def score(other: Scorable): F1Container = {
    other match {
      case that: OntoDatum => {
        val test = that.ner
        val gold = this
        val correct  = gold.entities.filter{e => test.entities.exists(t => e.start == t.start && e.end == t.end && e.label == t.label)}.size//.foldLeft(0)(_+_.width)
        val ucorrect = gold.entities.filter{e => test.entities.exists(t => e.start == t.start && e.end == t.end)}.size
        val hc = new OntoEvalContainer
        hc("ner labeled correct") = correct
        hc("ner unlabeled correct") = ucorrect
        hc("ner test comps") = test.entities.size//.foldLeft(0)(_+_.width)
        hc("ner gold comps") = gold.entities.size//foldLeft(0)(_+_.width)
        hc("count") = 1
        hc
      }
      case that: NamedEntityDatum => {
        val test = that
        val gold = this
        val correct  = gold.entities.filter{e => test.entities.exists(t => e.start == t.start && e.end == t.end && e.label == t.label)}.size //.foldLeft(0)(_+_.width)
        val ucorrect = gold.entities.filter{e => test.entities.exists(t => e.start == t.start && e.end == t.end)}.size
        val hc = new F1Container
        hc("ner labeled correct") = correct
        hc("ner unlabeled correct") = ucorrect
        hc("ner test comps") = test.entities.size//.foldLeft(0)(_+_.width)
        hc("ner gold comps") = gold.entities.size//.foldLeft(0)(_+_.width)
        test.entities.foreach { te =>
          gold.entities.foreach { ge =>
            if (te.start == ge.start && te.end == ge.end && te.label != ge.label) {
              hc.increment("mislabeled %s as %s".format(ge.label, te.label))
            }
          }
        }
        hc("count") = 1
//        hc("labal_errors") = hc("labal_errors") ++ Array()
//        System.err.println(gold.entities.filter{e => test.entities.exists(t => e.start == t.start && e.end == t.end && e.label == t.label)}.mkString("\n"))
//        System.err.println("%d/%d guesses correct.".format(correct, test.entities.size))
        hc
      }
      case _=> new F1Container
    }
  }

  override def toString = {
 //   println("words = " + words.mkString(" "))
    val sb = new StringBuilder
    for (i <- 0 until size) {
      val e = entities.find(e => e.start == i)
      e match {
        case Some(e) => {
//          println("e = " + e)
          sb.append("<ENAMEX TYPE=\"" + e.label + "\">")
        }
        case _=>
      }
      sb.append(words(i))
      if (entities.exists(_.end == i+1)) sb.append("</ENAMEX>")
      sb.append(" ")
    }
    sb.toString.trim
  }

  def getLabel(i: Int): String = {
    for (e <- entities) {
      if (e.start == i) return "B-" + e.label
      if (e.start < i && e.end > i) return "I-" + e.label
    }
    return "O"
  }
}

class F1Container extends HashCounter[String] with EvalContainer {

  def combine(that: EvalContainer): F1Container = {
    that match {
      case x: F1Container => {
        val e = new F1Container
        for (k <- this.keys) {
          e.increment(k, this(k))
        }
        for (k <- x.keys) {
          e.increment(k, x(k))
        }
        return e
      }
      case _ => {
        println("exiting the combine op")
        this
      }
    }
  }

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
    sb.append("PARSE ACCURACY:    Labeled - (P=%f/R=%f) = %f\n".format(plp, plr, plf1))
    sb.append("PARSE ACCURACY:  UnLabeled - (P=%f/R=%f) = %f\n".format(pup, pur, puf1))
    sb.toString
//    "NER ACCURACY: \n  Labeled - (P=%f/R=%f) = %f".format(lp, lr, lf1)
  }

  def toVerboseString = {
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
    sb.append("\n")
    sb.append("NER Precision:   Labeled - (%1.0f/%1.0f) = %f\n".format(count("ner labeled correct"), count("ner test comps"), lp))
    sb.append("NER Precision: Unlabeled - (%1.0f/%1.0f) = %f\n".format(count("ner unlabeled correct"), count("ner test comps"), up))
    sb.append("NER Recall:   Labeled - (%1.0f/%1.0f) = %f\n".format(count("ner labeled correct"), count("ner gold comps"), lr))
    sb.append("NER Recall: Unlabeled - (%1.0f/%1.0f) = %f\n".format(count("ner unlabeled correct"), count("ner gold comps"), ur))
    val plp  = count("labeled correct") / count("test comps")
    val plr  = count("labeled correct") / count("gold comps")
    val pup  = count("unlabeled correct") / count("test comps")
    val pur  = count("unlabeled correct") / count("gold comps")
    val plf1 = if ((plp + plr) == 0) 0 else 2 * ((plp * plr) / (plp + plr))
    val puf1 = if ((pup + pur) == 0) 0 else 2 * ((pup * pur) / (pup + pur))
    //    sb.append("PARSE ACCURACY:\n")
    sb.append("PARSE ACCURACY:    Labeled - (P=%f/R=%f) = %f\n".format(plp, plr, plf1))
    sb.append("PARSE ACCURACY:  UnLabeled - (P=%f/R=%f) = %f\n".format(pup, pur, puf1))
    sb.append("\n")
    sb.append("Incorrect Labelings:\n")
    for (key <- keys.filter(_.startsWith("mislabeled"))) {
      sb.append("%s\t%1.0f\n".format(key, count(key)))
    }
    sb.toString
    //    "NER ACCURACY: \n  Labeled - (P=%f/R=%f) = %f".format(lp, lr, lf1)
  }
}









       /*
       	def stats(gdatum: NamedEntityDatum, tdatum: NamedEntityDatum): F1Container = {
		val correct  = gdatum.entities.filter{e => tdatum.entities.exists(e==_)}.size
		val ucorrect = gdatum.entities.filter{e => tdatum.entities.exists(t => e.start == t.start && e.end == t.end)}.size
		val pden = tdatum.entities.size
		val rden = gdatum.entities.size
		return F1Container(correct, ucorrect, pden, rden)
	}


	  def score(other: Scorable): EvalBContainer = {
    other match {
      case that: ConstituentTree => EvalBContainer.construct(goldTree=that, testTree=this)
      case _=> new EvalBContainer()
    }
  }
        */
