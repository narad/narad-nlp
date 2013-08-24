package narad.io.ner

import narad.nlp.ner.NamedEntity
import narad.bp.optimize.Scorable
import narad.nlp.parser.metrics.EvalBContainer
import narad.util.eval.AccuracyContainer
import narad.util.HashCounter
import narad.bp.optimize.EvalContainer
import narad.io.onto.OntoDatum


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

  def score(other: Scorable): F1Container = {
    other match {
      case that: OntoDatum => {
        val test = this
        val gold = that.ner
        val correct  = gold.entities.filter{e => test.entities.exists(t => e.start == t.start && e.end == t.end && e.label == t.label)}.size//.foldLeft(0)(_+_.width)
        val ucorrect = gold.entities.filter{e => test.entities.exists(t => e.start == t.start && e.end == t.end)}.size
        val hc = new F1Container
        hc("labeled correct") = correct
        hc("unlabeled correct") = ucorrect
        hc("test comps") = test.entities.size//.foldLeft(0)(_+_.width)
        hc("gold comps") = gold.entities.size//foldLeft(0)(_+_.width)
        hc
      }
      case that: NamedEntityDatum => {
        val test = this
        val gold = that
        val correct  = gold.entities.filter{e => test.entities.exists(t => e.start == t.start && e.end == t.end && e.label == t.label)}.size //.foldLeft(0)(_+_.width)
        val ucorrect = gold.entities.filter{e => test.entities.exists(t => e.start == t.start && e.end == t.end)}.size
        val hc = new F1Container
        hc("labeled correct") = correct
        hc("unlabeled correct") = ucorrect
        hc("test comps") = test.entities.size//.foldLeft(0)(_+_.width)
        hc("gold comps") = gold.entities.size//.foldLeft(0)(_+_.width)
        hc
      }
      case _=> new F1Container
    }
  }

  override def toString = {
    val sb = new StringBuilder
    for (i <- 0 until size) {
      val e = entities.find(e => e.start == i)
      e match {
        case Some(e) => {
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
    val up  = count("unlabeled correct") / count("test comps")
    val ur  = count("unlabeled correct") / count("gold comps")
    val uf1 = if ((up + ur) == 0) 0 else 2 * ((up * ur) / (up + ur))
    val lp  = count("labeled correct") / count("test comps")
    val lr  = count("labeled correct") / count("gold comps")
    val lf1 = if ((lp + lr) == 0) 0 else 2 * ((lp * lr) / (lp + lr))
    val sb = new StringBuilder
    sb.append("NER ACCURACY:\n")
    sb.append("  Labeled   - (P=%f/R=%f) = %f\n".format(lp, lr, lf1))
    sb.append("  Unlabeled - (P=%f/R=%f) = %f\n".format(up, ur, uf1))
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
