package narad.nlp.tagger

import narad.bp.optimize.{EvalContainer, Scorable}
import narad.util.eval._
import collection.mutable.HashMap
import collection.mutable

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 5/1/13
 * Time: 3:17 PM
 */

class MultiTaggedSentence extends Scorable {
  val tags = new HashMap[String, Array[String]]

  def addTags(label: String, tagsToAdd: Array[String]) {
    tags(label) = tagsToAdd
  }

  def getTags(label: String): Array[String] = {
    tags.getOrElse(label, Array[String]())
  }

  def score(other: Scorable): MultiAccuracyContainer = {
    other match {
      case x : MultiTaggedSentence => {
        val accuracies = new HashMap[String, AccuracyContainer]()
        for (t <- tags.keys) {
          accuracies(t) = new AccuracyContainer(0,0)
          val t1s = tags(t)
          val t2s = x.tags(t)
          assert(t1s.size == t2s.size)
          for (i <- 0 until t1s.size) {
            if (t1s(i) == t2s(i)) {
              accuracies(t).correct += 1
            }
            else {
              accuracies(t).incorrect += 1
            }
          }
        }
        new MultiAccuracyContainer(accuracies)
      }
      case _ => {
        val accuracies = new mutable.HashMap[String, AccuracyContainer]()
        new MultiAccuracyContainer(accuracies)
      }
    }
  }
}