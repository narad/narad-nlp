package narad.util.eval

import collection.mutable.HashMap
import narad.bp.optimize.EvalContainer

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 5/29/13
 * Time: 4:31 PM
 */
class MultiAccuracyContainer(val accuracies: HashMap[String, AccuracyContainer]) extends EvalContainer {

  def combine(that: EvalContainer): EvalContainer = {
    that match {
      case x: MultiAccuracyContainer => {
        val counts = this.accuracies.clone()
        for (key <- x.accuracies.keys) {
          if (counts.contains(key)) {
            counts(key) = counts(key).combine(x.accuracies(key))
          }
          else {
            counts(key) = x.accuracies(key)
          }
        }
        new MultiAccuracyContainer(counts)
      }
      case _ => this
    }
  }

  override def toString = {
    val sb = new StringBuilder
    for (attr <- accuracies.keys) {
      val attrAcc = accuracies(attr)
      sb.append("%s accuracy = (%d/%d) = %f\n".format(attr, attrAcc.correct, attrAcc.total, attrAcc.accuracy))
    }
    sb.deleteCharAt(sb.size-1)
    sb.toString()
  }
}