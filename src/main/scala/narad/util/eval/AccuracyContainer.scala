package narad.util.eval

import narad.bp.optimize.EvalContainer

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 5/29/13
 * Time: 4:32 PM
 */

class AccuracyContainer(var correct: Int, var incorrect: Int) extends EvalContainer {

  def accuracy = 1.0 * correct / total

  def total = correct + incorrect

  def combine(that: EvalContainer): AccuracyContainer = {
    that match {
      case x: AccuracyContainer => {
        new AccuracyContainer(correct + x.correct, incorrect + x.incorrect)
      }
      case _ => this
    }
  }
}