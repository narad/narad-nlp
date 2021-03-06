package narad.nlp.srl

import narad.bp.optimize.EvalContainer
import narad.util.HashCounter

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 8/13/13
 * Time: 9:46 PM
 */
class SRLEvalContainer extends HashCounter[String] with EvalContainer {

  def combine(that: EvalContainer): SRLEvalContainer = {
    //    println("COMBINING")
    that match {
      case x: SRLEvalContainer => {
        val e = new SRLEvalContainer
        for (k <- this.keys) {
          e.increment(k, this(k))
        }
        for (k <- x.keys) {
          e.increment(k, x(k))
        }
        return e
      }
      case _ => {
        this
      }
    }
  }

  override def toString = {
    val up = count("correct arg") / count("test arg")
    val ur = count("correct arg") / count("total arg")
    val uf1 = if ((up + ur) == 0) 0 else 2 * ((up * ur) / (up + ur))

    val lp = count("correct role") / count("test arg")
    val lr = count("correct role") / count("total arg")
    val lf1 = if ((lp + lr) == 0) 0 else 2 * ((lp * lr) / (lp + lr))

    val sa = count("correct sense") / count("total preds") * 100

    val sb = new StringBuilder
    //    sb.append("# predicted args = " + count() + " / " + count("total arg") + " gold args.\n")
    sb.append("Unlabeled Precision: (%f/%f) = %f\n".format(count("correct arg"), count("test arg"), up))
    sb.append("Unlabeled Recall: (%f/%f) = %f\n".format(count("correct arg"), count("total arg"), ur))
    sb.append("Unlabeled F1: " + uf1 + "\n")
    sb.append("Labeled Precision: (%f/%f) = %f\n".format(count("correct role"), count("test arg"), lp))
    sb.append("Labeled Recall: (%f/%f) = %f\n".format(count("correct role"), count("total arg"), lr))
    sb.append("Labeled F1: " + lf1 + "\n")
    sb.append("Sense Accuracy: (%.2f / %.2f) = %.2f\n".format(count("correct sense"), count("total preds"), sa))
    for (i <- 0 to 5) {
      sb.append("gold frames of size %d = %f\n".format(i, count("gold frame " + i)))
    }
    for (i <- 0 to 5) {
      sb.append("test frames of size %d = %f\n".format(i, count("test frame " + i)))
    }
    for (i <- 1 to 5) {
      for (k <- keys.filter(_.startsWith("frame %d ".format(i)))) {
        sb.append(k + "\t" + this(k) + "\n")
      }
    }
    sb.toString()
  }
}

object SRLEvalContainer {

  def construct(goldSRL: SRLDatum, testSRL: SRLDatum): SRLEvalContainer = {
    val ec = new SRLEvalContainer()
    for (pidx <- goldSRL.predicates) {
      // Frame comps
      val gframe = goldSRL.argumentsOf(pidx).size
      val tframe = testSRL.argumentsOf(pidx).size
//      println("PIDX %d has %d arguments".format(pidx, gframe))
      ec.increment("frame %d %d".format(gframe, tframe))
      ec.increment("gold frame %d".format(gframe))
      ec.increment("test frame %d".format(tframe))
      // Sense comps
      if (goldSRL.sense(pidx) == testSRL.sense(pidx)) {
        ec.increment("correct sense")
      }
      for (aidx <- 0 to goldSRL.slen) {
        if (goldSRL.hasArg(pidx, aidx)) {
          if (testSRL.hasArg(pidx, aidx)) {
            ec.increment("correct arg")
          }
          ec.increment("total arg")
          val role = goldSRL.getLabel(pidx, aidx)
          if (testSRL.hasRole(pidx, aidx, role)) {
            ec.increment("correct role")
          }
        }
        if (testSRL.hasArg(pidx, aidx)) {
          ec.increment("test arg")
        }
      }
      ec.increment("total preds")
    }
    ec
  }
}















//    val lr  = count("correct arg") / count("gold comps")
//    val lf1 = if ((lp + lr) == 0) 0 else 2 * ((lp * lr) / (lp + lr))
//    "(P=%f/R=%f) = %f".format(lp, lr, lf1) //lf1.toString
//"%f\t%f\t%f".format(count("labeled correct"), count("gold comps"), count("test comps"))
