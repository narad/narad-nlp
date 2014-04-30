package narad.nlp.srl

import narad.io.srl.SRLReader
import narad.util.HashCounter

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 9/18/13
 * Time: 12:16 PM
 */
object SRLStatistics {

  def main(args: Array[String]) {
    val stats = new HashCounter[String]
    val roles = new HashCounter[String]
    val argLens = new HashCounter[String]
    val frames = new HashCounter[String]
    val pos = new HashCounter[String]
    var i = 0
    for (srl <- new SRLReader(args(0))) {
      i += 1
      val tree = srl.goldTree
      for (t <- srl.postags) if (!pos.contains(t)) pos(t) = 0.0
      for (p <- srl.frames; a <- p.args) {
        pos.increment(srl.postag(a.aidx))
        frames.increment(p.args.map(_.label).sortBy(_.toString).mkString(" "))
        roles.increment(a.label)
        argLens.increment(Math.abs(p.pidx - a.aidx).toString)
        val plen = tree.hasDirectedPath(a.aidx, p.pidx) match {
          case Some(path) => path.size
          case _=> {
      //      println(srl.toString)
            -1 }
        }
        stats.increment("args")
        stats.increment("path-%d".format(plen))
        if (tree.crosses(a.aidx, p.pidx)) {
          stats.increment("crosses")
//          println(srl)
//          println("SRL arg pair (%d,%d) crosses tree depenency".format(p.pidx, a.aidx))
//          println
        }
      }
    }
    println(" % of SRL covered by paths of len:")
    var sum = 0.0
    for (i <- 1 until 10) {
      val pct = stats.count("path-%d".format(i)) * 100 / stats.count("args")
      sum += pct
      println("  " + i + " = " + sum)
    }
    println(" % crosses = " + stats.count("crosses") * 100 / stats.count("args"))
    println(" # sentences = " + i)
    println()
    println("Role Frequencies (%d):".format(roles.keys.size))
    roles.toArray.sortBy(_._2 * -1).foreach { case(role, freq) =>
      println("  %s\t%f".format(role, freq))
    }
    println()
    println("Arg Lens:")
    for (i <- 0 to 80) {
      println("  %d\t%f".format(i, argLens.count(i.toString)))
    }
    println()
    println("Frame Frequencies (%d):".format(frames.keys.size))
    frames.toArray.sortBy(_._2 * -1).foreach { case(frame, freq) =>
      println("  %s\t%f".format(frame, freq))
    }
    println
    println("# of args per predicate:")
    new SRLReader(args(0)).map(_.frames).flatten.map(_.args.size).groupBy(_ * 1).toArray.sortBy(_._1).foreach { p =>
      println("  %d\t%d".format(p._1, p._2.size))
    }
    println()
    println("Duplicate Frequencies:")
    val dupedFrames = frames.map(_._1.split(" ")).filter(f => f.size != f.distinct.size)
    roles.toArray.sortBy(_._2 * -1).foreach { case(role, freq) =>
      val dcount = dupedFrames.filter(_.count(_ == role) >= 2).size
      if (dcount > 0) println("  %s\t%d".format(role, dcount))
    }
    println
    if (args.size > 1) {
      val trainLemmas = new SRLReader(args(0)).map(d => d.predicates.map(d.lemma(_))).flatten.toArray.distinct
      val testLemmas = new SRLReader(args(1)).map(d => d.predicates.map(d.lemma(_))).flatten.toArray.distinct
      val nf = testLemmas.filter(!trainLemmas.contains(_))
      println("Lemmas found in test data not found in training data: (%d/%d) = %f".format(nf.size, testLemmas.size, nf.size * 1.0 / testLemmas.size))
    }
    println
    println("POS tags as args:")
    for (k <- pos.keys.toArray.sortBy(pos(_))) {
      println(k + ": " + pos(k))
    }
  }
}
