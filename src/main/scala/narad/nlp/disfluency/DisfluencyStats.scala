package narad.nlp.disfluency

import narad.util.HashCounter
import narad.io.disfluency.{DisfluencyDatum, DisfluencyReader}
import narad.nlp.trees.{ConstituentTree => Tree}
import collection.mutable.HashSet

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 7/23/13
 * Time: 11:39 AM
 */
class DisfluencyStats extends HashCounter[String] {

}

object DisfluencyStats {

  def main(args: Array[String]) {
    val dDir = args(0)
    val sDir = args(1)
    val reader = new DisfluencyReader(dDir, sDir)
    construct(reader)
  }

  def construct(data: Iterable[(DisfluencyDatum, Tree)]): DisfluencyStats = {
    val stats = new DisfluencyStats
    val labels  = new HashSet[String]


    for (d <- data) {
      val dis = d._1
      val tree = d._2
      for (t <- tree.depthFirstSearch) {
        stats.increment("label_" + t.label)
        if (t.label == "(INTJ") println("WEIRD: " + t.tokens.mkString(" "))
      }
      println(d._1.toString())
      println
      println(d._2.toString())
      println
    }
    println("# of Disfluency Datums: " + data.size)
    println("# of labels: " + stats.keys.filter(_.startsWith("label_")).size)
    println(stats.keys.filter(_.startsWith("label_")).map{k => "%s: %s".format(k.substring(6), stats.count(k))}.mkString("\n"))
    stats
  }
}