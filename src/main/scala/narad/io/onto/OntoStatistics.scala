package narad.io.onto

import narad.util.HashCounter
import collection.mutable.HashSet

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 9/24/13
 * Time: 6:44 PM
 */
class OntoStatistics { //(entityLabels: HashSet[String], entityConstituents: HashSet[String]) {
  val entityLabels = new HashSet[String]
  val entityConstituents = new HashSet[String]

  def addEntityLabel(label: String) = entityLabels += label

  def addEntityConstituent(label: String) = entityConstituents += label

}

object OntoStatistics {

  def construct(reader: OntoReader): OntoStatistics = {
    val os = new OntoStatistics
    for (od <- reader) {
      for (i <- 0 until od.slen; j <- i+1 to od.slen) {
        if (od.ner.containsSpan(i, j)) {
          os.addEntityLabel(od.ner.labelOf(i, j))
          if (od.tree.containsSpan(i, j)) {
            os.addEntityConstituent(od.tree.labelsOfSpan(i, j).toArray.head)
          }
        }
      }
    }
    os
  }

  def main(args: Array[String]) {
    val hc = new HashCounter[String]
    for (datum <- new OntoReader(args(0), args(1))) {
//      println(datum.ner.entities.mkString("\n"))
//      println(datum.tree.toSpans.mkString("\n"))
      var btree = datum.tree
      btree = btree.binarize()

      for (nspan <- datum.ner.entities if nspan.width > 1) {
        val labels = datum.tree.labelsOfSpan(nspan.start, nspan.end).toArray
        if (labels.isEmpty) {
          println("no match for " + nspan)
          println(datum.tree)
          println
          hc.increment("NONE")
        }
        else{
          labels.foreach(hc.increment(_))
        }
        val blabels = btree.labelsOfSpan(nspan.start, nspan.end).toArray
        if (!blabels.isEmpty) {
          hc.increment("bin-align")
        }
        hc.increment("entity")
      }
    }
    val sum = hc.sum
    for (k <- hc.keys) {
      println("%s = (%f/%f) = %f".format(k, hc.count(k), sum, (hc.count(k) * 100 / sum)))
    }
    println("pct aligned to binarized trees = (%f/%f) = %f".format(hc.count("bin-align"), hc.count("entity"), hc.count("bin-align") * 100 / hc.count("entity")))
  }
}
