package narad.nlp.disfluency

import narad.bp.optimize.{Optimizer} // L2Regularizer
import narad.bp.util.{PotentialExample, PotentialReader}
import narad.bp.util.index._
import narad.nlp.parser.constituent._
import narad.io.disfluency._
import narad.nlp.trees.{ConstituentTree => Tree}
import scala.Array
import narad.nlp.ling.{TaggedToken => Token}

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 1/5/13
 * Time: 11:08 PM
 * To change this template use File | Settings | File Templates.
 */
object Disfluency {

  def main(args: Array[String]) {
    run(new DisfluencyParams(args))
  }

  def run(params: DisfluencyParams) {
    val dis = new DisfluencyModel(params)
    val stats = false
    if (stats) {
 //     printStats(params)
    }
    if (params.EXTRACT_FEATURES) {
      val reader = new DisfluencyReader(params.TRAIN_DISFLUENCY_FILE, params.TRAIN_SYNTAX_FILE)
//      val labels = util.iterator.toArray.map(_._2.binarize().nonterminals.map(_.label).toArray).flatten.distinct.sortBy(_.toString)
      val index = if (params.HASH_DICT) new HashIndex(params.PV_SIZE) else new ArrayIndex[String]()
      val trees = reader.iterator.map(_._2)
      val tstats = TreebankStatistics.construct(trees, prune=params.PRUNE)
      tstats.bin(numBins = 5, binSize=10)
      tstats.writeToFile("prune_stats.txt")
      dis.extractFeatures(params.TRAIN_DISFLUENCY_FILE, params.TRAIN_SYNTAX_FILE,
                          params.TRAIN_FEATURE_FILE, tstats, index, params)
      dis.extractFeatures(params.TEST_DISFLUENCY_FILE, params.TEST_SYNTAX_FILE,
                          params.TEST_FEATURE_FILE, tstats, index, params)
    }
    else if (params.getBoolean("--train")) {
//      val optimizer = new Optimizer(dis, params)
//      val data = new PotentialReader(params.TRAIN_FIDX_FILE)
//      optimizer.train(data)
    }
    else if (params.getBoolean("--test")) {
//      val optimizer = new Optimizer(dis, params)
//      val data = new PotentialReader(params.TEST_FIDX_FILE)
//      optimizer.test(data)
    }
  }

  /*
  def printStats(params: DisfluencyParams) {
    val out = new FileWriter("nt_probs.txt")
    val reader = new DisfluencyReader(params.TRAIN_DISFLUENCY_FILE, params.TRAIN_SYNTAX_FILE)
    val trees = reader.iterator.map(_._2).toArray

    val nts = trees.filter(!_.isLeaf).map(_.label).flatten

    val inside  = new HashCounter[String]
    val outside = new HashCounter[String]
    for (t <- trees) {
      val edits = new HashSet[(Int,Int)]
      for (st <- t) {
        if (st.label.startsWith("EDIT")) edits += ((st.start(), st.end()))
      }
      for (st <- t if st.width() <= 10) {
        if (edits.exists(t => t._1 > st.start() && t._2 <= st.end())) {
          inside.increment(st.label())
        }
        else {
          outside.increment(st.label())
        }
      }
    }
    val insideZ  = inside.keys.foldLeft(0.0)(_+inside.count(_))
    val outsideZ = outside.keys.foldLeft(0.0)(_+outside.count(_))
    out.write("INSIDE:\n")
    val insiders = inside.keys.map(i => (i, (inside.count(i) / insideZ))).toArray
    insiders.sortBy(_._2 * -1.0).foreach(p => out.write(p + "\n"))

    for (i <- inside.keys) {

      //        println(i + ":" + (inside.count(i) / insideZ))
    }

    out.write("\n\n\nOUTSIDE:\n")
    val outsiders = outside.keys.map(i => (i, (outside.count(i) / outsideZ))).toArray
    outsiders.sortBy(_._2 * -1.0).foreach(p => out.write(p + "\n"))
    for (i <- outside.keys) {
      //       println(i + ":" + (outside.count(i) / outsideZ))
    }
    out.close()
  }
  */
}


