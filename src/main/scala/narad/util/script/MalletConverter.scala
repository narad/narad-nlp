package narad.util.script

import narad.util.ArgParser
import narad.io.conll.CoNLLReader

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 3/5/13
 * Time: 12:39 PM
 * To change this template use File | Settings | File Templates.
 */
object MalletConverter {

  def main(args: Array[String]) {
    def options = new ArgParser(args)
//    for (line)
  }
}

object MalletTaggerFeatures {

  def main(args: Array[String]) {
    val options = new ArgParser(args)
    val filename = options.getString("--input.file")
    val train = options.getBoolean("--train")
    val test = options.getBoolean("--test")
    for (datum <- new CoNLLReader(filename)) {
      val words = datum.words.toArray
      for (i <- 1 to datum.slen) {
        val feats = Array("BIAS", datum.word(i))
        if (train) {
          println("%s %s".format(feats.mkString(" "), datum.postag(i)))
        }
        else {
          println("%s".format(feats.mkString(" ")))
        }
      }
      println
    }
  }

}
