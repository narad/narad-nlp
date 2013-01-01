package narad.nlp.tagger
import narad.io.conll._
import collection.mutable.HashMap
import narad.util.ArgParser

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 11/24/12
 * Time: 6:31 PM
 * To change this template use File | Settings | File Templates.
 */
class MaxentDictionary(data: Array[CoNLLDatum]) extends TagDictionary { //with MaxentBackoff {
//  val params = new TaggerParams("""--train.file %s
//                                """.format(split(" "))
//  val tagger = Tagger.run2(params)


/*
  override def tags(w: String): Iterator[String] = {
    tags()
  }

  override def tags(w: String, freqThreshold: Int): Iterator[String] = {
    val word = w.toLowerCase
    this.getOrElse(word, new HashMap[String, Int]()).filter(_._2 >= freqThreshold).keys.toList.sortBy(_.toString).iterator
  }
  */
}

object MaxentDictionary {

  def main(args: Array[String]) {
    val reader = new CoNLLReader(args(0))
    val data = reader.iterator.toArray
    val dict = new MaxentDictionary(data)

  }
}
