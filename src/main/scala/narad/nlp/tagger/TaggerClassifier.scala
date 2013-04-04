package narad.nlp.tagger

import narad.bp.util.index.Index
import narad.bp.optimize.Optimizer
import narad.bp.util.{Feature, PotentialExample, PotentialReader}
import narad.bp.structure.{Potential, ModelInstance}
import collection.mutable.{HashMap, ArrayBuffer, Map}

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 2/10/13
 * Time: 10:24 AM
 * To change this template use File | Settings | File Templates.
 */

class TaggerClassifier(pv: Array[Double], dict: TagDictionary, index: Index[String], params: TaggerParams) extends TaggerFeatures {
  val LABEL_PATTERN  = """ulabel\(([0-9]+),(.+)\)""".r

  def classify(words: Array[String]): Array[String] = {
    val tagger = new UnigramTaggerModel(params)
    val pex = getFeatures(words, Array[String](), dict, index)
    val mex = tagger.constructFromExample(pex, pv)
    val optimizer = new Optimizer(tagger, params)
    val data = new PotentialReader(params.TRAIN_FIDX_FILE)
    optimizer.infer(mex, params)
    decode(mex)
  }

  def decode(instance: ModelInstance): Array[String] = {
    val beliefs = instance.marginals
    val tags = new ArrayBuffer[String]
    val groups = beliefs.filter(_.name.startsWith("ulabel")).groupBy{pot =>
      val LABEL_PATTERN(widx, lidx) = pot.name
      widx
    }
    for (group <- groups.toArray.sortBy(_._1.toInt)) {
      val idx = group._1.toInt
      val pots = group._2
      assert(pots.size != 0, "Pots for group %d are empty in decoding?".format(idx))
      val maxpot = narad.util.Functions.argmax[Potential](_.value, pots)
      val LABEL_PATTERN(i,j) = maxpot.name
      tags += j
    }
    return tags.toArray
  }

  def getFeatures(words: Array[String], tags: Array[String], dict: TagDictionary, index: Index[String]): PotentialExample = {
    val map  = Map[String, String]()
    val pots = new ArrayBuffer[Potential]
    val featmap = new HashMap[String, Array[Feature]]
    val slen = words.size
    val alltags = dict.all.toArray
    map("slen") = slen.toString
    map("words") = words.mkString(" ")
    map("bigram") = "false"
    for (i <- 1 to slen) {
      val word = words(i)
      val feats = unigramLexicalFeatures(words, i, params)
      val dtags = if (dict.contains(word)) dict.tags(word).toArray else alltags
      dtags.zipWithIndex.foreach { case(tag, tidx) =>
        val name = "ulabel(%d,%s)".format(i, tag)
        val correct = tags.size > 0 && tag == tags(i)
        pots += new Potential(0.0, name, correct)
        featmap(name) = feats.view.map("%s_%s".format(tag, _)).map(s => new Feature(index.index(s), 1.0, 0)).toArray
      }
    }
    return new PotentialExample(map, pots, featmap)
  }
}
