package narad.nlp.ner

import narad.nlp.tagger.TagDictionary
import narad.bp.optimize.{OptimizerOptions, L1Regularizer, Optimizer}
import narad.bp.util.PotentialReader
import narad.util.ArgParser
import narad.bp.structure.ModelOptions
import narad.io.onto._

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 12/11/12
 * Time: 8:34 PM
 * To change this template use File | Settings | File Templates.
 */
object NamedEntityRecognizer {

  def main(args: Array[String]) = {
    val params = new NamedEntityParams(args)
    val ner = new NamedEntityModel(params)

    if (params.getBoolean("--extract.features")) {
      //val dict   = TagDictionary.construct(params.TRAIN_FILE, mode=params.MODE)
      //dict.toFile("tags.dict")
      //			println(dict.all.mkString("\n"))
      val reader = new OntoReader(params.TRAIN_NER_FILE, params.TRAIN_SYNTAX_FILE, params)
      val labels = reader.map(_.ner.entities.map(_.label)).flatten.toArray.distinct
      System.err.println("Using %d labels:\n%s".format(labels.size, labels.mkString("\n")))
      ner.extractFeatures(params.TRAIN_NER_FILE, params.TRAIN_SYNTAX_FILE, params.TRAIN_FEATURE_FILE, labels, 10, params)
      ner.extractFeatures(params.TEST_NER_FILE, params.TEST_SYNTAX_FILE, params.TEST_FEATURE_FILE, labels, 10, params)
    }
    else if (params.getBoolean("--train")) {
      val optimizer = new Optimizer(ner) with L1Regularizer
      val data = PotentialReader.read(params.TRAIN_FIDX_FILE).toArray
      optimizer.train(data, params)
    }
    else if (params.getBoolean("--test")) {
      val optimizer = new Optimizer(ner)
      val data = PotentialReader.read(params.TEST_FIDX_FILE).toArray
      optimizer.test(data, params)
    }
  }
}


class NamedEntityParams(args: Array[String]) extends ArgParser(args) with ModelOptions with OptimizerOptions {

  def LABEL_TYPE = getString("--label.type", "FINE")

  def PRINT_INTERVAL = getInt("--print.interval", 100)
  def TRAIN_FIDX_FILE = getString("--train.fidx.file", "train.fidx")
  def TEST_FIDX_FILE  = getString("--test.fidx.file", "test.fidx")
  def TRAIN_FEATURE_FILE = getString("--train.feature.file", "train.feats")
  def TEST_FEATURE_FILE = getString("--test.feature.file", "test.feats")
  def TRAIN_NER_FILE = getString("--train.ner.file")
  def TEST_NER_FILE  = getString("--test.ner.file")
  def TRAIN_SYNTAX_FILE = getString("--train.syntax.file")
  def TEST_SYNTAX_FILE  = getString("--test.syntax.file")

  def VARIANCE = getDouble("--variance", 1.0)
  def RATE = getDouble("--rate", .01)
  def PV_SIZE = getInt("--pv.size", -1)
  def TRAIN_ITERATIONS = getInt("--train.iterations", 10)
  def TRAIN_ORDER = getString("--train.order", "NORMAL")
  def MODEL_OUTPUT_FILE = getString("--model.output.file", "model")
  def INIT_FILE = getString("--init.file")
  def BATCH_SIZE = getInt("--batch.size", 1)
  def AVERAGE_LAST = getBoolean("--average.last", false)

  def DAMP_INIT = getDouble("--damp.init", 1.0)
  def DAMP_RATE = getDouble("--damp.rate", 0.01)
  def DIFF_THRESHOLD = getDouble("--diff.threshold", 0.001)
  def INFERENCE_ITERATIONS = getInt("--inference.iterations", 10)
  def VERBOSE = getBoolean("--verbose", false)

}


