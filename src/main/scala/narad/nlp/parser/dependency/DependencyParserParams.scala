package narad.nlp.parser.dependency

import narad.util.ArgParser
import narad.bp.structure.ModelOptions
import narad.bp.optimize.OptimizerOptions

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 11/28/12
 * Time: 11:47 AM
 * To change this template use File | Settings | File Templates.
 */
class DependencyParserParams(args: Array[String]) extends ArgParser(args) with ModelOptions with OptimizerOptions {

  def PRINT_INTERVAL = getInt("--print.interval", 100)
  def TRAIN_FIDX_FILE = getString("--train.fidx.file", "train.fidx")
  def TEST_FIDX_FILE  = getString("--test.fidx.file", "test.fidx")
  def TRAIN_FEATURE_FILE = getString("--train.feature.file", "train.feats")
  def TEST_FEATURE_FILE = getString("--test.feature.file", "test.feats")
  def INTEGERIZE = getBoolean("--integerize", false)
  def TRAIN_FILE = getString("--train.file")
  def TEST_FILE  = getString("--test.file")

  lazy val TRAIN = getBoolean("--train")
  lazy val TEST  = getBoolean("--test")
  lazy val EXTRACT_FEATURES = getBoolean("--extract.features")
  lazy val HASH_DICT = getBoolean("--hash.dict")
  lazy val OUTPUT_FILE = getString("--output.file")

  def NUM_CORES = getInt("--num.cores", 1)
  def VARIANCE = getDouble("--variance", 1.0)
  def RATE = getDouble("--rate", .01)
  lazy val DECAY = getDouble("--decay", 0.01)
  def PV_SIZE = getInt("--pv.size", -1)
  def PV_SET = getDouble("--pv.set", 0.0)
  def PV_SET_RANGE = getRange("--pv.set.range", (0,0))
  def TRAIN_ITERATIONS = getInt("--train.iterations", 10)
  def TRAIN_ITERATIONS_OFFSET = getInt("--train.iterations.offset", 0)
  def TRAIN_ORDER = getString("--train.order", "NORMAL")
  def MODEL_OUTPUT_FILE = getString("--model.output.file", "model")
  def INIT_FILE = getString("--init.file")
  def BATCH_SIZE = getInt("--batch.size", 1)
  def AVERAGE_BATCH = getBoolean("--average.batch", false)
  def AVERAGE_LAST = getBoolean("--average.last", false)
  def TIME = getBoolean("--time", false)

  def DAMP_INIT = getDouble("--damp.init", 1.0)
  def DAMP_RATE = getDouble("--damp.rate", 0.001)
  def DIFF_THRESHOLD = getDouble("--diff.threshold", 0.001)
  def INFERENCE_ITERATIONS = getInt("--inference.iterations", 50)
  def VERBOSE = getBoolean("--verbose", false)
  def CHECK_FOR_NAN = getBoolean("--check.nan", true)
  def PRINT_GRAPH = getBoolean("--print.graph", false)
  def PRINT_TRAIN_ACCURACY = getBoolean("--print.train.accuracy", false)
  def PRINT_DEV_ACCURACY = getBoolean("--print.dev.accuracy", false)
  def DEV_DATA_FILE = getString("--dev.data")

  def CONCURRENCY = getString("--concurrency", "SERIAL")
  def MARGINALIZATION = getBoolean("--marg", false)

  def GROUP1_REG = getDouble("--group.reg.1", 1.0)
  def GROUP2_REG = getDouble("--group.reg.2", 1.0)
  def GROUP3_REG = getDouble("--group.reg.3", 1.0)

}
