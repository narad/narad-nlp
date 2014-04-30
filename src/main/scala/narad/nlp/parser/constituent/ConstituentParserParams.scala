package narad.nlp.parser.constituent

import narad.util.ArgParser
import narad.bp.structure.ModelOptions
import narad.bp.optimize.OptimizerOptions

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 12/16/12
 * Time: 4:26 AM
 * To change this template use File | Settings | File Templates.
 */
class ConstituentParserParams(args: Array[String]) extends ArgParser(args) with ModelOptions with OptimizerOptions {

  lazy val MODEL = getString("--model", "LABEL")
  lazy val FEATURE_WINDOW = getInt("--feature.window", 3)
  lazy val MODE = getString("--mode", "FINE")
  lazy val BRACK_NAME = getString("--brack.name", "brack")
  lazy val BRACK_LABEL_NAME = getString("--brack.label.name", "spanLabel")
  lazy val UNARY_NAME = getString("--unary.name", "unary")
  lazy val UNARY_LABEL_NAME = getString("--unary.label.name", "unaryLabel")
  lazy val PREDICT_LABELS = getBoolean("--predict.labels")
  lazy val PREDICT_UNARIES = getBoolean("--predict.unaries")
  lazy val MIN_PARSE_LEN = getInt("--min.parse.len", 3)  // Below this length the CKY factor is never used, only unary-span prediction
  lazy val PRUNE = getBoolean("--prune", false)
  lazy val UNBINARIZE = getBoolean("--unbinarize", true)
  lazy val INTEGERIZE = getBoolean("--integerize", false)
  lazy val BINARIZE_MODE = getString("--binarize.mode", "RIGHT_SINGLE")

  lazy val NUM_CORES = getInt("--num.cores", 1)
  lazy val GRAMMAR_FILE = getString("--grammar.file")
  lazy val GRAMMAR_TRAIN_ITERATIONS = getInt("--grammar.train.iterations", 10)
  lazy val GRAMMAR_LEARN_RATE = getDouble("--grammar.learn.rate", .03)
  lazy val MAX_GRAMMAR_SIZE = getInt("--max.grammar.size", 1000000)
  lazy val TREE_FACTOR = getBoolean("--tree.factor", true)

  lazy val PRINT_INTERVAL = getInt("--print.interval", 100)
  lazy val TRAIN_FIDX_FILE = getString("--train.fidx.file", "train.fidx")
  lazy val TEST_FIDX_FILE  = getString("--test.fidx.file", "test.fidx")
  lazy val TRAIN_FEATURE_FILE = getString("--train.feature.file", "train.feats")
  lazy val TEST_FEATURE_FILE = getString("--test.feature.file", "test.feats")
  lazy val TRAIN_FILE = getString("--train.file")
  lazy val TEST_FILE  = getString("--test.file")
  lazy val OUTPUT_FILE = getString("--output.file")
  lazy val HASH_DICT = getBoolean("--hash.dict")
  lazy val CONCURRENCY = getString("--concurrency", "SERIAL")
  def MARGINALIZATION = getBoolean("--marginalization", false)


  lazy val TRAIN = getBoolean("--train")
  lazy val TEST  = getBoolean("--test")
  lazy val EXTRACT_FEATURES = getBoolean("--extract.features")

  lazy val VARIANCE = getDouble("--variance", 1.0)
  lazy val RATE = getDouble("--rate", .01)
  lazy val DECAY = getDouble("--decay", 0.01)
  lazy val PV_SIZE = getInt("--pv.size", -1)
  def PV_SET = getDouble("--pv.set", 0.0)
  def PV_SET_RANGE = getRange("--pv.set.range", (0,0))
  lazy val TRAIN_ITERATIONS = getInt("--train.iterations", 10)
  lazy val TRAIN_ITERATIONS_OFFSET = getInt("--train.iterations.offset", 0)
  lazy val TRAIN_ORDER = getString("--train.order", "NORMAL")
  lazy val MODEL_OUTPUT_FILE = getString("--model.output.file", "model")
  lazy val INIT_FILE = getString("--init.file")
  lazy val BATCH_SIZE = getInt("--batch.size", 1)
  lazy val AVERAGE_BATCH = getBoolean("--average.batch", false)
  lazy val AVERAGE_LAST = getBoolean("--average.last", false)
  lazy val TIME = getBoolean("--time", false)

  lazy val DAMP_INIT = getDouble("--damp.init", 1.0)
  lazy val DAMP_RATE = getDouble("--damp.rate", 0.01)
  lazy val DIFF_THRESHOLD = getDouble("--diff.threshold", 0.001)
  lazy val INFERENCE_ITERATIONS = getInt("--inference.iterations", 40)
  lazy val VERBOSE = getBoolean("--verbose", false)
  lazy val CHECK_FOR_NAN = getBoolean("--check.nan", true)
  lazy val PRINT_GRAPH = getBoolean("--print.graph", false)
  lazy val PRINT_TRAIN_ACCURACY = getBoolean("--print.train.accuracy", false)
  lazy val PRINT_DEV_ACCURACY = getBoolean("--print.dev.accuracy", false)
  lazy val DEV_DATA_FILE = getString("--dev.data")

  lazy val GROUP1_REG = getDouble("--group.reg.1", 1.0)
  lazy val GROUP2_REG = getDouble("--group.reg.2", 1.0)
  lazy val GROUP3_REG = getDouble("--group.reg.3", 1.0)
}
