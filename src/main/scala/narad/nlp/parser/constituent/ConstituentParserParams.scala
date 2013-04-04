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

    def MODEL = getString("--model", "LABEL")
    def FEATURE_WINDOW = getInt("--feature.window", 3)
    def MODE = getString("--mode", "FINE")
    def BRACK_NAME = getString("--brack.name", "brack")
    def BRACK_LABEL_NAME = getString("--brack.label.name", "spanLabel")
    def UNARY_NAME = getString("--unary.name", "unary")
    def UNARY_LABEL_NAME = getString("--unary.label.name", "unaryLabel")

    def PRINT_INTERVAL = getInt("--print.interval", 100)
    def TRAIN_FIDX_FILE = getString("--train.fidx.file", "train.fidx")
    def TEST_FIDX_FILE  = getString("--test.fidx.file", "test.fidx")
    def TRAIN_FEATURE_FILE = getString("--train.feature.file", "train.feats")
    def TEST_FEATURE_FILE = getString("--test.feature.file", "test.feats")
    def TRAIN_FILE = getString("--train.file")
    def TEST_FILE  = getString("--test.file")
    def OUTPUT_FILE = getString("--output.file")

    def TRAIN = getBoolean("--train")
    def TEST  = getBoolean("--test")
    def EXTRACT_FEATURES = getBoolean("--extract.features")

    def VARIANCE = getDouble("--variance", 1.0)
    def RATE = getDouble("--rate", .01)
    def PV_SIZE = getInt("--pv.size", -1)
    def TRAIN_ITERATIONS = getInt("--train.iterations", 10)
    def TRAIN_ORDER = getString("--train.order", "NORMAL")
    def MODEL_OUTPUT_FILE = getString("--model.output.file", "model")
    def INIT_FILE = getString("--init.file")
    def BATCH_SIZE = getInt("--batch.size", 1)
    def AVERAGE_LAST = getBoolean("--average.last", false)
    def TIME = getBoolean("--time", false)

    def DAMP_INIT = getDouble("--damp.init", 1.0)
    def DAMP_RATE = getDouble("--damp.rate", 0.01)
    def DIFF_THRESHOLD = getDouble("--diff.threshold", 0.001)
    def INFERENCE_ITERATIONS = getInt("--inference.iterations", 10)
    def VERBOSE = getBoolean("--verbose", false)

  def GROUP1_REG = getDouble("--group.reg.1", 1.0)
  def GROUP2_REG = getDouble("--group.reg.2", 1.0)
  def GROUP3_REG = getDouble("--group.reg.3", 1.0)


}