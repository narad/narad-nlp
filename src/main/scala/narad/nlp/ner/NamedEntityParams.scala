package narad.nlp.ner

import narad.util.ArgParser
import narad.bp.structure.ModelOptions
import narad.bp.optimize.OptimizerOptions
import narad.nlp.parser.constituent.ConstituentParserParams

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 7/20/13
 * Time: 1:57 AM
 */
class NamedEntityParams(args: Array[String]) extends ConstituentParserParams(args) { //ArgParser(args) with ModelOptions with OptimizerOptions {

  def LABEL_TYPE = getString("--label.type", "FINE")

  def TRAIN_NER_FILE = getString("--train.ner.file")
  def TEST_NER_FILE  = getString("--test.ner.file")
  def TRAIN_SYNTAX_FILE = getString("--train.syntax.file")
  def TEST_SYNTAX_FILE  = getString("--test.syntax.file")
  def MAX_SEG = getInt("--max.seg", 10)
  def TRAIN_MODE = getString("--train.mode", "STANDARD")
  def CONNECT_NP = getBoolean("--connect.np", false)
  def BINARIZE = getBoolean("--binarize", true)
//  def BINARIZE_MODE = getString("--binarize.mode", "RIGHT")
  def FEATURE_MODE = getString("--feature.mode", "SPAN")

  def OUTPUT_NER_FILE = getString("--output.ner.file")
  def OUTPUT_SYNTAX_FILE = getString("--output.syntax.file")

  override lazy val MARGINALIZATION = getBoolean("--marginalization", false)
}











/*  def PRINT_INTERVAL = getInt("--print.interval", 100)
def TRAIN_FIDX_FILE = getString("--train.fidx.file", "train.fidx")
def TEST_FIDX_FILE  = getString("--test.fidx.file", "test.fidx")
def TRAIN_FEATURE_FILE = getString("--train.feature.file", "train.feats")
def TEST_FEATURE_FILE = getString("--test.feature.file", "test.feats")
def EXTRACT_FEATURES = getBoolean("--extract.features")
def TRAIN = getBoolean("--train")
def TEST = getBoolean("--test")
def HASH_DICT = getBoolean("--hash.dict")
def VARIANCE = getDouble("--variance", 1.0)
def RATE = getDouble("--rate", .01)
def PV_SIZE = getInt("--pv.size", -1)
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
def DAMP_RATE = getDouble("--damp.rate", 0.01)
def DIFF_THRESHOLD = getDouble("--diff.threshold", 0.001)
def INFERENCE_ITERATIONS = getInt("--inference.iterations", 10)
def VERBOSE = getBoolean("--verbose", false)
def CHECK_FOR_NAN = getBoolean("--check.nan", true)
def PRINT_GRAPH = getBoolean("--print.graph", false)
def PRINT_TRAIN_ACCURACY = getBoolean("--print.train.accuracy", false)
def PRINT_DEV_ACCURACY = getBoolean("--print.dev.accuracy", false)
def DEV_DATA_FILE = getString("--dev.data")
def OUTPUT_FILE = getString("--output.file")
def MODEL = getString("--model", "STANDARD")

def GROUP1_REG = getDouble("--group.reg.1", 1.0)
def GROUP2_REG = getDouble("--group.reg.2", 1.0)
def GROUP3_REG = getDouble("--group.reg.3", 1.0)
*/
