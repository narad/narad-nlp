package narad.nlp.tagger
import narad.util.ArgParser
import narad.bp.structure._
import narad.bp.optimize._

class TaggerParams(args: Array[String]) extends ArgParser(args) with ModelOptions with OptimizerOptions {
	
	def MODEL = getString("--model", "unigram")
	def FEATURE_MODE = getInt("--feature.mode", 1)
	def MODE = getString("--mode", "FINE")
  def COARSE_TO_FINE = getBoolean("--coarse.to.fine", false)
  def ORDER = getInt("--order", 1)

	def PRINT_INTERVAL = getInt("--print.interval", 100)
	def TRAIN_FIDX_FILE = getString("--train.fidx.file", "train.fidx")
	def TEST_FIDX_FILE  = getString("--test.fidx.file", "test.fidx")
	def TRAIN_FEATURE_FILE = getString("--train.feature.file", "train.feats")
	def TEST_FEATURE_FILE = getString("--test.feature.file", "test.feats")
	def TRAIN_FILE = getString("--train.file")
	def TEST_FILE  = getString("--test.file")
  def FEATURE_BATCH_SIZE = getInt("--feature.batch.size", 1)
  def HASH = getBoolean("--hash")
  def TRAIN_AND_TEST_DICT = getBoolean("--train.and.test.dictionary", false)

  def TRAIN = getBoolean("--train")
  def TEST = getBoolean("--test")
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
  def GROUP_REGULARIZER = getBoolean("--group.regularization")

  def DAMP_INIT = getDouble("--damp.init", 1.0)
	def DAMP_RATE = getDouble("--damp.rate", 0.001)
	def DIFF_THRESHOLD = getDouble("--diff.threshold", 0.001)
	def INFERENCE_ITERATIONS = getInt("--inference.iterations", 10)
	def VERBOSE = getBoolean("--verbose", false)

  def GROUP1_REG = getDouble("--group.reg.1", 1.0)
  def GROUP2_REG = getDouble("--group.reg.2", 1.0)
  def GROUP3_REG = getDouble("--group.reg.3", 1.0)


}