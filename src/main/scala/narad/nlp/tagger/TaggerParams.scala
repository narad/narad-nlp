package narad.nlp.tagger
import narad.util.ArgParser
import narad.bp.structure._
import narad.bp.optimize._

class TaggerParams(args: Array[String]) extends ArgParser(args) with ModelOptions with OptimizerOptions {
	
	def ORDER = getString("--order", "unigram")
	def FEATURE_MODE = getString("--feature.mode", "mcdonald")
	def MODE = getString("--mode", "FINE")

	def PRINT_INTERVAL = getInt("--print.interval", 100)
	def TRAIN_FIDX_FILE = getString("--train.fidx.file", "train.fidx")
	def TEST_FIDX_FILE  = getString("--test.fidx.file", "test.fidx")
	def TRAIN_FEATURE_FILE = getString("--train.feature.file", "train.feats")
	def TEST_FEATURE_FILE = getString("--test.feature.file", "test.feats")
	def TRAIN_FILE = getString("--train.file")
	def TEST_FILE  = getString("--test.file")
	
	def VARIANCE = getDouble("--variance", 1.0)
	def RATE = getDouble("--rate", .01)
	def PV_SIZE = getInt("--pv.size", -1)
	def TRAIN_ITERATIONS = getInt("--train.iterations", 10)
	def TRAIN_ORDER = getString("--train.order", "NORMAL")
	def MODEL_OUTPUT_FILE = getString("--model.output.file", "model")
	def INIT_FILE = getString("--init.file", "model.pv")
	def BATCH_SIZE = getInt("--batch.size", 1)
	def AVERAGE_LAST = getBoolean("--average.last", false)
	
	def DAMP_INIT = getDouble("--damp.init", 1.0)
	def DAMP_RATE = getDouble("--damp.rate", 0.001)
	def DIFF_THRESHOLD = getDouble("--diff.threshold", 0.001)
	def INFERENCE_ITERATIONS = getInt("--inference.iterations", 10)
	def VERBOSE = getBoolean("--verbose", false)
	
}