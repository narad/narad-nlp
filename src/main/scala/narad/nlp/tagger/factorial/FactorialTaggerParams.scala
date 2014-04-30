package narad.nlp.tagger.factorial
import narad.nlp.tagger._

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 1/17/14
 * Time: 10:42 AM
 */
class FactorialTaggerParams(args: Array[String]) extends TaggerParams(args) {
  def NUM_CHAINS = getInt("--num.chains", 1)
  def LABEL_NAME = getString("--label.name", "label")
  def CHAIN_NAME = getString("--chain.name", "chain")
  def CHAIN_ORDER = getInt("--chain.order", 1)
  def SLICE_NAME = getString("--slice.name", "slice")
  def SLICE_DEPENDENCIES = getString("--slice.dependencies", "")
}



