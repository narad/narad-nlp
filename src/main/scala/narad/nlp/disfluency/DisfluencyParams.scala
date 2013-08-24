package narad.nlp.disfluency

import narad.nlp.parser.constituent.ConstituentParserParams

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 3/23/13
 * Time: 11:55 PM
 * To change this template use File | Settings | File Templates.
 */
class DisfluencyParams(args: Array[String]) extends ConstituentParserParams(args) {
  def TRAIN_DISFLUENCY_FILE = getString("--train.disfluency.file")
  def TRAIN_SYNTAX_FILE     = getString("--train.syntax.file")
  def TEST_DISFLUENCY_FILE  = getString("--test.disfluency.file")
  def TEST_SYNTAX_FILE      = getString("--test.syntax.file")
  def ALIGNMENT_FILE        = getString("--alignment.file")
  def PREDICT_DISFLUENCY    = getBoolean("--predict.disfluency")
  def PREDICT_SYNTAX        = getBoolean("--predict.syntax")
  def PRUNE_SYNTAX         = getBoolean("--prune.syntax")
}