package narad.nlp.srl
import narad.nlp.parser.constituent.ConstituentParserParams

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 12/30/12
 * Time: 10:29 AM
 * To change this template use File | Settings | File Templates.
 */
class SRLParams(args: Array[String]) extends ConstituentParserParams(args){

  override def MODEL = getString("--model", "BASELINE")

  def PRUNED_ROLE_LABEL = getString("--pruned.role.label", "A-PRUNED")

  def UNSEEN_SENSE_PROTOCOL = getString("--unseen.sense.protocol", "LEMMA_PLUS_1")

  def MODEL_VALENCY = getBoolean("--model.valency", false)

  def MAX_DIST = getInt("--max.dist", 100)

}
