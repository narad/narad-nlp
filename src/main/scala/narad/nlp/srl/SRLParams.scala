package narad.nlp.srl
import narad.nlp.parser.constituent.ConstituentParserParams
import narad.nlp.parser.dependency.DependencyParserParams

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 12/30/12
 * Time: 10:29 AM
 * To change this template use File | Settings | File Templates.
 */
class SRLParams(args: Array[String]) extends DependencyParserParams(args){

  lazy val MODEL = getString("--model", "BASELINE")

  override lazy val MARGINALIZATION = getBoolean("--marg", false)


  lazy val CONNECTION_ORDER = getString("--connection.order", "1")  // 1, 2, 1+2 supported

  lazy val PRUNED_ROLE_LABEL = getString("--pruned.role.label", "A-PRUNED")

  lazy val UNSEEN_SENSE_PROTOCOL = getString("--unseen.sense.protocol", "LEMMA_PLUS_1")

  lazy val MODEL_ARGS = getBoolean("--model.args", true)

  lazy val MODEL_ROLES = getBoolean("--model.roles", true)

  // Valency Options

  lazy val MODEL_ARG_VALENCY = getBoolean("--model.arg.valency", false)

  lazy val MODEL_ROLE_VALENCY = getBoolean("--model.role.valency", false)

  lazy val NUM_VALENCY_ARGS = getInt("--num.valency.args", 5)

  lazy val NUM_VALENCY_ROLES = getInt("--num.valency.roles", 3)

  // Pruning Options

  lazy val PRUNE_ARGS_BY_DIST = getBoolean("--prune.args.by.dist", false)

  lazy val PRUNE_ARGS_BY_TAG = getBoolean("--prune.args.by.tag", false)

  lazy val PRUNE_TAG_THRESHOLD = getInt("--prune.tag.threshold", 500)

  lazy val MAX_DIST = getInt("--max.dist", 100)

  lazy val PRUNE_ROLES_BY_COUNT = getBoolean("--prune.roles")

  lazy val ROLE_PRUNE_THRESHOLD = getInt("--prune.role.threshold", 50)

}






//  lazy val PRUNE_ROLES_BY_SENSE = getBoolean("--prune.roles.by.sense", false)
