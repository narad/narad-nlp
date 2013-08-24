package narad.nlp.trees

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 7/6/13
 * Time: 11:59 AM
 */
abstract class ConstituentNode(val label: String) {

  def isNonterminal: Boolean = this match {
    case x: NonterminalNode => true
    case _ => false
  }

  def isPreterminal: Boolean = this match {
    case x: PreterminalNode => true
    case _ => false
  }
}

case class NonterminalNode(override val label: String) extends ConstituentNode(label) {

  override def isNonterminal = true

  override def isPreterminal = false
}

case class PreterminalNode(override val label: String, word: String) extends ConstituentNode(label) {

  override def isNonterminal = false

  override def isPreterminal = true
}
