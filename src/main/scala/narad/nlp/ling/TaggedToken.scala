package narad.nlp.ling

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 12/13/12
 * Time: 2:59 PM
 * To change this template use File | Settings | File Templates.
 */

case class Token(word: String) {}

class TaggedToken(word: String, tag: String) extends Token(word) with HasTag {

  override def pos = tag
}

trait HasTag {

  def pos: String
}