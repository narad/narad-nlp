package narad.nlp.srl

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 8/14/13
 * Time: 11:35 AM
 */
case class SRLFrame(pidx: Int, sense: String, args: Array[SRLArg]) {

  def countLabel(label: String): Int = args.filter(_.label == label).size

  def removeLabel = new SRLFrame(pidx, sense, args.map(_.removeLabel))

  override def toString = "%s: %s".format(sense, args.mkString(", "))
}

case class SRLArg(aidx: Int, word: String, label: String) {

  def removeLabel = SRLArg(aidx, word, "")

  override def toString = "%d-%s".format(aidx, label)
}

case class SRLToken(word: String, lemma: String, pos: String, cpos: String, morph: String = "") {
  override def toString = "(%s %s)".format(pos, word)
}