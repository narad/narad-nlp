package narad.nlp.trees

import scala.collection.mutable.ArrayBuffer
import narad.io.tree.StringToTreeOps

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 2/22/13
 * Time: 11:05 AM
 * To change this template use File | Settings | File Templates.
 */
class HeadFinder {
  val rules = loadRules


  def headOf(t: ConstituentTree): String = {
    if (t.isPreterminal) return t.asInstanceOf[PreterminalNode].word
    if (t.label == "NP") {
      val tokens = t.tokens.toArray
      // If the last word is tagged POS, return it
      if (tokens.last.pos == "POS") return tokens.last.word
      // Else search right->left for the first NN/NNP/NNPS/NNS/NX/POS/JJR
      val set1 = Array("NN", "NNP", "NNPS", "NNS", "NX", "POS", "JJR")
      tokens.reverse.foreach(tt => if (set1.contains(tt.pos)) return tt.word)
      // Else search left->right for the first NP
      t.getChildren.foreach(st => if (st.label == "NP") return headOf(st)) //st.label())
      // ELse search right->left for the first child which is ADJP or PRN
      val set2 = Array("ADJP", "PRN")
      t.getChildren.reverse.foreach(st => if (set2.contains(st.label)) return headOf(st))
      // Else search right-> left for the first child which is CD
      t.getChildren.reverse.foreach(st => if (st.label == "CD") return headOf(st))
      // Else search right->left for the first child which is JJ, JJS, RB, or QP
      val set3 = Array("JJ", "JJS", "RB", "QP")
      t.getChildren.reverse.foreach(st => if (set3.contains(st.label)) return headOf(st))
    }
    val rule = if (rules.exists(_.constituent == t.label)) {
      rules.filter(_.constituent == t.label).head
    }
    else {
      new HeadFindingRule("DEFAULT", "right", Array())
    }

    if (rule.priority == "POS") {
      var orderedChildren = if (rule.direction == "left") t.getChildren else t.getChildren.reverse
      for (c <- orderedChildren) {
        for (r <- rule.candidates) {
          if (c.label == r) return headOf(c)
        }
      }
    }
    for (r <- rule.candidates) {
      for (c <- t.getChildren) {
        if (c.label == r) return headOf(c)
      }
    }
    if (rule.direction == "left") {
        return headOf(t.getChildren.head)
    }
    else { //if (rule.direction == "right")
     return headOf(t.getChildren.reverse.head)
    }
  }


  def headDistribution(t: ConstituentTree) = {

  }

  def loadRules: Array[HeadFindingRule] = {
    val rules = new ArrayBuffer[HeadFindingRule]
    rules += new HeadFindingRule("ADJP", "left", Array("NNS", "QP", "NN", "$", "ADVP", "JJ", "VBN", "VBG", "ADJP", "JJR", "NP", "JJS", "DT", "FW", "RBR", "RBS", "SBAR", "RB"))
    rules += new HeadFindingRule("ADVP", "right", Array("RB", "RBR", "RBS", "FW", "ADVP", "TO", "CD", "JJR", "JJ", "IN", "NP", "JJS", "NN"))
    rules += new HeadFindingRule("CONJP", "right", Array("CC", "RB", "IN"))
    rules += new HeadFindingRule("FRAG", "right", Array())
    rules += new HeadFindingRule("INTJ", "left", Array())
    rules += new HeadFindingRule("LST", "right", Array("LS", ":"))
    rules += new HeadFindingRule("NAC", "left", Array( "NN", "NNS", "NNP", "NNPS", "NP", "NAC", "EX", "$", "CD", "QP", "PRP", "VBG", "JJ", "JJS", "JJR", "ADJP", "FW"))
    rules += new HeadFindingRule("NX", "left", Array())
    rules += new HeadFindingRule("PP", "right", Array("IN", "TO", "VBG", "VBN", "RP", "FW"))
    rules += new HeadFindingRule("PRN", "left", Array())
    rules += new HeadFindingRule("PRT", "right", Array("RP"))
    rules += new HeadFindingRule("QP", "left", Array("$", "IN", "NNS", "NN", "JJ", "RB", "DT", "CD", "NCD", "QP", "JJR", "JJS"))
    rules += new HeadFindingRule("RRC", "right", Array("VP", "NP", "ADVP", "ADJP", "PP"))
    rules += new HeadFindingRule("S", "left", Array("TO", "IN", "VP", "S", "SBAR", "ADJP", "UCP", "NP"))
    rules += new HeadFindingRule("SBAR", "left", Array( "WHNP", "WHPP", "WHADVP", "WHADJP", "IN", "DT", "S", "SQ", "SINV", "SBAR", "FRAG"))
    rules += new HeadFindingRule("SBARQ", "left", Array("SQ", "S", "SINV", "SBARQ", "FRAG"))
    rules += new HeadFindingRule("SINV", "left", Array("VBZ", "VBD", "VBP", "VB", "MD", "VP", "S", "SINV", "ADJP", "NP"))
    rules += new HeadFindingRule("SQ", "left", Array("VBZ", "VBD", "VBP", "VB", "MD", "VP", "SQ"))
    rules += new HeadFindingRule("UCP", "right", Array())
    rules += new HeadFindingRule("VP", "left", Array("TO", "VBD", "VBN", "MD", "VBZ", "VB", "VBG", "VBP", "AUX", "AUXG", "VP", "ADJP", "NN", "NNS", "NP"))
    rules += new HeadFindingRule("WHADJP", "left", Array( "CC", "WRB", "JJ", "ADJP"))
    rules += new HeadFindingRule("WHADVP", "right", Array("CC", "WRB"))
    rules += new HeadFindingRule("WHNP", "left", Array("WDT", "WP", "WP$", "WHADJP", "WHPP", "WHNP"))
    rules += new HeadFindingRule("WHPP", "right", Array("IN", "TO", "FW"))
    rules += new HeadFindingRule("X", "right", Array())
    rules += new HeadFindingRule("TYPO", "left", Array())

//    rules += new HeadFindingRule("NP")
    rules.toArray
  }

}

/*                 nonTerminalInfo.put("NP", new String[][] {
{ "rightdis", "NN", "NNP", "NNPS", "NNS", "NX", "POS", "JJR" },
{ "left", "NP" }, { "rightdis", "$", "ADJP", "PRN" },
{ "right", "CD" }, { "rightdis", "JJ", "JJS", "RB", "QP" } });
*/

case class HeadFindingRule(constituent: String, direction: String, candidates: Array[String], priority: String = "CAT")

object HeadFinder extends StringToTreeOps {

  def main(args: Array[String]) {
    val treeStr = "((S (NP (DT the) (JJ quick) (JJ (AA (BB (CC brown)))) (NN fox)) (VP (VBD jumped) (PP (IN over) (NP (DT the) (JJ lazy) (NN dog)))) (. .)))"
    val tree = stringToTree(treeStr)
    val hf = new HeadFinder
    println(hf.headOf(tree))
  }
}

  /*
// should prefer JJ? (PP (JJ such) (IN as) (NP (NN crocidolite)))

nonTerminalInfo.put("NP", new String[][] {
{ "rightdis", "NN", "NNP", "NNPS", "NNS", "NX", "POS", "JJR" },
{ "left", "NP" }, { "rightdis", "$", "ADJP", "PRN" },
{ "right", "CD" }, { "rightdis", "JJ", "JJS", "RB", "QP" } });
nonTerminalInfo.put("TYPO", new String[][] { { "left" } }); // another

nathan -
Today's discovery: for coordinate structures of the form "V1, V2 and ADV V3", the Stanford head rules incorrectly have the adverb modifying the head of the entire conjoined phrase (V1) when it should modify V3.


                                                "((S (NP (DT the) (JJ quick) (JJ (AA (BB (CC brown)))) (NN fox)) (VP (VBD jumped) (PP (IN over) (NP (DT the) (JJ lazy) (NN dog)))) (. .)))"));

  */



/*

Implements a variant on the HeadFinder found in Michael Collins' 1999 thesis. This starts with Collins' head finder. As in CollinsHeadFinder.java, we've added a head rule for NX. Changes:

The PRN rule used to just take the leftmost thing, we now have it choose the leftmost lexical category (not the common punctuation etc.)
Delete IN as a possible head of S, and add FRAG (low priority)
Place NN before QP in ADJP head rules (more to do for ADJP!)
Place PDT before RB and after CD in QP rules. Also prefer CD to DT or RB. And DT to RB.
Add DT, WDT as low priority choice for head of NP. Add PRP before PRN Add RBR as low priority choice of head for NP.
Prefer NP or NX as head of NX, and otherwise default to rightmost not leftmost (NP-like headedness)
VP: add JJ and NNP as low priority heads (many tagging errors) Place JJ above NP in priority, as it is to be preferred to NP object.
PP: add PP as a possible head (rare conjunctions)
Added rule for POSSP (can be introduced by parser)
Added a sensible-ish rule for X.
Added NML head rules, which are the same as for NP.
NP head rule: NP and NML are treated almost identically (NP has precedence)
NAC head rule: NML comes after NN/NNS but after NNP/NNPS
PP head rule: JJ added
Added JJP (appearing in David Vadas's annotation), which seems to play the same role as ADJP.
These rules are suitable for the Penn Treebank.
A case that you apparently just can't handle well in this framework is (NP (NP ... NP)). If this is a conjunction, apposition or similar, then the leftmost NP is the head, but if the first is a measure phrase like (NP $ 38) (NP a share) then the second should probably be the head.

*/