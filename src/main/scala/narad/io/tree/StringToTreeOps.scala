package narad.io.tree

import narad.util.ArgParser
import collection.mutable.ArrayBuffer
import narad.nlp.trees.{ConstituentTreeFactory, ConstituentTree}

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 5/1/14
 * Time: 12:22 PM
 */
trait StringToTreeOps {
  private val TOKEN_PATTERN = """\(([^ \(]+) ([^ \)]+)\).*""".r
  private val CONSTITUENT_PATTERN = """\(([^ ]+) .*""".r
  private val EMPTY_PATTERN = """\( .*""".r
  private val DOUBLE_PAREN_PATTERN = """\(\(+.*""".r

  def stringToTree(str: String, options: ArgParser = null.asInstanceOf[ArgParser], defaultLabel: String = "TOP", coarseLabels: Boolean = false): ConstituentTree = {
    str match {
      case DOUBLE_PAREN_PATTERN() => {
        val children = subexpressions(str).map(stringToTree(_, defaultLabel=defaultLabel))
        return ConstituentTreeFactory.buildTree(label=defaultLabel, children=children)
      }
      case TOKEN_PATTERN(tag, word) => {
        ConstituentTreeFactory.buildTree(label=tag, word=word)
      }
      case CONSTITUENT_PATTERN(label) => {
        val children = subexpressions(str).map(stringToTree(_, defaultLabel=defaultLabel))
        //        if (coarseLabels)
        //          return ConstituentTreeFactory.buildTree(label=coarsen(label), children=children)
        //        else
        return ConstituentTreeFactory.buildTree(label=label, children=children)
      }
      case EMPTY_PATTERN() => {
        val children = subexpressions(str).map(stringToTree(_, defaultLabel=defaultLabel))
        return ConstituentTreeFactory.buildTree(label=defaultLabel, children=children)
      }
      case _ => {
        if (str != null) System.err.println("Not recognized: %s".format(str))
        return null.asInstanceOf[ConstituentTree]
      }
    }
  }

  def subexpressions(str: String, ldelim: Char='(', rdelim: Char=')'): List[String] = {
    val subs = new ArrayBuffer[String]
    var count = -1; var start = 0
    str.zipWithIndex.foreach { case(letter, index) =>
      if (letter == ldelim) {
        count += 1
        if (count == 1)
          start = index
      }
      else if (letter == rdelim) {
        count -= 1
        if (count == 0)
          subs += str.substring(start, index+1)
      }
    }
    subs.toList
  }
}