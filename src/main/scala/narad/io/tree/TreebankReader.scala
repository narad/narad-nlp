package narad.io.tree

import java.io._
import narad.util._
import narad.nlp.trees._
import scala.collection.mutable.ArrayBuffer
import narad.io.util.DirectoryReader


class TreebankReader(filename: String, options: TreebankReaderOptions = new DefaultTreebankReaderOptions) extends Iterable[ConstituentTree] with StringToTreeOps {

  def iterator: Iterator[ConstituentTree] = {
    val transformer = new TreeTransformer(options)
    val text = "(DOC %s)".format(io.Source.fromFile(filename).getLines().filter(!_.startsWith("*")).mkString)
    val t = stringToTree(text, defaultLabel=options.DEFAULT_LABEL, coarseLabels=options.COARSEN_LABELS)
    t.getChildren.map(transformer.transformTree(_)).iterator
  }
}

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


object TreebankReader {

  def main(args: Array[String]) {
    val options = new ArgParser(args)
    val treebankFile = options.getString("--treebank")
    val sort = options.getBoolean("--sort", false)
    val min = options.getInt("--min", 0)
    val max = options.getInt("--max", 100)
    val reader = if (sort) {
      read(treebankFile, options).toList.sortBy(_.length)
    }
    else {
      read(treebankFile, options)
    }
    if (options.getBoolean("--print.sentences")) {
      for (t <- reader if t.length >= min && t.length <= max) {
        println(t.words.mkString(" "))
      }
    }
    else {
      for (t <- reader if t.length >= min && t.length <= max) {
        println(t.toString())
      }
    }
  }

  def read(filename: String, options: ArgParser): Iterator[ConstituentTree] = {
    new TreebankReader(filename, TreebankReaderOptions.fromCommandLine(options)).iterator
  }
}

class WSJReader(dir: String) {

  def getTrees(): Array[ConstituentTree] = {
    val ab = new ArrayBuffer[ConstituentTree]()
    val freader = new DirectoryReader()
    for (file <- freader.listAllFiles(new File(dir), ".mrg")) {
      val treader = new TreebankReader(file.toString)
      ab ++= treader.iterator
    }
    return ab.toArray
  }
}











/*

def coarsen(ll: String): String = {
  if (ll == "-DFL-") return ll
  var l = ll
  while (l.startsWith("^")) {
    l = l.substring(1)
  }
  if (l.contains("-"))
    l = l.substring(0, l.indexOf("-"))
  if (l.contains("="))
    l = l.substring(0, l.indexOf("="))
  if (l.contains("^"))
    l = l.substring(0, l.indexOf("^"))
  return l
}
*/
















/*
	val tokenPattern = """\(([^ \(]+) ([^ \)]+)\).*""".r
	val constPattern = """\(([^ ]+) .*""".r
	val emptyPattern = """\( .*""".r
		
	def iterator(filename: String, options: ArgParser, defaultLabel: String="TOP") = SexpReader.read(filename, parseExpression(_, options, defaultLabel=defaultLabel))

	def read(filename: String, options: ArgParser=new ArgParser(Array[String]())): Iterator[ConstituentTree] = {
    System.out.println("READING: %s".format(filename))
		val text = "(DOC %s)".format(io.Source.fromFile(filename).getLines().mkString)
		val defaultLabel	 = options.getString("--default.label", "TOP")
		val transformer = new TreeTransformer(options)
		val trees = parseExpression(text, defaultLabel=defaultLabel).children.map(transformer.transformTree(_))
//		println(trees.size)
		trees.iterator //, coarseTags).children
	}
	
	def coarsenLabel(label: String): String = {
		var l = label
		if (l.contains("-"))
		l = l.substring(0, l.indexOf("-"))
		if (l.contains("="))
		l = l.substring(0, l.indexOf("="))
		if (l.contains("^"))
		l = l.substring(0, l.indexOf("^"))
		return l
	}
	
	
	// TODO - implement transformations into tree iterator style util
	def parseExpression(str: String, options: ArgParser = null.asInstanceOf[ArgParser], defaultLabel: String = "TOP"): ConstituentTree = {
		val coarseTags = true
		str match {
			case tokenPattern(tag, word) => ConstituentTreeFactory.buildTree(label=tag, word=word)
			case constPattern(label) => {
				val children = subexpressions(str).map(parseExpression(_, defaultLabel=defaultLabel)).toArray
				if (coarseTags)
				return ConstituentTreeFactory.buildTree(label=coarsenLabel(label), children=children)
				//					return transformTree(ConstituentTreeFactory.buildTree(label=coarsenLabel(label), children=children), options)
				else
				//					return transformTree(ConstituentTreeFactory.buildTree(label=label, children=children), options)
				return ConstituentTreeFactory.buildTree(label=label, children=children)
			}
			case emptyPattern() => {
				val children = subexpressions(str).map(parseExpression(_, defaultLabel=defaultLabel)).toArray				
				return ConstituentTreeFactory.buildTree(label=defaultLabel, children=children)
			}
			case entry => {
				if (str != null)
				System.err.println("Not recognized: %s".format(str))
				return null.asInstanceOf[ConstituentTree]
			}
		}
	}
	
	def subexpressions(str: String, ldelim: Char='(', rdelim: Char=')'): Array[String] = {
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
		subs.toArray
	}
}
            */