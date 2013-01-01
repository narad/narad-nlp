package narad.io.reader
import java.io._
import narad.util._
import narad.nlp.trees._
import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet}
import collection.mutable

class TreebankReader(filename: String, options: ArgParser = new ArgParser(Array[String]())) extends Iterable[ConstituentTree] {
  val tokenPattern = """\(([^ \(]+) ([^ \)]+)\).*""".r
  val constPattern = """\(([^ ]+) .*""".r
  val emptyPattern = """\( .*""".r

  def iterator: Iterator[ConstituentTree] = {
    val transformer = new TreeTransformer(options)
    val text = "(DOC %s)".format(io.Source.fromFile(filename).getLines().mkString)
    val defaultLabel = options.getString("--default.label", "TOP")
    parseExpression(text, defaultLabel=defaultLabel).children.map(transformer.transformTree(_)).iterator
  }

  // TODO - implement transformations into tree iterator style reader
  def parseExpression(str: String, options: ArgParser = null.asInstanceOf[ArgParser], defaultLabel: String = "TOP"): ConstituentTree = {
    val coarseTags = true
    str match {
      case tokenPattern(tag, word) => TreeFactory.buildTree(label=tag, word=word)
      case constPattern(label) => {
        val children = subexpressions(str).map(parseExpression(_, defaultLabel=defaultLabel)).toArray
        if (coarseTags)
          return TreeFactory.buildTree(label=coarsenLabel(label), children=children)
        //					return transformTree(TreeFactory.buildTree(label=coarsenLabel(label), children=children), options)
        else
        //					return transformTree(TreeFactory.buildTree(label=label, children=children), options)
          return TreeFactory.buildTree(label=label, children=children)
      }
      case emptyPattern() => {
        val children = subexpressions(str).map(parseExpression(_, defaultLabel=defaultLabel)).toArray
        return TreeFactory.buildTree(label=defaultLabel, children=children)
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
}


object TreebankReader {

  def read(filename: String, options: ArgParser): Iterator[ConstituentTree] = {
    val reader = new TreebankReader(filename, options)
    reader.iterator
  }
}


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
	
	
	// TODO - implement transformations into tree iterator style reader
	def parseExpression(str: String, options: ArgParser = null.asInstanceOf[ArgParser], defaultLabel: String = "TOP"): ConstituentTree = {
		val coarseTags = true
		str match {
			case tokenPattern(tag, word) => TreeFactory.buildTree(label=tag, word=word)
			case constPattern(label) => {
				val children = subexpressions(str).map(parseExpression(_, defaultLabel=defaultLabel)).toArray
				if (coarseTags)
				return TreeFactory.buildTree(label=coarsenLabel(label), children=children)
				//					return transformTree(TreeFactory.buildTree(label=coarsenLabel(label), children=children), options)
				else
				//					return transformTree(TreeFactory.buildTree(label=label, children=children), options)
				return TreeFactory.buildTree(label=label, children=children)
			}
			case emptyPattern() => {
				val children = subexpressions(str).map(parseExpression(_, defaultLabel=defaultLabel)).toArray				
				return TreeFactory.buildTree(label=defaultLabel, children=children)
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