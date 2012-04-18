package narad.util
import edu.stanford.nlp.trees._
import edu.stanford.nlp.parser.lexparser.{LexicalizedParser, Options}
import edu.stanford.nlp.trees.international.pennchinese.ChineseGrammaticalStructure
import narad.nlp.parse.{Tree => STree, TreeFactory => STreeFactory}
import scala.collection.JavaConversions._ 

class StanfordParserWrapper(model: String, options: Options=null.asInstanceOf[Options]) {
	val lexPattern = """(.*)\[.*\]""".r
	val depPattern = """(.*)\(.*\-([0-9]+),.*\-([0-9]+)\)""".r
	val parser = if (options == null) {
	 LexicalizedParser.getParserFromSerializedFile(model)
	}
	else {
		LexicalizedParser.getParserFromFile(model, options)		
	}
	

	def constituencyParse(str: String): Option[STree] = {
		try{
			var tree = parser.apply(str)
			return Some(toScalaTree(tree))
		}
		catch{ 
			case e: Exception => System.err.println("Error parsing <%s>".format(str)); return None
		}
	}
	
	def dependencyParse(str: String): Option[Array[Int]] = {
		try{
			var tree = parser.apply(str)
			val slen = str.split(" ").size
			val heads = new Array[Int](slen+1)
			for (i <- 0 until heads.size) { heads(i) = -1 }
			val dependencies = (new EnglishGrammaticalStructure(tree)).typedDependenciesCollapsed.toArray.map(_.toString())
			for (dep <- dependencies) {
				dep match {
					case depPattern(label, head, dep) => heads(dep.toInt) = head.toInt
					case _ =>
				}
			}
			return Some(heads)
		}
		catch{ 
			case e: Exception => System.err.println("Error parsing <%s>".format(str)); return None
		}
	}

	def parses(str: String, language: String = "ENGLISH"): Option[(STree, Array[Int])] = {
		try{
			var tree = parser.apply(str)
			val slen = str.split(" ").size
			val heads = new Array[Int](slen+1)
			for (i <- 0 until heads.size) { heads(i) = -1 }
			val dependencies = language match {
				case "ENGLISH" => (new EnglishGrammaticalStructure(tree)).typedDependenciesCollapsed.toArray.map(_.toString())
				case "CHINESE" => (new ChineseGrammaticalStructure(tree)).typedDependenciesCollapsed.toArray.map(_.toString())
			}
			for (dep <- dependencies) {
				dep match {
					case depPattern(label, head, dep) => heads(dep.toInt) = head.toInt
					case _ =>
				}
			}
			return Some(Tuple(toScalaTree(tree), heads))
		}
		catch{ 
			case e: Exception => System.err.println("Error parsing <%s>:\n%s\n".format(str, e.getStackTrace.mkString("\n"))); return None
		}
	}		
	
	def toScalaTree(tree: Tree, removeLex: Boolean = true): STree = tree match {
		case t if t.isPreTerminal => {
			val tag = removeLexicalization(t.label.toString)
			val word = t.children.first.label.toString
			return STreeFactory.buildTree(tag, word)			
		}
		case _=> {
			val label = if (removeLex) removeLexicalization(tree.label.toString) else tree.label.toString
			return STreeFactory.buildTree(label, children= tree.children.map(toScalaTree(_)))
		}
	}
	
	//NN[boy/NN]
	def removeLexicalization(str: String): String = {
		str match {
			case lexPattern(label) => return label
			case _=> return str
		}
	}
}

//		println(t); println(t.label.getClass); 

object StanfordParserWrapper {

	def main(args: Array[String]) = {
		val options = new ArgParser(args)
		val model  = options.getString("--parser")
		val input = options.getString("--input")
		val mode  = options.getString("--mode", "constituency")
		val parser = new StanfordParserWrapper(model)
		if (input == null) { // Debug
			val test = "The boy saw the man with the telescope ."
			println("Parsing sentence <%s>".format(test))
			parser.constituencyParse(test) match {
				case Some(tree) => println("Constituency Tree:"); println(tree)
				case _=> 
			}
			parser.dependencyParse(test) match {
				case Some(tree) => println("Dependency Tree:"); println(tree.mkString(" "))
				case _=> 
			}			
		}
		else {
			for (line <- io.Source.fromFile(input).getLines) {
				parser.constituencyParse(line) match {
					case Some(tree) => println(tree.toString.replaceAll("\\\\\\/", "/"))
					case _ =>
				}
			}
		}
	}
}







//			val dependencies = (new EnglishGrammaticalStructure(tree)).typedDependenciesCollapsed.toArray.map(_.toString())
//			return (0 until slen).map { wi =>
	//				dependencies.filter{ rel => rel.contains("-%d)".format(wi+1)) || rel.contains("-%d,".format(wi+1)) }
	//			}.toArray
