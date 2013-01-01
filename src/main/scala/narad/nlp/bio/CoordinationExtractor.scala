package narad.nlp.bio
import util.parsing.combinator.syntactical.StandardTokenParsers
import util.parsing.combinator.Parsers
import util.parsing.input.{Position, Reader}
import collection.mutable.ArrayBuffer
import narad.util.ArgParser
import narad.nlp.trees.{ConstituentTree => ConstituencyTree}
import narad.io.reader.TreebankReader

/*

object BioReader {

	def main(args: Array[String]) = {
		val options = new ArgParser(args)
		val filename = options.getString("--xml.file")
		val iformat = options.getString("--input.format", "UTF-8")
		parseDocument(filename, iformat)
	}
	
	def parseDocument(filename: String, format: String): Seq[BioDatum] = {
		try {
			val iformat = "UTF-8"
			val src = scala.io.Source.fromFile(filename, iformat)
			val xml = scala.xml.parsing.ConstructingParser.fromSource(src, false).document()
			val sentences = (xml \\ "_").filter(_.label == "sentence").map(parseSentenceNode(_))
			src.close()
			return sentences
		}
		catch {
			case e: Exception => {
				System.err.println("Error in XML reading:\n" + e.getStackTrace.mkString("\n"))
				return Seq[BioDatum]()
			}
		}		
	}

  def parseSentenceNode(node: scala.xml.Node): BioDatum = {
		println("Parsing sentence:")
		val entities  = (node \ "entity").map(parseEntityNode(_))
		val relations = (node \ "pair").map(parseRelationNode(_))
		val ctreestr  = (node \ "sentenceanalyses" \ "bracketings" \ "bracketing").head.attribute("bracketing").getOrElse("").toString
		val ctree = TreebankReader.parseExpression(ctreestr)
		println("from xml: " + ctreestr)
		println("from treebankreader: " + ctree.toString)
		
		val deps = (node \ "sentenceanalyses" \ "parses" \ "parse" \ "dependency").map(parseDependencyNode(_))
//		val deptree = new LabeledDependencyTree(deps.map(_.))
		
		val tokens = (node \ "sentenceanalyses" \ "tokenizations" \ "tokenization" \ "token").map(parseTokenNode(_))
//		assert(tokens.size == deptree.size)

		println(entities.mkString("\n"))
		println(relations.mkString("\n"))
		println(deps.mkString("\n"))
		println()
		return BioDatum(tokens, entities, relations) //, ctree, deptree)
//		val ctree = new
//		println(ctree)
	}		
	
	def parseEntityNode(node: scala.xml.Node): BioEntity = {
		val eid = node.attribute("id").getOrElse("-1").toString
		val etext = node.attribute("text").getOrElse("-1").toString
		val etype = node.attribute("type").getOrElse("-1").toString
		return BioEntity(eid, etext, etype)
	}
	
	def parseRelationNode(node: scala.xml.Node): BioRelation = {
		val e1 = node.attribute("id").getOrElse("-1").toString
		val e2 = node.attribute("id").getOrElse("-1").toString
		val pid  = node.attribute("id").getOrElse("-1").toString
		val pint = node.attribute("id").getOrElse("-1").toString == "True"
		return BioRelation(e1, e2, pid, pint)
	}
	
	def parseDependencyNode(node: scala.xml.Node): BioDependency = {
		val d1 = node.attribute("t1").getOrElse("1_-1").toString.split("_")(1).toInt
		val d2 = node.attribute("t2").getOrElse("1_-1").toString.split("_")(1).toInt
		val dtype  = node.attribute("type").getOrElse("-1").toString
		return BioDependency(d1, d2, dtype)
	}
	
	def parseTokenNode(node: scala.xml.Node): BioToken = {
		val word = node.attribute("text").getOrElse("-1").toString
		val pos  = node.attribute("POS").getOrElse("-1").toString
		return BioToken(word, pos)
	}
}

case class BioToken(word: String, pos: String)

case class BioDependency(d1: Int, d2: Int, dtype: String)

case class BioRelation(eid1: String, eid2: String, rid: String, rint: Boolean)

case class BioEntity(eid: String, etext: String, etype: String)

case class BioDatum(tokens: Seq[BioToken], entities: Seq[BioEntity], relations: Seq[BioRelation]) 
//										ctree: ConstituencyTree, deptree: LabeledDependencyTree)

case class DependencyTree(heads: Array[Int])

case class LabeledDependencyTree(heads: Array[Int], labels: Array[Int])



         */



/*
 foreach { node =>
	if (node.label == "sentence") { 
		parseSentenceNode(node)
	} 
}

*/
	//			if (node.label == "TEXT") { tb += node.text; println(node.text.split("\n").size); println(node.text) }
	//; println(node.text.split("\n").size); println(node.text) }


/*
/**
* @author sriedel
*/
object CoordinationExtractor {

def extract(pair: GenePairMention, sentence: SentenceHelper): Option[ProteinProteinCoordination] = {
val head1 = pair.arg1().tokenHead()
val head2 = pair.arg2().tokenHead()
val deps = sentence.deps
val conjunctions = deps.filter(_.label().startsWith("conj"))
val parents1 = deps.filter(_.mod() == head1)
val parents2 = deps.filter(_.mod() == head2)
val cand1 = head1 +: parents1.map(_.head())
val cand2 = head2 +: parents2.map(_.head())
val cands = (cand1 ++ cand2).toSet

def validOtherMod(edge: NLP.DepEdge) = {
!cands(edge.mod()) && (edge.label().startsWith("ad"))
}

def findCoord(cand1: Seq[Int], cand2: Seq[Int]) = {
val conj12 = conjunctions.filter(d => cand1.exists(d.head() == _) && cand2.exists(d.mod() == _))
if (conj12.isEmpty) {
None
} else {
val tokens = sentence.tokens
val firstCoordArgument = conj12.head
val coordinationHead = firstCoordArgument.head()
val conjParents = deps.filter(_.mod() == firstCoordArgument.head())
val conjLabel = firstCoordArgument.label()
val dep = conjParents.headOption.map(_.label()).getOrElse("NO CONJ PARENT")
if (conjParents.isEmpty) {
Some(ProteinProteinCoordination("NO PARENT", dep, conjLabel, "NO OTHER MOD", coordinationHead))
} else {
val parentToken = tokens(conjParents.head.head())
val parentLemma = parentToken.lemma()
val otherModDeps = deps.filter(d => validOtherMod(d) && d.head() == parentToken.indexInSentence())
val otherModLemma = otherModDeps.headOption.map(d => d.label() + ":" + tokens(d.mod()).lemma()).getOrElse("NO OTHER MOD")
Some(ProteinProteinCoordination(parentLemma, dep, conjLabel, otherModLemma, coordinationHead))
}
}
}
val result = findCoord(cand1, cand2)
result
//if (result.isDefined) result else findCoord(cand2,cand1)
}

}

object SentenceCoordinationFinder {

trait HasHead {
def head: Int
}
case class Conj(head: Int) extends HasHead
case class Comma(head: Int) extends HasHead
case class Gene(gene: Prot.GeneMention) extends HasHead {
def head = gene.tokenHead()
}
case class Block(gene: Gene)
case class Coordination(blocks: Seq[Block]) {
def coordinatesPair(pair: Prot.GenePairMention) = {
blocks.exists(block1 => block1.gene.head == pair.arg1().tokenHead() &&
blocks.exists(block2 => block1 != block2 && block2.gene.head == pair.arg2().tokenHead()))
}
}
case class Space(from: Int, to: Int)

case class SeqReader[T](seq: Seq[T], index: Int = 0, prev: Seq[T] = Seq.empty) extends Reader[T] {
def first = seq.head
def atEnd = seq.size == 1
def rest = if (seq.size == 1) this else SeqReader(seq.drop(1), index + seq.head.toString.length() + 1, prev :+ seq.head)
def pos = new Position {
def line = 0
def column = index
protected def lineContents = (prev ++ seq).mkString(" ")
}
}

object Parser extends Parsers {

type Elem = Any
def matchParser[T](partial: PartialFunction[Elem, T]) = Parser(input => {
if (partial.isDefinedAt(input.first)) Success(partial(input.first), input.rest)
else Failure("Mismatch", input.rest)
})

def CONJ = matchParser({
case x@Conj(_) => x
})
def COMMA = matchParser({
case x@Comma(_) => x
})
def GENE = matchParser({
case x@Gene(_) => x
})
def BlockExpr = (GENE ~ COMMA) ^^ {
case gene ~ comma => Block(gene)
}
def BlockExprEnd = (CONJ ~ GENE) ^^ {
case _ ~ gene => Block(gene)
}

def Coord = (BlockExpr ~ rep1(BlockExpr) ~ BlockExprEnd) ^^ {
case start ~ list ~ end => Coordination(start +: list :+ end)
}


def Sentence = rep(
Coord
| COMMA
| CONJ
| GENE
)

def parse(reader: Reader[Any]): ParseResult[List[Coordination]] = {
phrase(Sentence)(reader).map(_.collect({
case c: Coordination => c
}))
}

def parse(seq: Seq[Any]): ParseResult[List[Coordination]] = {
parse(SeqReader(seq))
}

}

def findCoordinations(sentence: SentenceHelper,
geneMentions: Seq[Prot.GeneMention],
pairs: Seq[Prot.GenePairMention]) {

//serialize
val commas = sentence.tokens.filter(_.word() == ",").map(t => Comma(t.indexInSentence()))
val conjs = sentence.tokens.filter(_.tag() == "CC").map(t => Conj(t.indexInSentence()))
val genes = geneMentions.map(m => Gene(m))
val serial = (commas ++ conjs ++ genes).sortBy(_.head)
if (serial.isEmpty) return
//    val parsed = Parser.parse(serial)
ProtXMLLoader.log.println(sentence.tokens.map(_.word()).mkString(" "))
ProtXMLLoader.log.println(serial.mkString(" "))

var inCoordination = false
var endOfCoordination = false
val coordinations = new ArrayBuffer[Coordination]
val blocks = new ArrayBuffer[Block]
for (window <- serial.sliding(2)) {
inCoordination match {
case false => window match {
case Array(gene@Gene(_), Comma(commaHead)) if (commaHead - gene.gene.tokenEnd() < 5) =>
inCoordination = true
blocks += Block(gene)
case _ =>
}
case true => window match {
case Array(Comma(_), Conj(_)) =>
endOfCoordination = true
case Array(Conj(_), gene@Gene(_)) if (endOfCoordination) =>
inCoordination = false
endOfCoordination = false
blocks += Block(gene)
coordinations += Coordination(Seq.empty[Block] ++ blocks)
blocks.clear()
case Array(Comma(commaHead), gene@Gene(_)) if (gene.gene.tokenEnd() - commaHead < 5) =>
case Array(gene1@Gene(_), gene2@Gene(_)) if (gene2.head == gene1.head) =>
blocks += Block(gene1)
case Array(gene@Gene(_), Comma(commaHead)) if (commaHead - gene.gene.tokenEnd() < 5) =>
blocks += Block(gene)
case Array(gene@Gene(_), Conj(_)) =>
endOfCoordination = true
blocks += Block(gene)
case _ =>
blocks.clear()
endOfCoordination = false
inCoordination = false

}
}
}
ProtXMLLoader.log.println(coordinations.mkString("\n"))


//    ProtXMLLoader.log.println(parsed)
for (pair <- pairs) {
for (coordination <- coordinations.find(_.coordinatesPair(pair))) {
val firstBlock = coordination.blocks.head
val firstGene = firstBlock.gene
val from = firstGene.gene.tokenBegin()
val to = coordination.blocks.last.gene.gene.tokenEnd()
val span = Prot.GeneMention.facade().tokenBegin.set(from).tokenEnd.set(to)
val head = MentionHeadFinder.findHead(span, sentence)
val parent = sentence.deps.find(d => d.mod() == head && (d.head() < from || d.head() >= to))
val parentLemma = parent.map(d => sentence.tokens(d.head()).lemma()).getOrElse("NO PARENT LEMMA")
val parentDep = parent.map(_.label()).getOrElse("NO PARENT DEP")
val result = ProteinProteinCoordination(parentLemma, parentDep, "conj_and", "NO OTHER MOD", head)
pair.coordinationPattern := Prot.ProtProtCoordination.facade().load(result)
}
}
}

def main(args: Array[String]) {
val gene1 = Gene(Prot.GeneMention.facade().tokenHead.set(5))
val comma1 = Comma(7)
val gene2 = Gene(Prot.GeneMention.facade().tokenHead.set(9))
val comma2 = Comma(11)
val conj1 = Conj(12)
val gene3 = Gene(Prot.GeneMention.facade().tokenHead.set(13))

val all = Seq(gene1, comma1, gene2, comma2, conj1, gene3, Comma(14)).sortBy(_.head)

println(Parser.parse(SeqReader(all)))

}

}

case class ProteinProteinCoordination(parentLemma: String, parentDep: String,
conjLabel: String, otherMod: String, head: Int)

object MentionHeadFinder {
def edgeHeadOutside(e: NLP.DepEdge, gene: Prot.GeneMention): Boolean = {
(e.head() < gene.tokenBegin() || e.head() >= gene.tokenEnd())
}
def edgeModInside(e: NLP.DepEdge, gene: Prot.GeneMention): Boolean = {
(e.mod() >= gene.tokenBegin() && e.mod() < gene.tokenEnd())
}
def findHead(gene: Prot.GeneMention, sentence: SentenceHelper): Int = {
val outsideParents = sentence.deps.find(e => edgeHeadOutside(e, gene) && edgeModInside(e, gene))
outsideParents.headOption.map(_.mod()).getOrElse(gene.tokenEnd() - 1)
}
}

class SentenceHelper(val sentence: NLP.Sentence) {
lazy val tokens = sentence.tokens().toArray
lazy val deps = depStructure.deps().toArray
lazy val depStructure = sentence.depStructures().head
lazy val paths = depStructure.shortestPaths()
lazy val parents = deps.groupBy(_.mod())
lazy val children = deps.groupBy(_.head())

def ancestors(token: Int, visited: Set[Int] = Set.empty): Seq[Seq[NLP.DepEdge]] = {
val allowed = parents.getOrElse(token, Array.empty[NLP.DepEdge]).filter(edge => !visited(edge.head()))
if (allowed.isEmpty) Seq(Seq.empty)
else
allowed.flatMap(edge => ancestors(edge.head(), visited + token).map(edge +: _))
}
}
*/