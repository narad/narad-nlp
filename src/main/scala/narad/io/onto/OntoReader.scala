package narad.io.onto
import narad.nlp.trees.ConstituentTree
import narad.io.tree.TreebankReader
import narad.util.ArgParser
import narad.io.ner.{NamedEntityReader, NamedEntityDatum}
import narad.bp.optimize.Scorable

class OntoReader(nerFile: String, treeFile: String, options: ArgParser = new ArgParser(Array[String]())) extends Iterable[OntoDatum] {
	
	def iterator: Iterator[OntoDatum] = {
		val treeIter = new TreebankReader(treeFile)
    val nerIter = new NamedEntityReader(nerFile)
		val zipped = nerIter.iterator.zip(treeIter.iterator)
		for (pair <- zipped) yield new OntoDatum(pair._1, pair._2)
	}
}


case class OntoDatum(ner: NamedEntityDatum, tree: ConstituentTree) extends Scorable {

  def tokens = tree.tokens

	def slen = tree.length

  def score(other: Scorable) = {
    println("onto scorer")
    ner.score(other)
  }
}
