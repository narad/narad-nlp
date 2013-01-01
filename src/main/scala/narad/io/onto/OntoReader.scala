package narad.io.onto
import narad.nlp.trees.ConstituentTree
import narad.io.reader.TreebankReader
import narad.util.ArgParser

class OntoReader(nerFile: String, treeFile: String, options: ArgParser = new ArgParser(Array[String]())) extends Iterable[OntoDatum] {
	
	def iterator: Iterator[OntoDatum] = {
		val treeIter = new TreebankReader(treeFile)
    val nerIter = new NamedEntityReader(nerFile)
		val zipped = nerIter.iterator.zip(treeIter.iterator)
		for (pair <- zipped) yield new OntoDatum(pair._1, pair._2)
	}
}


case class OntoDatum(ner: NamedEntityDatum, tree: ConstituentTree) {

//  System.out.println("NER = " + ner)
//  System.out.println("SYNTAX = " + tree)

  def tokens = tree.taggedTokens()

	def slen = ner.size
}
