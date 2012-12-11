package narad.io.onto
import narad.nlp.trees.Tree
import narad.io.reader.TreebankReader
import narad.util.ArgParser

object OntoReader {
	
	def read(nerFile: String, treeFile: String, options: ArgParser): Iterator[OntoDatum] = {
		val treeIter = TreebankReader.read(treeFile, options)
//		val nerIter  = NamedEntityReader.read(nerFile, options)
    val nerIter = new NamedEntityReader(nerFile)
		val zipped = nerIter.iterator.zip(treeIter)
		for (pair <- zipped) yield new OntoDatum(pair._1, pair._2)
	}
}

case class OntoDatum(ner: NamedEntityDatum, tree: Tree) {
	
	def size = ner.size
}