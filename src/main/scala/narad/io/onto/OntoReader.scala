package narad.io.onto
import narad.nlp.trees.ConstituentTree
import narad.io.tree.TreebankReader
import narad.util.{HashCounter, ArgParser}
import narad.io.ner.{F1Container, NamedEntityReader, NamedEntityDatum}
import narad.bp.optimize.Scorable
import narad.nlp.parser.metrics.EvalBContainer
import java.io.FileWriter

class OntoReader(nerFile: String, treeFile: String, options: OntoReaderOptions = new OntoReaderOptions(Array())) extends Iterable[OntoDatum] {
	
	def iterator: Iterator[OntoDatum] = {
		val treeIter = new TreebankReader(treeFile)
    val nerIter = new NamedEntityReader(nerFile)
		val zipped = nerIter.iterator.zip(treeIter.iterator).filter { case(ner,tree) =>
      assert(ner.words.size == tree.tokens.size, "Number of tokens did not match: ner (%d) vs. syntax (%d):\n%s\n%s".format(ner.words.size, tree.words.size, ner.words.mkString(" "), tree.words.mkString(" ")))
      tree.length > options.MIN_LENGTH
    }
		for (pair <- zipped) yield new OntoDatum(pair._1, pair._2)
	}
}

object OntoReader {

  def main(args: Array[String]) {
    val options = new OntoReaderOptions(args)
    val reader = new OntoReader(options.NER_FILE, options.TREE_FILE, options)
    val nerOut = new FileWriter(options.NER_OUT_FILE)
    val treeOut = new FileWriter(options.TREE_OUT_FILE)
    for (datum <- reader) {
      if (options.WRITE_TO_FILE) {
        nerOut.write(datum.ner + "\n")
        treeOut.write(datum.tree + "\n\n")
      }
    }
    nerOut.close()
    treeOut.close()
  }
}

class OntoReaderOptions(args: Array[String] = Array()) extends ArgParser(args) {

  def NER_FILE = getString("--ner.file")

  def TREE_FILE = getString("--tree.file")

  def NER_OUT_FILE = getString("--ner.out.file")

  def TREE_OUT_FILE = getString("--tree.out.file")

  def MIN_LENGTH = getInt("--min.length", 0)

  def WRITE_TO_FILE = getBoolean("--write.to.file")
}