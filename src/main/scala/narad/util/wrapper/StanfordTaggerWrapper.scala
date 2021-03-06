package narad.util
import edu.stanford.nlp.ling.HasWord;
import edu.stanford.nlp.ling.Sentence;
import edu.stanford.nlp.ling.TaggedWord;
import edu.stanford.nlp.tagger.maxent.MaxentTagger;
import edu.stanford.nlp.tagger.maxent.TaggerConfig;
import edu.stanford.nlp.util.XMLUtils;
import narad.nlp.trees.ConstituentTree
import narad.io.tree.TreebankReader

class StanfordTaggerWrapper(filename: String) {
	val tagger = new MaxentTagger(filename)
	
	def replaceTags(trees: Array[ConstituentTree]) = {
		for (tree <- trees) {
			val sentence = tree.words.mkString(" ")
			val tagged = tagger.tagTokenizedString(sentence)
			val tags = tagged.split(" ").map(_.split("_")(1))
//			tree.replaceTags(tags)
      assert(true, "Code should not be run without above line fix")
		}
	}	
}

object StanfordTaggerWrapper {
	
	def main(args: Array[String]) = {
		val options = new ArgParser(args)
		val treebank = options.getString("--treebank")
		val taggerFile = options.getString("--tagger.file")
		val trees = TreebankReader.read(treebank, options).toArray
		val tagger = new StanfordTaggerWrapper(taggerFile)
		tagger.replaceTags(trees)
		for (t <- trees) println(t)
	}
}