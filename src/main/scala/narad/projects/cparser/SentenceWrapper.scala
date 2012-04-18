package narad.projects.cparser

object SentenceWrapper {
	
	def main(args: Array[String]) {
		for (tree <- narad.nlp.parse.TreeReader.read(new narad.util.ArgParser(args))) {
			println("(TOP %s)".format(tree.tokens.mkString(" ")))
		}	
	}
}