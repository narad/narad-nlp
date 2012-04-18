package narad.projects.tagger
import narad.nlp.parse._
import scala.collection.mutable.{ArrayBuffer, HashSet}

object TFeaturizer {
	
	def main(args: Array[String]) {
		val options = new narad.util.ArgParser(args)
		val input  = options.getString("-treebank")
		val tagset = new HashSet[String]
		for (tree <- TreeReader.read(input)) {
			tagset ++= tree.tokens.map(_.pos)
		}
		val taglist = tagset.toArray
		for (tree <- TreeReader.read(input)) {
			printFeatures(tree, taglist)
		}
	}
	
	def printFeatures(tree: Tree, tagset: Array[String], bigram: Boolean = true) = {
		val tokens = tree.tokens
		println("@slen\t%d".format(tokens.size))
		println("@words\t%s".format(tokens.map(_.word).mkString(" ")))
		println("@tags\t%s".format(tokens.map(_.pos).mkString(" ")))
		println("@tagset\t%s".format(tagset.mkString(" ")))
		for (i <- 0 until tokens.size; j <- 0 until tagset.size) {
			val feats = lexicalFeatures(tokens(i)).map("%s_%s".format(tagset(j), _)).mkString(" ")
			if (tokens(i).pos == tagset(j)) {
				println("pos(%d,%d)\t+%s".format(i,j,feats))		
			}
			else {
				println("pos(%d,%d)\t%s".format(i,j,feats))				
			}
		}
		if (bigram) {
			for (i <- 0 until tokens.size-1; j <- 0 until tagset.size; k <- 0 until tagset.size) {
				val feats = "W1-%s W2-%s W1-2-%s_%s".format(tokens(i).word, tokens(i+1).word, tokens(i).word, tokens(i+1).word).map("%s_%s_%s".format(tagset(j), tagset(k), _)).mkString(" ")
				if (tokens(i).pos == tagset(j) && tokens(i+1).pos == tagset(k)) {
					println("bpos(%d,%d,%d)\t+%s".format(i,j,k,feats))
				}
				else {
					println("bpos(%d,%d,%d)\t%s".format(i,j,k,feats))
				}
			}
		}
		println
	}
	
	def lexicalFeatures(token: Token): Array[String] = {
		val feats = new ArrayBuffer[String]
		val word = token.word
		val pos  = token.pos
		feats += "word_%s".format(word)
//		feats += "wordlen_%d".format(word.size)
//		for (i <- 1 to Math.min(4,word.size)) {
//			feats += "suffix_%d_%s".format(i, word.substring(word.size-i))
//		}
		return feats.toArray
	}
}



