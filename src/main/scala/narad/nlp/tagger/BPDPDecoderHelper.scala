package narad.nlp.tagger
import narad.util.ArgParser

object BPDPDecoderHelper {
	
	def main(args: Array[String]) = {
		val options = new ArgParser(args)
		val dictFile = options.getString("--dict.file")
		val bpdpFile = options.getString("--bpdp.file")
		val dict = TagDictionary.fromFile(dictFile)
		val alltags = dict.all.toArray
		for (line <- io.Source.fromFile(bpdpFile).getLines()) {
//			println(line)
			if (line.isEmpty) {
				println()
			}
			else {
				val cols = line.split("\t").map(_.trim)
				if (cols.size == 3) {
					val word = cols(1)
					val id = cols(2).toInt
					if (dict.contains(word)) {
						val tags = dict.tags(word).toArray
						println(tags(id))					
					}
					else {
						println(alltags(id))
					}					
				}
			}
		}
	}
}