package narad.util

object FeatureStatistics {
	
	def main(args: Array[String]) = {
		val options = new ArgParser(args)
		val featureFilename = options.getString("--feature.file")
		val counter = new HashCounter
		for (line <- io.Source.fromFile(featureFilename).getLines) {
			if (!line.startsWith("@") && line.contains("\t")) {
				var name, feats = line.split("\t")
				println(name)
				println(feats)
			}
		}
	}
}