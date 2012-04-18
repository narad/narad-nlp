package narad.projects.freebaser
import java.io.{File, FileOutputStream}
import narad.util.ArgParser
import scala.collection.mutable.ArrayBuffer

object NYTSentenceExtractor {
	val splitPattern = "( [a-z][a-zA-Z0-9]*)([\\.\\?!])( +[A-Z]|$)".r

	def main(args: Array[String]) {
		val options = new ArgParser(args)
		val corpusPath = options.getString("--corpus")
		val outputDir  = options.getString("--output.dir")
		val min        = options.getInt("--min.sentence", 5)
		
		System.err.println("Extracting Sentences...")
		val corpusFile = new File(corpusPath)
		for (file <- filenames(corpusFile, _.contains(".xml"))) {
			System.err.println("...%s".format(file))
			val sentences = readNYT(file, min).mkString("\n\n")
			if (outputDir != null) {
				val apath = file.getAbsolutePath
				val cpath = corpusFile.getAbsolutePath
				val opath = new File(outputDir).getAbsolutePath
				val ofile = new File(opath + "/" + file.getName) //new File(opath + apath.substring(cpath.size))
				if (!ofile.exists) {
					ofile.getParentFile.mkdirs
					ofile.createNewFile
				}
				var out = new java.io.FileWriter(ofile)
				out.write(sentences) 
				out.close				
			}
			else {
				println(sentences)
			}
		}
	}

	def filenames(path: File, filter: (String) => Boolean): Array[File] = {
		val files = new ArrayBuffer[File]
		if (!path.isDirectory && filter(path.toString)) return Array(path)
		for (file <- path.listFiles) {
			if (file.isDirectory) {
				files ++= filenames(file, filter)
			}
			else if (filter(file.getName)) {
				files += file
			}
		}
		return files.toArray
	}

	def readNYT(file: File, min: Int):Array[String] = {
		val snippets = new ArrayBuffer[String]
		val xml = scala.xml.parsing.ConstructingParser.fromSource(scala.io.Source.fromFile(file), false).document
		val testResults = xml \\ "_" foreach { node =>
			if (node.attributes.exists(_.value.toString == "full_text")) {			
				for (p <- node \\ "_" if p.label == "p") {
					snippets ++= splitSentence(p.text)
				}
			}
		}
		return snippets.toArray
	}

	def splitSentence(str: String, min: Int = 5): Array[String] = {
		val sentences = new ArrayBuffer[String]
		var last = 0
		(splitPattern findAllIn str).matchData foreach { m =>
			val sentence = (str.substring(last, m.start) + m.group(1) + " " + m.group(2)).trim
			if (sentence.split(" ").size >= min) sentences += sentence
			last = m.start + m.group(1).size + 1
		}
		sentences += str.substring(last)
		sentences.toArray
	}
}
