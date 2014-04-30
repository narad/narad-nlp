package narad.io.util


import java.io._
import narad.io.datum._
import narad.util.ArgParser
import scala.collection.mutable.ArrayBuffer

object AceReader {
	
	def readSGM(filename: String, iformat: String = "UTF-8"): String = {
		val tb = new ArrayBuffer[String]
		try {
		val src = scala.io.Source.fromFile(filename, iformat)
		val xml = scala.xml.parsing.ConstructingParser.fromSource(src, false).document
		val testResults = xml \\ "_" foreach { node =>
//			if (node.label == "TEXT") { tb += node.text; println(node.text.split("\n").size); println(node.text) }
			if (node.label == "TURN") { tb += node.text } //; println(node.text.split("\n").size); println(node.text) }
		}
		src.close
	}
	catch {
		case e: Exception => System.err.println("Error in XML reading:\n" + e.getStackTrace.mkString("\n"))
	}
		return tb.mkString("\n")
	}
	
	def readFullMentions(filename: String, iformat: String = "UTF-8"): Array[FullMention] = {
		val entities = readEntities(filename, iformat)
		val fms = new ArrayBuffer[FullMention]
		for (e <- entities) {
			for (m <- e.mentions) {
				fms += FullMention(m.label, e.label, e.sublabel, m.start, m.end, m.text)
			}
		}
		return fms.toArray
	}

	def readRelations(filename: String, iformat: String = "UTF-8"): Array[ACERelation] = {
		val relations = new ArrayBuffer[ACERelation]
		try {
		val src = scala.io.Source.fromFile(filename, iformat)
		val xml = scala.xml.parsing.ConstructingParser.fromSource(src, false).document
		val testResults = xml \\ "_" foreach { node =>
			if (node.label == "relation") {
				extractRelation(node) match {
					case Some(r) => relations += r
					case _ =>
				}
			}
		}
		src.close
	}
	catch {
		case e: Exception => System.err.println("Error in XML reading:\n" + e.getStackTrace.mkString("\n"))
	}
		return relations.toArray
	}

	def extractRelation(node: scala.xml.Node): Option[ACERelation] = {
		var label = getAttribute(node, "TYPE")
		var sublabel = getAttribute(node, "SUBTYPE")
		var a1 = null.asInstanceOf[ACEArg]
		var a2 = null.asInstanceOf[ACEArg]
		for (child <- node.child) {
			for (gchild <- child.child if gchild.label == "relation_mention_argument") {
				val extent = gchild.child(0).child(0)
				if (getAttribute(gchild, "ROLE") == "Arg-1") {
					val start = getAttribute(extent, "START").toInt
					val end = getAttribute(extent, "END").toInt
					a1 = new ACEArg(start, end, gchild.text)
				}
				else if (getAttribute(gchild, "ROLE") == "Arg-2") {
					val start = getAttribute(extent, "START").toInt
					val end = getAttribute(extent, "END").toInt
					a2 = new ACEArg(start, end, gchild.text)
				} 					
			}
		}
		if (label == null || a1 == null || a2 == null) {
			return None
		}
		else {
			return Some(ACERelation(label, sublabel, a1, a2))			
		}
	}
	
  def readEntities(filename: String, iformat: String="UTF-8"): Array[ACEEntity] = { 
		val entities = new ArrayBuffer[ACEEntity]
		try {
			val src = scala.io.Source.fromFile(filename, iformat)
			val xml = scala.xml.parsing.ConstructingParser.fromSource(src, false).document
			xml \\ "_" foreach { node =>
				if (node.label == "entity") {
					val label = getAttribute(node, "TYPE")
					val sublabel = getAttribute(node, "SUBTYPE")
					val mentions = readEntityMentions(node)
					entities += new ACEEntity(label, sublabel, mentions)
				}
			}
			src.close
		}
		catch {
			case e: Exception => System.err.println("Error in XML reading:\n" + e.getStackTrace.mkString("\n"))
		}
		return entities.toArray
	}

	def readEntityMentions(node: scala.xml.Node): Array[ACEEntityMention] = {
		node.child.filter(_.label == "entity_mention").map { child =>
			extractEntity(child)
		}.collect{case Some(m) => m}.toArray		
	}

	def extractEntity(node: scala.xml.Node): Option[ACEEntityMention] = {
		var label = getAttribute(node, "TYPE")
		for (child <- node.child; gchild <- child.child if child.label == "extent") {
			val start = getAttribute(gchild, "START").toInt
			val end = getAttribute(gchild, "END").toInt
			return Some(new ACEEntityMention(label, start, end, gchild.text))
		}
		return None
	}
	
	def getAttribute(node: scala.xml.Node, attribute: String): String = {
		val att = node.attribute(attribute)
		att match {
			case Some(value) => return value(0).text
			case _ => System.err.println("Error: Attribute %s not found in Node %s".format(attribute, node))
		}
		return "N/A"
	}
	
	def main(args: Array[String]) {
		val options = new ArgParser(args)
		val filename = options.getString("--sgm.file")
		val basename = filename.substring(0, filename.size-4)
		val text = AceReader.readSGM(basename + ".sgm")
		val entities  = AceReader.readEntities(basename + ".apf.xml")
		for (entity <- entities) {
			println(entity)
		}
	}
}




































































//		.filter{e => val s = e.text.split(" ").size; s >= minwords && s <= maxwords}

//		val relations = AceReader.readRelations(basename + ".apf.xml").filter(!_.overlaps)





























/*
def extractRelation(node: scala.xml.Node): Option[ACERelation] = {
	var label = getAttribute(node, "TYPE")
	var sublabel = getAttribute(node, "SUBTYPE")
	var a1 = null.asInstanceOf[EntityMention]
	var a2 = null.asInstanceOf[EntityMention]
	for (child <- node.child) {
		for (gchild <- child.child if gchild.label == "relation_mention_argument") {
			if (getAttribute(gchild, "ROLE") == "Arg-1") {
				val extent = gchild.child(0).child(0)
				val start = getAttribute(extent, "START").toInt
				val end = getAttribute(extent, "END").toInt
				a1 = new EntityMention(gchild.text, start, end)
			}
			else if (getAttribute(gchild, "ROLE") == "Arg-2") {
				val extent = gchild.child(0).child(0)
				val start = getAttribute(extent, "START").toInt
				val end = getAttribute(extent, "END").toInt
				a2 = new EntityMention(gchild.text, start, end)
			} 					
		}
	}
	if (label == null || a1 == null || a2 == null) {
		return None
	}
	else {
		return Some(Relation(label, sublabel, a1, a2))			
	}
}

*/






/*
package narad.nlp.relations

import edu.stanford.nlp.tagger.maxent.MaxentTagger
import edu.stanford.nlp.tagger.maxent.TaggerConfig
import edu.stanford.nlp.util.XMLUtils
import java.io._
import narad.nlp.io.SentenceReader
import narad.nlp.parse.ConstituentTree
import narad.util.{ArgParser, ChunkReader, StanfordParserWrapper}
import scala.collection.mutable.ArrayBuffer

case class DataTemplate(id: Int, word: String, isEntity: Boolean, rel: String = "_",
 												reltype: String = "_", relsubtype: String = "_", argslot: String = "_") {}

case class ACEDocument(text: String, relations: Array[Relation], entities: Array[ACEEntity]) {}

case class ACEEntity(label: String, sublabel: String, mentions: Array[EntityMention]) {}

object AceReader {
	
	def readSGM(filename: String): String = {
		val tb = new ArrayBuffer[String]
		try {
		val src = scala.io.Source.fromFile(filename)
		val xml = scala.xml.parsing.ConstructingParser.fromSource(src, false).document
		val testResults = xml \\ "_" foreach { node =>
			if (node.label == "TEXT") tb += node.text
		}
		src.close
	}
	catch {
		case e: Exception => System.err.println("Error in XML reading:\n" + e.getStackTrace.mkString("\n"))
	}
		return tb.mkString("\n")
	}
	
	def readRelations(filename: String): Array[Relation] = {
		val relations = new ArrayBuffer[Relation]
		try {
		val src = scala.io.Source.fromFile(filename)
		val xml = scala.xml.parsing.ConstructingParser.fromSource(src, false).document
		val testResults = xml \\ "_" foreach { node =>
			if (node.label == "relation") {
				extractRelation(node) match {
					case Some(r) => relations += r
					case _ =>
				}
			}
		}
		src.close
	}
	catch {
		case e: Exception => System.err.println("Error in XML reading:\n" + e.getStackTrace.mkString("\n"))
	}
		return relations.toArray
	}

// 	<entity ID="AFP_ENG_20030320.0722-E1" TYPE="ORG" SUBTYPE="Media" CLASS="SPC">
// <entity_mention ID="AFP_ENG_20030320.0722-E1-2" TYPE="NAM" LDCTYPE="NAM" LDCATR="TRUE">
	
  def readEntities(filename: String) = readEntityMentions(filename) // replace soon

	def readEntityMentions(filename: String): Array[EntityMention] = {
		val entities = new ArrayBuffer[EntityMention]
		try {
		val src = scala.io.Source.fromFile(filename)
		val xml = scala.xml.parsing.ConstructingParser.fromSource(src, false).document
		val testResults = xml \\ "_" foreach { node =>
			if (node.label == "entity_mention") {
				extractEntity(node) match {
					case Some(e) => entities += e
					case _ =>
				}
			}
		}
		src.close
	}
	catch {
		case e: Exception => System.err.println("Error in XML reading:\n" + e.getStackTrace.mkString("\n"))
	}
		return entities.toArray
	}
	
	def extractRelation(node: scala.xml.Node): Option[Relation] = {
		var label = getAttribute(node, "TYPE")
		var sublabel = getAttribute(node, "SUBTYPE")
		var a1 = null.asInstanceOf[EntityMention]
		var a2 = null.asInstanceOf[EntityMention]
		for (child <- node.child) {
			for (gchild <- child.child if gchild.label == "relation_mention_argument") {
				if (getAttribute(gchild, "ROLE") == "Arg-1") {
					val extent = gchild.child(0).child(0)
					val start = getAttribute(extent, "START").toInt
					val end = getAttribute(extent, "END").toInt
					a1 = new EntityMention(gchild.text, start, end)
				}
				else if (getAttribute(gchild, "ROLE") == "Arg-2") {
					val extent = gchild.child(0).child(0)
					val start = getAttribute(extent, "START").toInt
					val end = getAttribute(extent, "END").toInt
					a2 = new EntityMention(gchild.text, start, end)
				} 					
			}
		}
		if (label == null || a1 == null || a2 == null) {
			return None
		}
		else {
			return Some(Relation(label, sublabel, a1, a2))			
		}
	}

	def extractEntity(node: scala.xml.Node): Option[EntityMention] = {
		var label = getAttribute(node, "TYPE")
		for (child <- node.child; gchild <- child.child if child.label == "extent") {
			val start = getAttribute(gchild, "START").toInt
			val end = getAttribute(gchild, "END").toInt
			return Some(new EntityMention(gchild.text, start, end, label))
		}
		return None
	}
	
	def getAttribute(node: scala.xml.Node, attribute: String): String = {
		val att = node.attribute(attribute)
		att match {
			case Some(value) => return value(0).text
			case _ => System.err.println("Error: Attribute %s not found in Node %s".format(attribute, node))
		}
		return "N/A"
	}
}

object AcePreprocessor {
	
	def maximalRelations(relations: Array[Relation]): Array[Relation] = {
			val maxrels = new ArrayBuffer[Relation]
			val numRels = relations.size
			for (i <- 0 until numRels) {
				var covered = false
				for (j <- i+1 until numRels if i != j) {
					if (relations(j).sharesMention(relations(i))) covered = true
				}
				if (!covered) maxrels += relations(i)
			}
			maxrels.toArray		
	}
	
	def maximalEntities(entities: Array[EntityMention], relations: Array[Relation]): Array[EntityMention] = {
		val maxents = new ArrayBuffer[EntityMention]
		val intents = new ArrayBuffer[EntityMention]
		val numEnts = entities.size
		for (i <- 0 until numEnts) {
			val ent = entities(i)
			var conflicts = false
			for (r <- relations) {
				if (r.arg1.overlaps(ent) || r.arg2.overlaps(ent)) conflicts = true
			}
			if (!conflicts) intents += ent
		}
		for (i <- 0 until intents.size) {
			val ent = intents(i)
			var conflicts = false
			for (j <- 0 until intents.size if i != j) {
				if (intents(j).encompasses(intents(i))) conflicts = true							
			}
			if (!conflicts) maxents += ent
		}
		return maxents.toArray
	}

	def extractTemplates(sentence: String, ents: Array[EntityMention], rels: Array[Relation]): Array[DataTemplate] = {
		val templates = new ArrayBuffer[DataTemplate]
		val rindices = rels.map{r => new Tuple2(r, r.matchIndices(sentence))}
		val eindices = ents.map{e => new Tuple2(e, e.matchIndices(sentence))}
		val tokens = sentence.split(" ") 
		val rbuff = new ArrayBuffer[String]
		val rs = new Array[Relation](tokens.size)
		val rss = Array.ofDim[Int](rindices.size, 2)
		val words = new ArrayBuffer[String]
		var i = 0
		var j = 0
		while(i < tokens.size) {
			var found = false
			var rcount = 0
			for (r <- rindices) {
				val rel = r._1
				if (i == r._2._1._1 && !found) {
					val combed = r._1.arg1.text.replaceAll(" ", "_")
					rbuff += "%d\t%s\t%s\t%s\t%s\t%s\t%s".format(j+1, combed, "Y", "<R_PLACE_HOLDER>", rel.label, rel.sublabel,"ARG-1")
					words += combed
					i += 1 + r._2._1._2 - r._2._1._1
					j += 1
					rss(rcount)(0) = j
					found = true
				}
				else if (i == r._2._2._1 && !found) {
					val combed = r._1.arg2.text.replaceAll(" ", "_")
					rbuff += "%d\t%s\t%s\t%s\t%s\t%s\t%s".format(j+1, combed, "Y", "<R_PLACE_HOLDER>", rel.label, rel.sublabel,"ARG-2")
					words += combed
					i += 1 + r._2._2._2 - r._2._2._1		
					j += 1					
					rss(rcount)(1) = j
					found = true
				}
				rcount += 1
			}
			if (!found) {
				for (e <- eindices) {
					if (i == e._2._1 && !found) {
						val combed = e._1.text.replaceAll(" ", "_")
						rbuff += "%d\t%s\t%s\t%s\t%s\t%s\t%s".format(j+1, combed, "Y", "_", "_", "_", "_") //, "<R_PLACE_HOLDER>", rel.label, rel.sublabel,"ARG-1")
						words += combed
						i += 1 + e._2._2 - e._2._1
						j += 1
						found = true
					}								
				}
			}
			if (!found) {
				rbuff += (j+1 + "\t" + tokens(i) + "\t_\t_\t_\t_\t_")
				words += tokens(i)
				i += 1						
				j += 1
			}
		}

		var c = 0
		//					for (i <- 0 until rss.size) {println(c + "\t" + rss(i).mkString(", ")); c += 1}
		for (i <- 0 until rbuff.size) {
			for (rr <- rss) {
				if (rr(0) == i+1) {
					rbuff(i) = rbuff(i).replaceAll("<R_PLACE_HOLDER>", rr(1).toString)														
				}
				else if (rr(1) == i+1) {
					rbuff(i) = rbuff(i).replaceAll("<R_PLACE_HOLDER>", rr(0).toString)														
				}
			}					
		}
		for (line <- rbuff) {
			val cols = line.split("\t")
			templates += DataTemplate(cols(0).toInt, cols(1), cols(2) == "Y", cols(3), cols(4), cols(5), cols(6))
		}
		return templates.toArray
	}

  def containsArrayAt(array: Array[String], subarray: Array[String], start: Int): Boolean = {
		println("------------------")
		println(array.mkString(" "))
		println(subarray.mkString(" "))
		if (array.size-start >= subarray.size) {
			var j = 0
			while (j < subarray.size) {
				if (array(start+j) != subarray(j)) return false
			}
			return true			
		}
		return false
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

	def main(args: Array[String]) = {
		val options = new ArgParser(args)
		val input = options.getString("--input")
		val taggerFile = options.getString("--tagger.file")
		val parserFile = options.getString("--parser.file")

		if (parserFile == null) {
			annotateRelations(input, taggerFile)
		}
		else {
			annotateSyntax(input, parserFile)
		}
	}

	def annotateRelations(input: String, taggerFile: String) = {
		var scount = 0
		val minwords = 1
		val maxwords = 8
		val tagger = if (taggerFile != null) new MaxentTagger(taggerFile) else null
		for (file <- filenames(new File(input), _.endsWith(".sgm"))) {
			val filename = file.toString
			val basename = filename.substring(0, filename.size-4)
			val sentences = SentenceReader.read(AceReader.readSGM(basename + ".sgm"))
			val relations = AceReader.readRelations(basename + ".apf.xml").filter(!_.overlaps)
			val entities  = AceReader.readEntities(basename + ".apf.xml").filter{e => val s = e.text.split(" ").size; s >= minwords && s <= maxwords}
			for (sentence <- sentences) {
				scount += 1
				val rels = maximalRelations(relations.filter(_.matches(sentence)))
				val ents = maximalEntities(entities.filter(_.matches(sentence)), rels)
				if (rels.size > 0 || ents.size > 1) {
					val templates = extractTemplates(sentence, ents, rels)
					val words = templates.map(_.word)
					val slen = words.size
					var tags = new Array[String](slen)

					if (tagger != null) {
						tags = tagger.tagTokenizedString(words.mkString(" ")).split(" ").map(s => s.substring(s.lastIndexOf("_")+1))						
					}
					assert(tags.size == slen, "Tags (%d) does not match slen of %d".format(tags.size,  slen))

					var i = 0
					for (template <- templates) {
						val slots = Array(
							template.id,
							template.word,
							tags(i),
							tags(i),
							"-1",
							if (template.isEntity) "Y" else "_",
							template.rel,
							template.reltype,
							template.relsubtype,
							template.argslot)
							println(slots.mkString("\t"))
							i += 1
					}
				}
				println
			}
		}
		System.err.println("%d sentences extracted.".format(scount))
	}

	def annotateSyntax(input: String, parserFile: String) = {
		val parser = new StanfordParserWrapper(parserFile)
		var scount = 0
		for (chunk <- ChunkReader.read(input) if chunk.split("\n").size > 1) {
			val lines = chunk.split("\n")
			val numEnts = lines.map(_.split("\t")(5)).filter(_ == "Y").size
			if (numEnts > 1) {
				val words = lines.map(_.split("\t")(1))
				val slen = words.size
				var heads = new Array[String](slen+1)	
				var tree = null.asInstanceOf[ConstituentTree]
				var parseFailed = false				
				if (parser != null) {
					parser.parses(words.mkString(" ")) match {
						case Some(parsepair) => {
							tree = parsepair._1
							tree.annotateWithIndices(0)
							tree.setLabels("BRK")
							tree.setYield(words ++ words, words ++ words)
							heads = parsepair._2.map(_.toString)
						}
						case _=> {
							val parse = new Array[String](slen+1)
							for (i <- 0 to slen) parse(i) = "_"
							parseFailed = true
							parse
						}
					}						
				}
				if (!parseFailed) {
					scount += 1
					assert(heads.size-1 == slen, "Parse length (%d) does not match slen of %d".format(heads.size,  slen))
					println("@parse\t" + tree.toString)
					for (i <- 0 until lines.size) {
						val cols = lines(i).split("\t")
						cols(4) = heads(i+1)
						println(cols.mkString("\t"))
					}
					println					
				}
			}				
		}
		System.err.println("%d sentences extracted.".format(scount))
	}
}







//"/Users/narad/Desktop/freebaser/models/tagger/english-bidirectional-distsim.tagger")


/*

	def process(dir: String, outFile: File, tagger: MaxentTagger) = {
		val wout = new FileWriter(outFile)					
		var scount = 0
		for (file <- filenames(new File(dir), _.endsWith(".sgm"))) {
			val filename = file.toString
			val basename = filename.substring(0, filename.size-4)
			val sentences = SentenceReader.read(AceReader.readSGM(basename + ".sgm"))
			val relations = AceReader.readRelations(basename + ".apf.xml").filter(!_.overlaps)
			for (sentence <- sentences) {
				val vrels = relations.filter(_.matches(sentence))
				val grels = new ArrayBuffer[Relation]
				for (i <- 0 until vrels.size) {
					var winner = true
					for (j <- i+1 until vrels.size if i != j) {
						if (vrels(j).sharesMention(vrels(i))) winner = false
					}
					if (winner) grels += vrels(i)
				}

//				println(sentence)				

				if (grels.size > 0) {				
					scount += 1	
					val rindices = grels.map{g => new Tuple2(g, g.matchIndices(sentence))}
					val tokens = sentence.split(" ") 
					val rbuff = new ArrayBuffer[String]
					val rs = new Array[Relation](tokens.size)
					val rss = Array.ofDim[Int](rindices.size, 2)
					val words = new ArrayBuffer[String]
					var i = 0
					var j = 0
					while(i < tokens.size) {
						var found = false
						var rcount = 0
						for (r <- rindices) {
							val rel = r._1
							if (i == r._2._1._1 && !found) {
								val combed = r._1.arg1.text.replaceAll(" ", "_")
								rbuff += "%d\t%s\t%s\t%s\t%s\t%s\t%s".format(j+1, combed, "Y", "<R_PLACE_HOLDER>", rel.label, rel.sublabel,"ARG-1")
								words += combed
								i += 1 + r._2._1._2 - r._2._1._1
								j += 1
								rss(rcount)(0) = j
								found = true
							}
							else if (i == r._2._2._1 && !found) {
								val combed = r._1.arg2.text.replaceAll(" ", "_")
								rbuff += "%d\t%s\t%s\t%s\t%s\t%s\t%s".format(j+1, combed, "Y", "<R_PLACE_HOLDER>", rel.label, rel.sublabel,"ARG-2")
								words += combed
								i += 1 + r._2._2._2 - r._2._2._1		
								j += 1					
								rss(rcount)(1) = j
								found = true
							}
							rcount += 1
						}
						if (!found) {
							rbuff += (j+1 + "\t" + tokens(i) + "\t_\t_\t_\t_\t_")
							words += tokens(i)
							i += 1						
							j += 1
						}
					}
					
					val tags = tagger.tagTokenizedString(words.mkString(" ")).split(" ").map(s => s.substring(s.lastIndexOf("_")+1))
					for (i <- 0 until words.size) {
						wout.write("%d\t%s\t_\t%s\t%s\n".format(i+1, words(i), tags(i), tags(i)))
					}
					wout.write("\n")
					
					var c = 0
//					for (i <- 0 until rss.size) {println(c + "\t" + rss(i).mkString(", ")); c += 1}
					for (i <- 0 until rbuff.size) {
						for (rr <- rss) {
							if (rr(0) == i+1) {
								rbuff(i) = rbuff(i).replaceAll("<R_PLACE_HOLDER>", rr(1).toString)														
							}
							else if (rr(1) == i+1) {
								rbuff(i) = rbuff(i).replaceAll("<R_PLACE_HOLDER>", rr(0).toString)														
							}
						}					
					}
					println(rbuff.mkString("\n"))
					println
				}
			}
		}
		wout.close
		System.err.println("%d sentences extracted.".format(scount))
	}
	
	
	
	
	
	
	
	if (parser != null) {
		heads = parser.dependencyParse(words.mkString(" ")) match {
			case Some(parse) => parse.map(_.toString)
			case _=> {
				val parse = new Array[String](slen+1)
				for (i <- 0 to slen) parse(i) = "_"
				parse
			}
		}						
		parser.constituencyParse(words.mkString(" ")) match {
			case Some(parse) => {
				tags = parse.tokens.map(_.pos)
				parse.map(_.toString)
			}
			case _=> {
				for (i <- 0 until slen) tags(i) = "ERR"
			}
		}						
	}
	if (tagger != null) {
		tags = tagger.tagTokenizedString(words.mkString(" ")).split(" ").map(s => s.substring(s.lastIndexOf("_")+1))						
	}
	assert(tags.size == slen && heads.size-1 == slen, "Tags (%d) or Parse (%d) do not match slen of %d".format(tags.size, heads.size, slen))
	

*/


*/


//"/Users/narad/Desktop/freebaser/models/tagger/english-bidirectional-distsim.tagger")


/*

	def process(dir: String, outFile: File, tagger: MaxentTagger) = {
		val wout = new FileWriter(outFile)					
		var scount = 0
		for (file <- filenames(new File(dir), _.endsWith(".sgm"))) {
			val filename = file.toString
			val basename = filename.substring(0, filename.size-4)
			val sentences = SentenceReader.read(AceReader.readSGM(basename + ".sgm"))
			val relations = AceReader.readRelations(basename + ".apf.xml").filter(!_.overlaps)
			for (sentence <- sentences) {
				val vrels = relations.filter(_.matches(sentence))
				val grels = new ArrayBuffer[Relation]
				for (i <- 0 until vrels.size) {
					var winner = true
					for (j <- i+1 until vrels.size if i != j) {
						if (vrels(j).sharesMention(vrels(i))) winner = false
					}
					if (winner) grels += vrels(i)
				}

//				println(sentence)				

				if (grels.size > 0) {				
					scount += 1	
					val rindices = grels.map{g => new Tuple2(g, g.matchIndices(sentence))}
					val tokens = sentence.split(" ") 
					val rbuff = new ArrayBuffer[String]
					val rs = new Array[Relation](tokens.size)
					val rss = Array.ofDim[Int](rindices.size, 2)
					val words = new ArrayBuffer[String]
					var i = 0
					var j = 0
					while(i < tokens.size) {
						var found = false
						var rcount = 0
						for (r <- rindices) {
							val rel = r._1
							if (i == r._2._1._1 && !found) {
								val combed = r._1.arg1.text.replaceAll(" ", "_")
								rbuff += "%d\t%s\t%s\t%s\t%s\t%s\t%s".format(j+1, combed, "Y", "<R_PLACE_HOLDER>", rel.label, rel.sublabel,"ARG-1")
								words += combed
								i += 1 + r._2._1._2 - r._2._1._1
								j += 1
								rss(rcount)(0) = j
								found = true
							}
							else if (i == r._2._2._1 && !found) {
								val combed = r._1.arg2.text.replaceAll(" ", "_")
								rbuff += "%d\t%s\t%s\t%s\t%s\t%s\t%s".format(j+1, combed, "Y", "<R_PLACE_HOLDER>", rel.label, rel.sublabel,"ARG-2")
								words += combed
								i += 1 + r._2._2._2 - r._2._2._1		
								j += 1					
								rss(rcount)(1) = j
								found = true
							}
							rcount += 1
						}
						if (!found) {
							rbuff += (j+1 + "\t" + tokens(i) + "\t_\t_\t_\t_\t_")
							words += tokens(i)
							i += 1						
							j += 1
						}
					}
					
					val tags = tagger.tagTokenizedString(words.mkString(" ")).split(" ").map(s => s.substring(s.lastIndexOf("_")+1))
					for (i <- 0 until words.size) {
						wout.write("%d\t%s\t_\t%s\t%s\n".format(i+1, words(i), tags(i), tags(i)))
					}
					wout.write("\n")
					
					var c = 0
//					for (i <- 0 until rss.size) {println(c + "\t" + rss(i).mkString(", ")); c += 1}
					for (i <- 0 until rbuff.size) {
						for (rr <- rss) {
							if (rr(0) == i+1) {
								rbuff(i) = rbuff(i).replaceAll("<R_PLACE_HOLDER>", rr(1).toString)														
							}
							else if (rr(1) == i+1) {
								rbuff(i) = rbuff(i).replaceAll("<R_PLACE_HOLDER>", rr(0).toString)														
							}
						}					
					}
					println(rbuff.mkString("\n"))
					println
				}
			}
		}
		wout.close
		System.err.println("%d sentences extracted.".format(scount))
	}
	
	
	
	
	
	
	
	if (parser != null) {
		heads = parser.dependencyParse(words.mkString(" ")) match {
			case Some(parse) => parse.map(_.toString)
			case _=> {
				val parse = new Array[String](slen+1)
				for (i <- 0 to slen) parse(i) = "_"
				parse
			}
		}						
		parser.constituencyParse(words.mkString(" ")) match {
			case Some(parse) => {
				tags = parse.tokens.map(_.pos)
				parse.map(_.toString)
			}
			case _=> {
				for (i <- 0 until slen) tags(i) = "ERR"
			}
		}						
	}
	if (tagger != null) {
		tags = tagger.tagTokenizedString(words.mkString(" ")).split(" ").map(s => s.substring(s.lastIndexOf("_")+1))						
	}
	assert(tags.size == slen && heads.size-1 == slen, "Tags (%d) or Parse (%d) do not match slen of %d".format(tags.size, heads.size, slen))
	

*/

