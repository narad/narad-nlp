package narad.nlp.relations

import edu.stanford.nlp.tagger.maxent.MaxentTagger
import edu.stanford.nlp.tagger.maxent.TaggerConfig
import edu.stanford.nlp.parser.lexparser.{ChineseTreebankParserParams, Options}
import edu.stanford.nlp.util.XMLUtils
import java.io._
import narad.nlp.io.SentenceReader
import narad.nlp.parse.Tree
import narad.util.{ArgParser, ChunkReader, StanfordParserWrapper}
import scala.collection.mutable.ArrayBuffer



object AcePreprocessor {

	def main(args: Array[String]) = {
		val options = new ArgParser(args)
		val input      = options.getString("--input")
		val taggerFile = options.getString("--tagger.file")
		val parserFile = options.getString("--parser.file")
		val language   = options.getString("--language", "english")
		val filter     = options.getBoolean("--filter", false)
		val minEntity  = options.getInt("--min.entity", 0)
		val maxEntity  = options.getInt("--max.entity", 7)

		if (parserFile == null) {
			language match {
				case "english" => {
					annotateEnglishRelations(input, taggerFile, filter, minEntity, maxEntity)					
				}
				case "chinese" => {
					annotateChineseRelations(input, taggerFile, filter)
				}
			}
		}
		else {
			language match {
				case "english" => annotateEnglishSyntax(input, parserFile)
				case "chinese" => annotateChineseSyntax(input, parserFile)
			}
		}
	}
	
		def annotateEnglishRelations(input: String, taggerFile: String, filter: Boolean = false, minEntity: Int = 0, maxEntity: Int = 7) = {
			var scount = 0
			val tagger = if (taggerFile != null) new MaxentTagger(taggerFile) else null
			for (file <- filenames(new File(input), _.endsWith(".sgm"))) {
				val filename = file.toString
				val basename = filename.substring(0, filename.size-4)
				val sentences = SentenceReader.read(AceReader.readSGM(basename + ".sgm"))
				val relations = AceReader.readRelations(basename + ".apf.xml").filter(!_.overlaps)
				val entities  = AceReader.readFullMentions(basename + ".apf.xml").filter{e => val s = e.text.split(" ").size; s >= minEntity && s <= maxEntity}
				for (sentence <- sentences) {
					val rels = maximalACERelations(relations.filter(_.matches(sentence)))
					val ents = maximalEntities(entities.filter(_.matches(sentence)), rels)
					val slen1 = sentence.split(" ").size
					if ((rels.size > 0 || (ents.size > 2 && ents.size <= 5)) &&
					    slen1 > 3 && slen1 <= 40) { //} || ents.size > 1) {
						scount += 1
	//					val templates = extractTemplates(sentence, ents, rels)
						val slots = fillSlots(sentence, ents, rels)
						val words = slots.map{c => c(1) } //templates.map(_.word)
						val slen = words.size
						var tags = new Array[String](slen)

						if (tagger != null) {
							tags = tagger.tagTokenizedString(words.mkString(" ")).split(" ").map(s => s.substring(s.lastIndexOf("_")+1))						
						}
						assert(tags.size == slen, "Tags (%d) does not match slen of %d".format(tags.size,  slen))

						var i = 0
						for (s <- slots) {
							s(2) = tags(i)
							s(3) = tags(i)
							i += 1
						}
						println(slots.map(_.mkString("\t")).mkString("\n"))
						println
					}
				}
			}
			System.err.println("%d sentences extracted.".format(scount))
		}
		
			def annotateChineseRelations(input: String, taggerFile: String, filter: Boolean = false) = {
				var scount = 0
//				val props = new Properties
//				val config = new TaggerConfig(props)
				val iformat = "UTF-8"
				val oformat = "UTF-8"
				val out = new PrintStream(System.out, true, oformat)
				val tagger = if (taggerFile != null) new MaxentTagger(taggerFile) else null
				for (file <- filenames(new File(input), _.endsWith(".sgm.tok"))) {
					val filename = file.toString
					val basename = filename.substring(0, filename.size-8)
					val sentences = AceReader.readSGM(basename + ".sgm.tok", iformat).split("_BR_")
//					println("SENTENCES(%d):\n%s\n".format(sentences.size, sentences.mkString("\n")))
					val relations = AceReader.readRelations(basename + ".apf.xml.tok", iformat).filter(!_.overlaps)
//					println("RELATIONS:\n%s\n".format(relations.mkString("\n")))
					val entities  = AceReader.readFullMentions(basename + ".apf.xml.tok", iformat) 
//					println("ENTITIES:\n%s\n".format(entities.mkString("\n")))
					for (sentence <- sentences) {
						val rels = maximalACERelations(relations.filter(_.matches(sentence)))
//						println("Matching RELATIONS:\n%s\n".format(rels.mkString("\n")))
						val ents = maximalEntities(entities.filter(_.matches(sentence)), rels)
//						println("maximal Ents:\n%s\n".format(ents.mkString("\n")))
						val slen1 = sentence.split(" ").size
						if ((rels.size > 0 || (ents.size > 2 && ents.size <= 5)) &&
						    slen1 > 3 && slen1 <= 40) { 
							scount += 1
							val slots = fillSlots(sentence, ents, rels)
							val words = slots.map{c => c(1).replaceAll("\n", "") } 
							val slen = words.size
							var tags = new Array[String](slen)

							if (tagger != null) {
//								println("TAGS: " + tagger.tagTokenizedString(words.mkString(" ")))
								tags = tagger.tagTokenizedString(words.mkString(" ")).split(" ").map(s => s.substring(s.lastIndexOf("#")+1))						
							}
							else {
								for (i <- 0 until tags.size) tags(i) = "XX"
							}
							assert(tags.size == slen, "Tags (%d) does not match slen of %d".format(tags.size,  slen))

							var i = 0
							for (s <- slots) {
								s(2) = tags(i)
								s(3) = tags(i)
								i += 1
							}
							out.println(slots.map(_.mkString("\t").replaceAll("\n", "")).mkString("\n"))
							out.println
						}
					}
				}
				System.err.println("%d sentences extracted.".format(scount))
			}

	def maximalACERelations(relations: Array[ACERelation]): Array[ACERelation] = {
			val maxrels = new ArrayBuffer[ACERelation]
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
	
	def maximalEntities(entities: Array[FullMention], relations: Array[ACERelation]): Array[FullMention] = {
		val maxents = new ArrayBuffer[FullMention]
		val intents = new ArrayBuffer[FullMention]
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

	def fillSlots(sentence: String, ents: Array[FullMention], rels: Array[ACERelation]): Array[Array[String]] = {
//		val templates = new ArrayBuffer[DataTemplate]
		val rindices = rels.map{r => new Tuple2(r, r.matchIndices(sentence))}
		val eindices = ents.map{e => new Tuple2(e, e.matchIndices(sentence))}
		val tokens = sentence.split(" ") 
		val rbuff = new ArrayBuffer[String]
		val rs = new Array[ACERelation](tokens.size)
		val rss = Array.ofDim[Int](rindices.size, 2)
		val words = new ArrayBuffer[String]
		var i = 0
		var j = 0
		val sslots = new ArrayBuffer[Array[String]]//new Array[Array[String]](tokens.size)
		while(i < tokens.size) {
			var found = false
			var rcount = 0
			// 1:ID 2:word 3:pos 4:pos 5:dephead 6:is_ent 7:ent_type 8:ent_subtype 9:mention_type 10:rel_head 11:rel_type 12:rel_subtype 13:arg_slot
			val slots = new Array[String](13)
			for (i <- 0 until slots.size) slots(i) = "_"
			slots(0) = (j+1).toString
			var oi = i
			var oj = j	
			for (r <- rindices) {
				val rel = r._1
				val left = r._2._1
				val right = r._2._2
				if (i == left._1) {
					val combed = r._1.arg1.text.replaceAll(" ", "_")
					slots(1) = combed
					slots(5) = "Y"
					slots(10) = rel.label
					slots(11) = rel.sublabel
					slots(12) = "ARG-1"
					slots(6) = "NA"
					slots(7) = "NA"
					slots(8) = "NA"
					words += combed
					for (e <- eindices) {
						if (i == e._2._1) {
							val ent = e._1
							slots(6) = ent.elabel
							slots(7) = ent.esublabel
							slots(8) = ent.label					
						}													
					}
					i += 1 + r._2._1._2 - r._2._1._1
					j += 1
					rss(rcount)(0) = j
					found = true
				}
				else if (i == right._1) {
					val combed = r._1.arg2.text.replaceAll(" ", "_")
					slots(1) = combed
					slots(5) = "Y"
					slots(10) = rel.label
					slots(11) = rel.sublabel
					slots(12) = "ARG-2"
					slots(6) = "NA"
					slots(7) = "NA"
					slots(8) = "NA"
					words += combed
					for (e <- eindices) {
						if (i == e._2._1) {
							val ent = e._1
							slots(6) = ent.elabel
							slots(7) = ent.esublabel
							slots(8) = ent.label					
						}													
					}
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
						val ent = e._1
						val combed = e._1.text.replaceAll(" ", "_")
						slots(1) = combed
						slots(5) = "Y"
						slots(6) = ent.elabel
						slots(7) = ent.esublabel
						slots(8) = ent.label					
						words += combed
						i += 1 + e._2._2 - e._2._1
						j += 1
						found = true
					}								
				}					
			}
			if (!found) {
				slots(1) = tokens(i)
				words += tokens(i)
				i += 1						
				j += 1
			}
			sslots += slots
		}
		
		for (i <- 0 until sslots.size) {
			for (rr <- rss) {
				if (rr(0) == i+1) {
					sslots(i)(9) = rr(1).toString
				}
				else if (rr(1) == i+1){
					sslots(i)(9) = rr(0).toString
				}
			}
		}
		return sslots.toArray
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

	def annotateEnglishSyntax(input: String, parserFile: String) = {
		val parser = new StanfordParserWrapper(parserFile)
		var scount = 0
		for (chunk <- ChunkReader.read(input) if chunk.split("\n").size > 1) {
			val lines = chunk.split("\n")
			println("!\n%s!".format(lines.mkString("\n")))
			val numEnts = lines.map(_.split("\t")(5)).filter(_ == "Y").size
			if (numEnts > 1) {
				val words = lines.map(_.split("\t")(1))
				val tags = lines.map(_.split("\t")(2))
				val slen = words.size
				var heads = new Array[String](slen+1)	
				var tree = null.asInstanceOf[Tree]
				var parseFailed = false				
				if (parser != null) {
					parser.parses(words.mkString(" ")) match {
						case Some(parsepair) => {
							tree = parsepair._1
							tree.annotateWithIndices(0)
							tree.clearLabels("BRK")
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

	def annotateChineseSyntax(input: String, parserFile: String) = {
		val options = new ChineseTreebankParserParams() 
		val gb = "GB18030"
		val utf = "UTF-8"
		options.setInputEncoding("UTF-8")
		options.setOutputEncoding("UTF-8")
		val parser = new StanfordParserWrapper(parserFile, new Options(options))
		var scount = 0
		
		val iformat = "UTF-8"
		val oformat = "UTF-8"
		val out = new PrintStream(System.out, true, oformat)
		
		for (chunk <- ChunkReader.read(input, "UTF-8") if chunk.split("\n").size > 1) {
			val lines = chunk.split("\n")
//			println("!\n%s!".format(lines.mkString("\n")))
			val numEnts = lines.map{ l =>
				val ll = l.split("\t")
				System.err.println(ll.mkString(","))
				ll(5)
			}.filter(_ == "Y").size
			if (numEnts > 1) {
				val words = lines.map(_.split("\t")(1))
				var tags = lines.map(_.split("\t")(2))
				val slen = words.size
				var heads = new Array[String](slen+1)	
				var tree = null.asInstanceOf[Tree]
				var parseFailed = false				
				if (parser != null) {
//					val sent = words.mkString(" ")
					val sent = convertEncoding(words.mkString(" ").replaceAll("_", ""), utf, utf)
					parser.parses(sent, language="CHINESE") match {
						case Some(parsepair) => {
							tree = parsepair._1
							tree.annotateWithIndices(0)
							tree.clearLabels("BRK")
							tree.setYield(words ++ words, words ++ words)
							tags = tree.tokens.map(_.pos)
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
					out.println("@parse\t" + tree.toString)
					out.println("@deps\t" + heads.mkString(" "))
					for (i <- 0 until lines.size) {
						val cols = lines(i).split("\t")
						cols(3) = tags(i)
						cols(4) = heads(i+1)
						out.println(cols.mkString("\t"))
					}
					out.println					
				}
			}				
		}
		System.err.println("%d sentences extracted.".format(scount))
	} 

	
	def convertEncoding(str: String, oEncoding: String, nEncoding: String): String = {
		return new String(str.getBytes(oEncoding), nEncoding)
	}
}






/*while(i < tokens.size) {
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
	
	
	
	
	
			//var c = 0
			//					for (i <- 0 until rss.size) {println(c + "\t" + rss(i).mkString(", ")); c += 1}
	/*
			for (i <- 0 until slots.size) {
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
	*/












	def annotateACERelations(input: String, taggerFile: String) = {
		var scount = 0
		val minwords = 1
		val maxwords = 8
		val tagger = if (taggerFile != null) new MaxentTagger(taggerFile) else null
		for (file <- filenames(new File(input), _.endsWith(".sgm"))) {
			val filename = file.toString
			val basename = filename.substring(0, filename.size-4)
			val sentences = SentenceReader.read(AceReader.readSGM(basename + ".sgm"))
			val relations = AceReader.readRelations(basename + ".apf.xml").filter(!_.overlaps)
			val entities  = AceReader.readFullMentions(basename + ".apf.xml").filter{e => val s = e.text.split(" ").size; s >= minwords && s <= maxwords}
			for (sentence <- sentences) {
				scount += 1
				val rels = maximalACERelations(relations.filter(_.matches(sentence)))
				val ents = maximalEntities(entities.filter(_.matches(sentence)), rels)
				if (rels.size > 0 || ents.size > 1) {
//					val templates = extractTemplates(sentence, ents, rels)
					val slots = fillSlots(sentence, ents, rels)
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
}

}
*/