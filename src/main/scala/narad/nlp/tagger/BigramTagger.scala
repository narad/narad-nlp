package narad.nlp.tagger

import narad.bp.structure._
import narad.bp.optimize._
import narad.bp.util._
import narad.io.conll._
import java.io._

class BigramTagger(params: TaggerParams) extends Tagger(params) with BigramTaggerFeatures 

trait BigramTaggerFeatures extends TaggerFeatures {

	override def extractFeatures(trainFile: String, trainFeatureFile: String, dict: TagDictionary, params: TaggerParams) = {
		val in = trainFile
		val out = new FileWriter(trainFeatureFile)
		val rout = new FileWriter(trainFeatureFile + ".bpdp")
		val alltags = dict.all.toArray
    val reader = new CoNLLReader(in)
		reader.zipWithIndex.foreach { case(datum, i) =>
			if (i % params.PRINT_INTERVAL == 0) System.err.println("  example %d...".format(i))
			val slen = datum.slen
			out.write("@slen\t%d\n".format(slen))
			out.write("@words\t%s\n".format(datum.words.mkString(" ")))
			out.write("@bigram\ttrue\n")
			rout.write("@slen\t%d\n".format(slen))
			rout.write("@words\t%s\n".format(datum.words.mkString(" ")))
			rout.write("@bigram\ttrue\n")
			datum.words.map(_.toLowerCase).zipWithIndex.foreach { case(word, widx) =>
				val feats = unigramFeatures(datum, widx+1, useMorph=false, useSyntax=false)
				val tags = if (dict.contains(word)) dict.tags(word).toArray else alltags
				tags.zipWithIndex.foreach { case(tag, tidx) => 
					val builder = new StringBuilder()
					for (f <- feats) builder.append(" " + tag + "_" + f)
          val isCorrect1 = tag == correct(datum, widx+1, params.MODE) //datum.postag(widx+1) == tag
          out.write("ulabel(%d,%s)\t%s%s\n".format(widx+1, tag, if (isCorrect1) "+" else "", builder.toString().trim))
					rout.write("ulabel(%d,%d)\t%s%s\n".format(widx+1, tidx, if (isCorrect1) "+" else "", builder.toString().trim))
					if (widx > 0) {
						val prevword = datum.word(widx).toLowerCase
           //println("word = " + word + "; prevword = " + prevword)
						val bigramfeats = bigramFeatures(datum, widx, widx+1, useMorph=false, useSyntax=false)
						val tags = if (dict.contains(prevword)) dict.tags(prevword).toArray else alltags
						tags.zipWithIndex.foreach { case(btag, btidx) => 
							builder.clear()
							for (f <- bigramfeats) builder.append(" " + btag + "_" + tag + "_" + f)
							val isCorrect2 = correct(datum, widx+1, params.MODE) == tag && correct(datum, widx, params.MODE) == btag //datum.postag(widx+1) == tag && datum.postag(widx) == btag
							out.write("blabel(%d,%d,%s,%s)\t%s%s\n".format(widx, widx+1, btag, tag,  if (isCorrect2) "+" else "", builder.toString().trim))
							rout.write("blabel(%d,%d,%d,%d)\t%s%s\n".format(widx, widx+1, btidx, tidx,  if (isCorrect2) "+" else "", builder.toString().trim))
						}
					}	
				}
			}
			out.write("\n")
			rout.write("\n")
		}
		out.close()
		rout.close()
	}
}



//						dict.tags.getOrElse(prevword, alltags).zipWithIndex.foreach { case(btag, btidx) =>
