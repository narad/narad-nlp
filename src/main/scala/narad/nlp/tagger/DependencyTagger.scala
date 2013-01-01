package narad.nlp.tagger

import narad.bp.structure._
import narad.bp.optimize._
import narad.bp.util._
import narad.io.conll._
import narad.nlp.parser.dependency._
import java.io._
import scala.collection.mutable.ArrayBuffer

class DependencyTagger(params: TaggerParams) extends Tagger(params) with DependencyTaggerFeatures {}

trait DependencyTaggerFeatures extends TaggerFeatures with DependencyParseFeatures {
	
	override def extractFeatures(trainFile: String, trainFeatureFile: String, dict: TagDictionary, params: TaggerParams) = {
		val useIndices = false
		val in = trainFile
		val alltags = dict.all.toArray
		val out = new FileWriter(trainFeatureFile)
		val rout = new FileWriter(trainFeatureFile + ".bpdp")
		val oracle = params.FEATURE_MODE == "ORACLE"
    val reader = new CoNLLReader(in)
		reader.zipWithIndex.foreach { case(datum, i) =>
			println("Found datum:")
			println(datum)
			if (i % params.PRINT_INTERVAL == 0) System.err.println("  example %d...".format(i))
			val slen = datum.slen
			out.write("@slen\t%d\n".format(slen))
			out.write("@words\t%s\n".format(datum.words.mkString(" ")))
      out.write("@oracle\ttrue\n")
      val heads = datum.heads.toArray.zipWithIndex.filter(_._1 > 0)
			if (oracle) out.write("@heads\t%s\n".format(heads.map(_._1).mkString(" ")))
			if (oracle) out.write("@children\t%s\n".format(heads.map(_._2 + 1).mkString(" ")))
			rout.write("@slen\t%d\n".format(slen))
			rout.write("@words\t%s\n".format(datum.words.mkString(" ")))
      rout.write("@oracle\ttrue\n")
      if (oracle) rout.write("@heads\t%s\n".format(heads.map(_._1).mkString(" ")))
			if (oracle) rout.write("@children\t%s\n".format(heads.map(_._2 + 1).mkString(" ")))
			extractUnigramFeatures(datum, dict, out, rout)
			extractDependencyFeatures(datum, oracle, out, rout)
			extractConnectionFeatures(datum, dict, oracle, out, rout)
			out.write("\n")
			rout.write("\n")
		}
		out.close()
		rout.close()
	}
	
	def extractUnigramFeatures(datum: CoNLLDatum, dict: TagDictionary, out1: FileWriter, out2: FileWriter) = {
		datum.words.zipWithIndex.foreach { case(word, widx) =>
			val feats = unigramFeatures(datum, widx+1, useMorph=false, useSyntax=false)
			val tags = if (dict.contains(word)) dict.tags(word).toArray else dict.all.toArray
			tags.zipWithIndex.foreach { case(tag, tidx) => 
				val builder = new StringBuilder()
				for (f <- feats) builder.append(" " + tag + "_" + f)
				val correct = datum.postag(widx+1) == tag
				out1.write("ulabel(%d,%s)\t%s%s\n".format(widx+1, tag, if (correct) "+" else "", builder.toString().trim))
				out2.write("ulabel(%d,%d)\t%s%s\n".format(widx+1, tidx, if (correct) "+" else "", builder.toString().trim))
			}
		}		
	}
	
	def extractDependencyFeatures(datum: CoNLLDatum, oracle: Boolean, out1: FileWriter, out2: FileWriter) = {
		val slen = datum.slen
		for (i <- 0 to slen; j <- 1 to slen if i != j) {
			val feats = if (oracle) Array("NONE") else dependencyFeatures(datum, i, j)
			val builder = new StringBuilder()
			for (f <- feats) builder.append(" " + f)
			val correct = oracle && datum.head(j) == i
			out1.write("un(%d,%d)\t%s%s\n".format(i, j, if (correct) "+" else "", builder))								
			out2.write("un(%d,%d)\t%s%s\n".format(i, j, if (correct) "+" else "", builder))											
		}
	}
	
	def extractConnectionFeatures(datum: CoNLLDatum, dict: TagDictionary, oracle: Boolean, out1: FileWriter, out2: FileWriter) = {
		val slen  = datum.slen
		for (i <- 1 to slen; j <- 1 to slen if i != j && (!oracle || datum.head(j) == i)) {
			val w1 = datum.word(i)
			val w2 = datum.word(j)
			val feats = connectionFeatures(datum, i, j)

			val tags1 = if (dict.contains(w1)) dict.tags(w1).toArray else dict.all.toArray
			val tags2 = if (dict.contains(w2)) dict.tags(w2).toArray else dict.all.toArray
			val gtag1 = datum.postag(i)
			val gtag2 = datum.postag(j)
			for (t1 <- 0 until tags1.size; t2 <- 0 until tags2.size) {
				val tag1 = tags1(t1)
				val tag2 = tags2(t2)
				val correct = oracle && gtag1 == tags1(t1) && gtag2 == tags2(t2) && datum.head(j) == i //false
				val builder = new StringBuilder()
				for (f <- feats) builder.append(" " + tag1 + "_" + tag2 + "_" + f)

        val ii = if (i < j) i else j
        val jj = if (i < j) j else i
        val tt1 = if (i < j) tag1 else tag2
        val tt2 = if (i < j) tag2 else tag1
        val ti1 = if (i < j) t1 else t2
        val ti2 = if (i < j) t2 else t1
        out1.write("blabel(%d,%d,%s,%s)\t%s%s\n".format(ii, jj, tt1, tt2,  if (correct) "+" else "", builder.toString().trim))
        out2.write("blabel(%d,%d,%d,%d)\t%s%s\n".format(ii, jj, ti1, ti2,  if (correct) "+" else "", builder.toString().trim))

        //        out1.write("connect(%d,%d,%s,%s)\t%s%s\n".format(i, j, tag1, tag2, if (correct) "+" else "", builder.toString.trim))
//				out2.write("connect(%d,%d,%d,%d)\t%s%s\n".format(i, j, t1, t2, if (correct) "+" else "", builder.toString.trim))
			}
		}
	}
	
/*
	def dependencyFeatures(datum: CoNLLDatum, head: Int, modifier: Int): Array[String] = {
		return Array("LBIAS")
	}
*/
	
	def connectionFeatures(datum: CoNLLDatum, head: Int, dep: Int): Array[String] = {
		val feats = new ArrayBuffer[String]
		val dir  = if (dep > head) "R" else "L"
		val dist = Math.abs(head - dep)
		feats += "dist-%d".format(dist)
		feats += "dir-%s".format(dir)
		feats += "dist-%d-dir-%s".format(dist, dir)
		feats += "head-word-%s".format(datum.word(head))
		feats += "dep-word-%s".format(datum.word(dep))
		feats += "head-word-%s-dep-word-%s".format(datum.word(head), datum.word(dep))
		feats += "head-word-%s-dep-word-%s-dist-%d".format(datum.word(head), datum.word(dep), dist)
		feats += "head-word-%s-dep-word-%s-dir-%s".format(datum.word(head), datum.word(dep), dir)
		feats += "head-word-%s-dep-word-%s-dist-%d-dir-%s".format(datum.word(head), datum.word(dep), dist, dir)
		
		feats.toArray
	}
}



