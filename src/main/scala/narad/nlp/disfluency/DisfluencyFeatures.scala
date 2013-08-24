package narad.nlp.disfluency

import narad.nlp.ling.{TaggedToken => Token}
import collection.mutable.ArrayBuffer
import narad.nlp.parser.constituent.{ConstituentLabelFeatures, ConstituentParserParams, TreebankStatistics}
import narad.io.tree.{OntoNotesTreebankReaderOptions, TreebankReader}
import narad.bp.util.{GZipWriter, Feature, PotentialExample}
import narad.io.disfluency.DisfluencyReader
import java.io.FileWriter
import narad.bp.util.index._

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 3/24/13
 * Time: 10:52 AM
 * To change this template use File | Settings | File Templates.
 */
trait DisfluencyFeatures extends ConstituentLabelFeatures {

  def extractFeatures(trainNERFile: String, trainSyntaxFile: String, trainFeatureFile: String,
                               stats: TreebankStatistics, index: Index[String], params: ConstituentParserParams) {
    println("Extracting label features (in batch sizes of %d)".format(params.BATCH_SIZE))
    val out = new GZipWriter(trainFeatureFile + ".gz") //new FileWriter(trainFeatureFile)
    val reader = new DisfluencyReader(trainNERFile, trainSyntaxFile) //, new OntoNotesTreebankReaderOptions)
    var startTime = System.currentTimeMillis()
    reader.zipWithIndex.grouped(params.BATCH_SIZE).foreach { batch =>
      val batchArray = batch.toArray
      val pexs = new Array[PotentialExample](batchArray.size)
      batchArray.par.map { case((dis, tree), i) =>
        if (i % params.PRINT_INTERVAL == 0) System.err.print("\r  example %d...[index contains %d elements].".format(i, index.size))
        pexs(i % params.BATCH_SIZE) = getAllFeatures(tree, stats, index, params)
      }
      pexs.foreach { pex =>
        pex.writeToFile(out)
        out.write("\n")
      }
    }
    out.close()
    if (params.TIME) System.err.println("Finished Feature Extraction [%fs.]".format((System.currentTimeMillis() - startTime) / 1000.0))
  }

  /*
  def extractFeatures(disfluencyDir: String, treebankDir: String, outfile: String, tstats: TreebankStatistics, params: DisfluencyParams) {
    val DISFLUENCY_FEATURES = params.PREDICT_DISFLUENCY
    val SYNTAX_FEATURES = params.PREDICT_SYNTAX
    val dreader = new DisfluencyReader(disfluencyDir, treebankDir)
    val out = new FileWriter(outfile)
    System.out.println("Extracting features for %d examples".format(dreader.size))

    val MIN_REPAR_LEN  = 1
    val MIN_INTER_LEN  = 0
    val MIN_REPAIR_LEN = 1

    val MAX_REPAR_LEN  = 5
    val MAX_INTER_LEN  = 5
    val MAX_REPAIR_LEN = 5

    dreader.zipWithIndex.foreach { case(pair, i) =>
      //     println("ex = " + i)
      val dis = pair._1
      val tree = pair._2.removeNones().removeUnaryChains().binarize()
      if (i % params.PRINT_INTERVAL == 0) System.err.println("  example %d...".format(i))
      out.write("@slen\t%d\n".format(dis.slen))
      out.write("@words\t%s\n".format(dis.words.mkString(" ")))
      out.write("@tags\t%s\n".format(dis.tags.mkString(" ")))
      out.write("@line\t%s\n".format(dis.lline))
      out.write("@ints\t%s\n".format(dis.intIndex.mkString(", ")))
      out.write("@repas\t%s\n".format(dis.repas.mkString(", ")))
      out.write("@repairs\t%s\n".format(dis.repairs.mkString(", ")))
      out.write("@aligns\t%s\n".format(dis.alignments.mkString(", ")))
      out.write("@tree\t%s\n".format(tree.toString()))
      out.write("@maxrepar\t%d\n".format(MAX_REPAR_LEN))
      out.write("@maxint\t%d\n".format(MAX_INTER_LEN))
      out.write("@maxrepair\t%d\n".format(MAX_REPAIR_LEN))
      out.write("@minrepar\t%d\n".format(MIN_REPAR_LEN))
      out.write("@minint\t%d\n".format(MIN_INTER_LEN))
      out.write("@minrepair\t%d\n".format(MIN_REPAIR_LEN))
      out.write("@ex\t%d\n".format(i))
      val slen = dis.slen

      //      println(dis.hasAlignment(4,11))
      //      println(dis.hasAlignment(5,12))

      if (SYNTAX_FEATURES) {
        val LABEL_NAME = params.BRACK_LABEL_NAME
        //        System.err.println("Extracting syntax features with label set (%d):\n%s".format(labels.size, labels.mkString("\n")))
        for ( width <- 2 to slen; start <- 0 to (slen - width)) {
          val end = start + width
          val feats = constituentSpanFeatures(tree.tokens.toArray, start, end, params)
          out.write("%s(%d,%d)\t%s%s\n".format("brack", start, end, if (tree.containsSpan(start, end)) "+" else "", feats.mkString(" ")))
          val labels = if (params.PRUNE_SYNTAX) tstats.constituentLabelsOfSize(width) else tstats.constituentLabels
          for (label <- labels) {
            val builder = new StringBuilder()
            val isCorrect = tree.containsSpan(start, end, label)
            for (f <- feats) builder.append(" " + label + "_" + f)
            out.write("%s%s(%d,%d)\t%s%s\n".format(LABEL_NAME, label, start, end, if (isCorrect) "+" else "", builder.toString.trim))
            builder.clear()
          }
        }
      }

      if (DISFLUENCY_FEATURES) {

        val MIN_DIS_LEN = MIN_REPAR_LEN + MIN_INTER_LEN + MIN_REPAIR_LEN
        val MAX_DIS_LEN = MAX_REPAR_LEN + MAX_INTER_LEN + MAX_REPAIR_LEN
        //        for ( width <- 2 to slen; start <- 0 to (slen - width)) {}
        for (width <- 1 to MAX_INTER_LEN; start <- 0 to(slen-width)) {
          val end = start+width
          val feats = interregnumFeats(dis.tokens.toArray, start, end)
          out.write("%s(%d,%d)\t%s%s\n".format("fp", start, end, if (dis.hasInterregnum(start, end)) "+" else "", feats.mkString(" ")))
        }
        for (i <- 0 to slen - (MIN_DIS_LEN); j <- i+1 to i+MAX_REPAR_LEN if j <= slen) {
          //        println("iter: " + i + " " + j)
          for (k <- j+MIN_INTER_LEN to j+MAX_INTER_LEN; l <- k+MIN_REPAIR_LEN to k+MAX_REPAIR_LEN if l <= slen) {
            val disCorrect = (dis.hasReparandum(i,j) && dis.hasRepair(k,l) && (j==k || dis.hasInterregnum(j,k)))
            val disFeats = DisfluencyFeatures(dis.tokens.toArray, i, j, k, l)
            out.write("%s(%d,%d,%d,%d)\t%s%s\n".format("dis", i, j, k, l, if (disCorrect) "+" else "", disFeats.mkString(" ")))
            if (i+1 == j && k+1 == l) {   // Word Copy
            val simCorrect = dis.hasAlignment(i, k) //(dis.hasReparandum(i,j) && dis.hasRepair(k,l))
            val feats = similarityWordFeats(dis.tokens.toArray, i, k)
              out.write("%s(%d,%d,%d,%d)\t%s%s\n".format("str-copy", i, j, k, l, if (simCorrect) "+" else "", feats.mkString(" ")))
            }
            else {   // Partial Copy
            val simCorrect = (dis.hasReparandum(i,j) && dis.hasRepair(k,l))
              val simFeats = similarityFeats(dis.tokens.toArray, i, j, k, l)
              out.write("%s(%d,%d,%d,%d)\t%s%s\n".format("str-copy", i, j, k, l, if (simCorrect) "+" else "", simFeats.mkString(" ")))
            }
          }
        }
      }
      out.write("\n")
    }
    out.close()
  }
  */

  def DisfluencyFeatures(tokens: Array[Token], i: Int, j: Int, k: Int, l: Int): Array[String] = {
    val ab = new ArrayBuffer[String]()
    ab += "[form-form]-%s-%s".format(tokens(i).word, tokens(j).word)
    if (tokens(i).word == tokens(j).word) ab += "[ident-form]"
    if (tokens(i).pos == tokens(j).pos) ab += "[ident-pos]"

    ab += "[form-form-gap]-%s-%s-%d".format(tokens(i).word, tokens(j).word, j-i)
    if (tokens(i).word == tokens(j).word) ab += "[ident-form-gap]-%d".format(j-i)
    if (tokens(i).pos == tokens(j).pos) ab += "[ident-pos-gap]-%d".format(j-i)
    ab.toArray
  }

  def similarityWordFeats(tokens: Array[Token], i: Int, j: Int): Array[String] = {
    val ab = new ArrayBuffer[String]()
    ab += "[form-form]-%s-%s".format(tokens(i).word, tokens(j).word)
    if (tokens(i).word == tokens(j).word) ab += "[ident-form]"
    if (tokens(i).pos == tokens(j).pos) ab += "[ident-pos]"

    ab += "[form-form-gap]-%s-%s-%d".format(tokens(i).word, tokens(j).word, j-i)
    if (tokens(i).word == tokens(j).word) ab += "[ident-form-gap]-%d".format(j-i)
    if (tokens(i).pos == tokens(j).pos) ab += "[ident-pos-gap]-%d".format(j-i)
    ab.toArray
  }

  def similarityFeats(tokens: Array[Token], start1: Int, end1: Int, start2: Int, end2: Int): Array[String] = {
    val ab = new ArrayBuffer[String]()
    ab += tokens.map(_.word).slice(start1,end1).mkString("-") + "-SIMTO-" + tokens.map(_.word).slice(start2,end2).mkString("-")
    ab += tokens.map(_.pos).slice(start1,end1).mkString("-") + "-SIMTO-" + tokens.map(_.pos).slice(start2,end2).mkString("-")
    if (end1 == start1+1 && end2 == start2+1 && tokens(start1).word == tokens(start2).word) {
      ab += "[same-word]"
    }
    ab.toArray
  }

  def interregnumFeats(tokens: Array[Token], start: Int, end: Int): Array[String] = {
    val buf = new ArrayBuffer[String]()
    buf += "inter-" + tokens.map(_.word).slice(start, end).mkString("-")
    buf += "inter-" + tokens.map(_.pos).slice(start, end).mkString("-")
    buf.toArray
  }

}
