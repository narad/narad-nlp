package narad.nlp.parser.constituent

import java.io.FileWriter
import narad.io.tree.TreebankReader
import narad.nlp.trees.{ConstituentTree => Tree}
import collection.mutable.{HashMap, ArrayBuffer, Map}
import narad.bp.util.{Feature, PotentialExample}
import narad.bp.structure.Potential
import narad.bp.util.index._

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 12/16/12
 * Time: 5:29 AM
 * To change this template use File | Settings | File Templates.
 */


class ConstituentLabelParser(params: ConstituentParserParams) extends ConstituentParser(params) with ConstituentLabelFeatures

trait ConstituentLabelFeatures extends ConstituentBracketFeatures {

  override def extractFeatures(trainFile: String, trainFeatureFile: String, stats: TreebankStatistics, index: Index[String], params: ConstituentParserParams) = {
    println("Extracting label features")
    val out = new FileWriter(trainFeatureFile)
    val reader = new TreebankReader(trainFile)
    var startTime = System.currentTimeMillis()
    reader.zipWithIndex.foreach { case(tree, i) =>
      if (i % params.PRINT_INTERVAL == 0) System.err.println("  example %d...".format(i))
      val slen = tree.slen
      out.write("@slen\t%d\n".format(slen))
      out.write("@words\t%s\n".format(tree.words.mkString(" ")))
      out.write("@tags\t%s\n".format(tree.tags.mkString(" ")))
      val btree = tree.binarize.removeUnaryChains
      btree.annotateWithIndices()
      val get = false
      if (get) {
        val f1 = getBracketFeatures(btree, index, params)
        f1.writeToFile(out)
        val f2 = getLabelFeatures(btree, stats, index, params)
        f2.writeToFile(out)
        val f3 = getUnaryFeatures(btree, stats, params)
        f3.writeToFile(out)
      }
      else {
        extractBracketFeatures(btree, out, params)
        extractLabelFeatures(btree, stats, out, params)
        extractUnaryFeatures(tree, stats, out, params)
      }
      out.write("\n")
    }
    out.close()
    if (params.TIME) System.err.println("Finished Feature Extraction [%fs.]".format((System.currentTimeMillis() - startTime) / 1000.0))
  }

 /*
    override def extractFeatures(trainFile: String, trainFeatureFile: String, stats: TreebankStatistics, params: ConstituentParserParams) = {
    val out = new FileWriter(trainFeatureFile)
    val util = new TreebankReader(trainFile)
    var startTime = System.currentTimeMillis()
    util.zipWithIndex.foreach { case(tree, i) =>
      if (i % params.PRINT_INTERVAL == 0) System.err.println("  example %d...".format(i))
      val slen = tree.slen
      out.write("@slen\t%d\n".format(slen))
      out.write("@words\t%s\n".format(tree.words.mkString(" ")))
      out.write("@tags\t%s\n".format(tree.tags.mkString(" ")))
      val btree = tree.binarize.removeUnaryChains
      btree.annotateWithIndices()
      extractBracketFeatures(btree, out, params)
      extractLabelFeatures(btree, stats, out, params)
      extractUnaryFeatures(tree, stats, out, params)
      out.write("\n")
    }
    if (params.TIME) System.err.println("Finished Feature Extraction [%fs.]".format((System.currentTimeMillis() - startTime) / 1000.0))
  }
  */

  def getLabelFeatures(tree: Tree, stats: TreebankStatistics, index: Index[String], params: ConstituentParserParams): PotentialExample = {
    val attributes  = Map[String, String]()
    val potentials = new ArrayBuffer[Potential]
    val featureMap = new HashMap[String, Array[Feature]]

    val LABEL_NAME = params.BRACK_LABEL_NAME
    val labels = stats.constituentLabels.toArray
    val slen = tree.slen
    for ( width <- 2 to slen; start <- 0 to (slen - width)) {
      val end = start + width
      val feats = constituentSpanFeatures(tree.tokens(), start, end, params)
      for (label <- labels) {
        val isCorrect = tree.containsSpan(start, end, label)
        val potname = "%s%s(%d,%d)".format(LABEL_NAME, label, start, end)
        potentials += new Potential(1.0, potname, isCorrect)
        featureMap(potname) = feats.map(f => new Feature(index.index(label + "_" + f), 1.0, 0))
       // out.write("%s%s(%d,%d)\t%s%s\n".format(LABEL_NAME, label, start, end, if (isCorrect) "+" else "", builder.toString.trim))
      }
    }
    new PotentialExample(attributes, potentials, featureMap)
  }


    def extractLabelFeatures(tree: Tree, stats: TreebankStatistics, out: FileWriter, params: ConstituentParserParams) {
    val LABEL_NAME = params.BRACK_LABEL_NAME
    val labels = stats.constituentLabels.toArray
    val slen = tree.slen
    for ( width <- 2 to slen; start <- 0 to (slen - width)) {
      val end = start + width
      val feats = constituentSpanFeatures(tree.tokens(), start, end, params)
      val builder = new StringBuilder()
      for (label <- labels) {
        val isCorrect = tree.containsSpan(start, end, label)
        for (f <- feats) builder.append(" " + label + "_" + f)
        out.write("%s%s(%d,%d)\t%s%s\n".format(LABEL_NAME, label, start, end, if (isCorrect) "+" else "", builder.toString.trim))
        builder.clear()
      }
    }
  }




  def extractUnaryFeatures(tree: Tree, stats: TreebankStatistics, out: FileWriter, params: ConstituentParserParams) {
    val UNARY_NAME = params.UNARY_NAME
    val UNARY_LABEL_NAME = params.UNARY_LABEL_NAME
    val labels = stats.unaryLabels.toArray
    val slen = tree.slen
    for (idx <- 0 until slen) {
      val features = unaryFeatures(tree, idx)
      val hasUnary = tree.containsUnarySpan(idx, idx+1)
      out.write("%s(%d,%d)\t%s%s\n".format(UNARY_NAME, idx, idx+1, if (hasUnary) "+" else "", features.map("U-%s".format(_)).mkString(" ")))
      for (ulabel <- 0 until labels.size) {
        val correctLabel = tree.containsUnarySpan(idx, idx+1, labels(ulabel))
        val builder = new StringBuilder()
        for (f <- features) builder.append(" [unary-" + ulabel + "]-" + f)
        out.write("%s%s(%d,%d)\t%s%s\n".format(UNARY_LABEL_NAME, labels(ulabel), idx, idx+1, if (correctLabel) "+" else "", builder.toString.trim))
        builder.clear()
      }
    }
  }

  def getUnaryFeatures(tree: Tree, stats: TreebankStatistics, params: ConstituentParserParams): PotentialExample = {
    val attributes  = Map[String, String]()
    val potentials = new ArrayBuffer[Potential]
    val featureMap = new HashMap[String, Array[Feature]]
    val UNARY_NAME = params.UNARY_NAME
    val UNARY_LABEL_NAME = params.UNARY_LABEL_NAME
    val labels = stats.unaryLabels.toArray
    val slen = tree.slen
    for (idx <- 0 until slen) {
      val features = unaryFeatures(tree, idx)
      val hasUnary = tree.containsUnarySpan(idx, idx+1)
      //out.write("%s(%d,%d)\t%s%s\n".format(UNARY_NAME, idx, idx+1, if (hasUnary) "+" else "", features.map("U-%s".format(_)).mkString(" ")))
      val potname = "%s(%d,%d)".format(UNARY_NAME, idx, idx+1)
      potentials += new Potential(1.0, potname, hasUnary)
      featureMap(potname) = features.map(f => new Feature(f.size, 1.0, 0))

      for (ulabel <- 0 until labels.size) {
        val correctLabel = tree.containsUnarySpan(idx, idx+1, labels(ulabel))
        val ulpotname = "%s%s(%d,%d)".format(UNARY_LABEL_NAME, labels(ulabel), idx, idx+1)
        featureMap(potname) = features.map(f => new Feature((labels(ulabel) + "_" + f).size, 1.0, 0))
      }
    }
    new PotentialExample(attributes, potentials, featureMap)
  }

  def unaryFeatures(tree: Tree, index: Int, treeFeatures:Boolean = false, window: Int=4): Array[String] = {
    val features = new ArrayBuffer[String]
    val tokens = pad(tree.tokens(), window, window)
    features += "BIAS"
    // Simple Features
    features += "INDEX-%d".format(index)
    features += "LENGTH-%d".format(index)
    var ccount = 0
    for (i <- index to index+window+window) {
      ccount += 1
      features ++= unigramFeatures(tokens(i))
      features ++= unigramFeatures(tokens(i)).map("%d-%s".format(ccount, _))
    }
    features.toArray
  }
}










/*
  if (useUnaries) {
    val uterms = stats.unaryLabels.toArray //clabels.filter(!_.contains("@"))
    val tokens = ctree.tokens
    for (idx <- 0 until length) {
      val features = ConstituentFeatureFactory.unaryFeatures(ctree, idx) //tree.removeUnaryChains, idx)
      val hasUnary = ctree.containsUnarySpan(idx, idx+1)
      out.write("%s(%d,%d)\t%s%s\n".format(uspanName, idx, idx+1, if (hasUnary) "+" else "", features.map("U-%s".format(_)).mkString(" ")))
      var ccount = 0
      for (uterm <- uterms) {
        val correctLabel = ctree.containsUnarySpan(idx, idx+1, uterm)
        if (correctLabel) {
          ccount += 1
        }
        val builder = new StringBuilder()
        for (f <- features) {
          builder.append(" [unary-" + uterm + "]-" + f)
        }
        if (bpdp) {
          out.write("%s%s(%d,%d)\t%s%s\n".format(ulabelName, uterm, idx, idx+1, if (correctLabel) "+" else "", builder.toString.trim))
        }
        else {
          out.write("%s(%d,%d,%s)\t%s%s\n".format(ulabelName, idx, idx+1, uterm, if (correctLabel) "+" else "", builder.toString.trim))
        }
      }
    }
  }

  out.write("\n")
}


}

*/