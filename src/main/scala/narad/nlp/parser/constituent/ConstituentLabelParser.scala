package narad.nlp.parser.constituent

import java.io.FileWriter
import narad.io.reader.TreebankReader
import narad.nlp.trees.ConstituentTree
import collection.mutable.ArrayBuffer

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 12/16/12
 * Time: 5:29 AM
 * To change this template use File | Settings | File Templates.
 */
class ConstituentLabelParser(params: ConstituentParserParams) extends ConstituentParser(params) with ConstituentLabelFeatures

trait ConstituentLabelFeatures extends ConstituentBracketFeatures {

  override def extractFeatures(trainFile: String, trainFeatureFile: String, labels: Array[String],
                               ulabels: Array[String], params: ConstituentParserParams) = {
    val out = new FileWriter(trainFeatureFile)
    val reader = new TreebankReader(trainFile)
    reader.zipWithIndex.foreach { case(tree, i) =>
      if (i % params.PRINT_INTERVAL == 0) System.err.println("  example %d...".format(i))
      val slen = tree.slen
      out.write("@slen\t%d\n".format(slen))
      val btree = tree.binarize.removeUnaryChains
      btree.annotateWithIndices()
      extractBracketFeatures(btree, out, params)
      extractLabelFeatures(btree, labels, out, params)
      extractUnaryFeatures(tree, ulabels, out, params)
      out.write("\n")
    }
  }

  def extractLabelFeatures(tree: ConstituentTree, labels: Array[String], out: FileWriter, params: ConstituentParserParams) {
    val LABEL_NAME = params.LABEL_NAME
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

  def extractUnaryFeatures(tree: ConstituentTree, labels: Array[String], out: FileWriter, params: ConstituentParserParams) {
    val UNARY_NAME = params.UNARY_NAME
    val slen = tree.slen
    for (idx <- 0 until slen) {
      val features = unaryFeatures(tree, idx)
      val hasUnary = tree.containsUnarySpan(idx, idx+1)
      out.write("%s(%d,%d)\t%s%s\n".format("unary", idx, idx+1, if (hasUnary) "+" else "", features.map("U-%s".format(_)).mkString(" ")))
      for (ulabel <- 0 until labels.size) {
        val correctLabel = tree.containsUnarySpan(idx, idx+1, labels(ulabel))
        val builder = new StringBuilder()
        for (f <- features) builder.append(" [unary-" + ulabel + "]-" + f)
        out.write("%s%s(%d,%d)\t%s%s\n".format(UNARY_NAME, ulabel, idx, idx+1, if (correctLabel) "+" else "", builder.toString.trim))
        builder.clear()
      }
    }
  }

  def unaryFeatures(tree: ConstituentTree, index: Int, treeFeatures:Boolean = false, window: Int=4): Array[String] = {
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