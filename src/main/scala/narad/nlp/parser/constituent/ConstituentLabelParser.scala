package narad.nlp.parser.constituent

import java.io.FileWriter
import narad.io.tree.TreebankReader
import narad.nlp.trees.{ConstituentTree => Tree}
import collection.mutable.{HashMap, ArrayBuffer, Map}
import narad.bp.util.{GZipWriter, Feature, StringFeature, PotentialExample}
import narad.bp.structure.Potential
import narad.bp.util.index._
import narad.io.tree.OntoNotesTreebankReaderOptions
import narad.util.StringOps

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 12/16/12
 * Time: 5:29 AM
 * To change this template use File | Settings | File Templates.
 */


class ConstituentLabelParser(params: ConstituentParserParams) extends ConstituentParserModel(params) with ConstituentLabelFeatures

trait ConstituentLabelFeatures extends ConstituentBracketFeatures {

  override def extractFeatures(trainFile: String, trainFeatureFile: String, stats: TreebankStatistics, index: Index[String], params: ConstituentParserParams) = {
    println("Extracting label features (in batch sizes of %d)".format(params.BATCH_SIZE))
    println("  windo of " + params.FEATURE_WINDOW)
    val out = new GZipWriter(trainFeatureFile + ".gz") //new FileWriter(trainFeatureFile)
    val reader = new TreebankReader(trainFile, new OntoNotesTreebankReaderOptions)
    var startTime = System.currentTimeMillis()
    reader.zipWithIndex.grouped(params.BATCH_SIZE).foreach { batch =>
      val batchArray = batch.toArray
      val pexs = new Array[PotentialExample](batchArray.size)
      batchArray.par.map { case(tree, i) =>
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


  def getAllFeatures(tree: Tree, stats: TreebankStatistics, index: Index[String], params: ConstituentParserParams): PotentialExample = {
    val btree = if (params.BINARIZE_MODE == "MARG") {
      tree.removeNones().removeUnaryChains()
    }
    else {
      tree.removeNones().removeUnaryChains().binarize(params.BINARIZE_MODE)
    }
    val ex = new PotentialExample
    ex.attributes("slen") = tree.length.toString
    ex.attributes("words") = tree.words.mkString(" ")
    ex.attributes("tags") = tree.tags.mkString(" ")
    ex.attributes("tree") = tree.toString()
    ex.attributes("btree") = btree.toString()
    ex.attributes("spans") = btree.toSpans.mkString(" ")
    ex.attributes("labels") = stats.tagset.toArray.mkString(" ") //btree.toSpans.map(_.label).toArray.distinct.sortBy(_.toString).mkString(" ")
    val f1 = getBracketFeatures(btree, index, params)
    val f2 = getLabelFeatures(btree, stats, index, params)
    val f3 = getUnaryFeatures(tree, stats, index, params)
    ex.potentials ++= f1.potentials ++ f2.potentials ++ f3.potentials
    ex.features ++= f1.features ++ f2.features ++ f3.features
    ex
     //new PotentialExample(attributes, f1.potentials ++ f2.potentials ++ f3.potentials, f1.features ++ f2.features ++ f3.features)
  }

  def getLabelFeatures(tree: Tree, stats: TreebankStatistics, index: Index[String], params: ConstituentParserParams): PotentialExample = {

    val ex = new PotentialExample
    //val attributes  = Map[String, String]()
    //val potentials = new ArrayBuffer[Potential]
    //val featureMap = new HashMap[String, Array[Feature]]

    val LABEL_NAME = params.BRACK_LABEL_NAME
    val tagset = stats.tagset.toArray
 //   println("TTAGSET = " + tagset.mkString(" "))
    val slen = tree.length
    val tokens = tree.tokens.toArray
    for ( width <- 2 to slen; start <- 0 to (slen - width)) {
      val labels = if (params.PRUNE) stats.constituentLabelsOfSize(width) else stats.constituentLabels
//      println(tree.toString)
//      println("labels = " + labels.mkString(", "))
//      println
      val end = start + width
      val feats = constituentSpanFeatures(tokens, start, end, params, tagset)
      for (label <- labels) {
 //       println(" checking label %s for %d, %d".format(label, start, end))
        val isCorrect = tree.containsSpan(start, end, label)
        val potname = "%s%s(%d,%d)".format(LABEL_NAME, label, start, end)
        ex.potentials += new Potential(1.0, potname, isCorrect)
        ex.features(potname) = feats.map{f =>
          if (params.INTEGERIZE) {
            new Feature(index.index(label + "_" + f), 1.0, 0)
          }
          else {
            new StringFeature(label + "_" + f, index.index(label + "_" + f), 1.0, 0)
          }
        }
      }
    }
    ex
  }

  //new PotentialExample(attributes, potentials, featureMap)
  // out.write("%s%s(%d,%d)\t%s%s\n".format(LABEL_NAME, label, start, end, if (isCorrect) "+" else "", builder.toString.trim))




  def getUnaryFeatures(tree: Tree, stats: TreebankStatistics, index: Index[String], params: ConstituentParserParams): PotentialExample = {
    val ex = new PotentialExample
    //val attributes  = Map[String, String]()
    //val potentials = new ArrayBuffer[Potential]
    //val featureMap = new HashMap[String, Array[Feature]]
    val UNARY_NAME = params.UNARY_NAME
    val UNARY_LABEL_NAME = params.UNARY_LABEL_NAME
    val labels = stats.unaryLabels.toArray
    val slen = tree.length
    for (idx <- 0 until slen) {
      val features = unaryFeatures(tree, idx)
      val hasUnary = tree.containsUnarySpan(idx, idx+1)
      val potname = "%s(%d,%d)".format(UNARY_NAME, idx, idx+1)
      ex.potentials += new Potential(1.0, potname, hasUnary)
      ex.features(potname) = features.map { f =>
        if (params.INTEGERIZE) {
          new Feature(index.index(f), 1.0, 0)
        }
        else {
          new StringFeature(f, index.index(f), 1.0, 0)
        }
      }
      for (ulabel <- 0 until labels.size) {
        val correctLabel = tree.highestUnarySpan(idx, idx+1) == labels(ulabel) //containsUnarySpan(idx, idx+1, labels(ulabel), 1)
        val ulpotname = "%s%s(%d,%d)".format(UNARY_LABEL_NAME, labels(ulabel), idx, idx+1)
        ex.potentials += new Potential(1.0, ulpotname, correctLabel)
        ex.features(ulpotname) = features.map { f =>
          if (params.INTEGERIZE) {
            new Feature(index.index(labels(ulabel) + "_" + f), 1.0, 0)
          }
          else {
            new StringFeature(labels(ulabel) + "_" + f, index.index(labels(ulabel) + "_" + f), 1.0, 0)
          }
        }
      }
    }
    ex // new PotentialExample(attributes, potentials, featureMap)
  }

  def unaryFeatures(tree: Tree, index: Int, treeFeatures:Boolean = false, window: Int=1): Array[String] = {
    val features = new ArrayBuffer[String]
//    val tokens = pad(tree.tokens(), window, window)
    val tokens = tree.tokens.toArray
    val slen = tokens.size
    features += "UBIAS"
    features += "UWORD-%s".format(tokens(index).word)
    features += "UTAG-%s".format(tokens(index).pos)
    features += "UBOTH-%s-%s".format(tokens(index).word, tokens(index).pos)
    if (isCapitalized(tokens(index).word)) features += "UCAP"
    if (isCapitalized(tokens(index).word)) features += "UCAP-%s".format(tokens(index).pos)
    if (index == 0) features += "U-FIRST"
    if (index > 0) {
      features += "U-2-PRETAG-%s-%s".format(tokens(index-1).pos, tokens(index).pos)
    }
    if (index < slen-1) {
      features += "U-2-POSTAG-%s-%s".format(tokens(index).pos, tokens(index+1).pos)
    }
    if (index > 0 && index < slen-1) {
      features += "U-3-TAG-%s-%s-%s".format(tokens(index-1).pos, tokens(index).pos, tokens(index+1).pos)
    }
    features.toArray
  }
}

































//    features += "LENGTH-%d".format(index)
/*
    var ccount = 0
    for (i <- index to index+window+window) {
      ccount += 1
      features ++= unigramFeatures(tokens(i-window))
      features ++= unigramFeatures(tokens(i-window)).map("%d-%s".format(ccount, _))
    }
    */












/*
    reader.zipWithIndex.foreach { case(tree, i) =>
      if (i % params.PRINT_INTERVAL == 0) System.err.println("  example %d...".format(i))
      val pe = getAllFeatures(tree, stats, index, params)
      pe.writeToFile(out)
      out.write("\n")
    }
*/

//      val get = false
//      if (get) {
//      }
/*
      else {
        //extractBracketFeatures(btree, out, params)
        extractLabelFeatures(btree, stats, out, params)
        //extractUnaryFeatures(tree, stats, out, params)
      }
*/


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
/*
def extractLabelFeatures(tree: Tree, stats: TreebankStatistics, out: FileWriter, params: ConstituentParserParams) {
  val LABEL_NAME = params.BRACK_LABEL_NAME
  val labels = stats.constituentLabels.toArray

//      println("LABEL SET: " + labels.mkString(" "))
//      println("TREE: " + tree)

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
*/



/*
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
*/
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