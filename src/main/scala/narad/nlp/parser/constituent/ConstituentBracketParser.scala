package narad.nlp.parser.constituent

import java.io.FileWriter
import narad.io.tree.{OntoNotesTreebankReaderOptions, TreebankReader}
import narad.nlp.trees.ConstituentTree
import collection.mutable.{HashMap, HashSet, ArrayBuffer, Map}
import math.min
import narad.nlp.ling.{TaggedToken => Token}
import narad.nlp.trees.ConstituentTree
import narad.bp.util.{GZipWriter, Feature, StringFeature, PotentialExample}
import narad.bp.structure.Potential
import narad.bp.util.index._

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 12/16/12
 * Time: 5:29 AM
 * To change this template use File | Settings | File Templates.
 */
class ConstituentBracketParser(params: ConstituentParserParams) extends ConstituentParserModel(params) with ConstituentBracketFeatures

trait ConstituentBracketFeatures extends ConstituentParserFeatures {

  def extractFeatures(trainFile: String, trainFeatureFile: String, stats: TreebankStatistics, index: Index[String], params: ConstituentParserParams) = {
    println("Extracting brack features (in batch sizes of %d)".format(params.BATCH_SIZE))
    println("  window of " + params.FEATURE_WINDOW)
    val out = new GZipWriter(trainFeatureFile + ".gz") //new FileWriter(trainFeatureFile)
    val reader = new TreebankReader(trainFile, new OntoNotesTreebankReaderOptions)
    var startTime = System.currentTimeMillis()
    reader.zipWithIndex.grouped(params.BATCH_SIZE).foreach { batch =>
      val batchArray = batch.toArray
      val pexs = new Array[PotentialExample](batchArray.size)
      batchArray.par.map { case(tree, i) =>
        val btree = if (params.BINARIZE_MODE == "MARG") {
          tree.removeNones().removeUnaryChains()
        }
        else {
          tree.removeNones().removeUnaryChains().binarize(params.BINARIZE_MODE)
        }
        if (i % params.PRINT_INTERVAL == 0) System.err.print("\r  example %d...[index contains %d elements].".format(i, index.size))
        pexs(i % params.BATCH_SIZE) = getBracketFeatures(btree, index, params)
      }
      pexs.foreach { pex =>
        pex.writeToFile(out)
        out.write("\n")
      }
    }
    out.close()
    if (params.TIME) System.err.println("Finished Feature Extraction [%fs.]".format((System.currentTimeMillis() - startTime) / 1000.0))
  }

  def getBracketFeatures(tree: ConstituentTree, index: Index[String], params: ConstituentParserParams): PotentialExample = {
    val ex = new PotentialExample
    val BRACK_NAME = params.BRACK_NAME
    val slen = tree.length
    ex.attributes("slen") = tree.length.toString
    ex.attributes("words") = tree.words.mkString(" ")
    ex.attributes("tags") = tree.tags.mkString(" ")
    ex.attributes("tree") = tree.toString()
    for ( width <- 2 to slen; start <- 0 to (slen - width)) {
      val end = start + width
      val feats = constituentSpanFeatures(tree.tokens.toArray, start, end, params)
      val isCorrect = tree.containsSpan(start, end)
      val potname = "%s(%d,%d)".format(BRACK_NAME, start, end)
      ex.potentials += new Potential(1.0, potname, isCorrect)
      ex.features(potname) = feats.map{ f =>
        if (params.INTEGERIZE) {
          new Feature(index.index(f), 1.0, 0)
        }
      else {
          new StringFeature(f, index.index(f), 1.0, 0)
        }
      }
    }
    ex
  }
}

























/*
def extractFeatures(trainFile: String, trainFeatureFile: String, stats: TreebankStatistics, index: Index[String], params: ConstituentParserParams) = {
  println("Extracting bracket features")
  println("  windo of " + params.FEATURE_WINDOW)
  val out = new FileWriter(trainFeatureFile)
  val reader = new TreebankReader(trainFile)
  var startTime = System.currentTimeMillis()
  reader.zipWithIndex.grouped(params.BATCH_SIZE).foreach { batch =>
    val batchArray = batch.toArray
    val pexs = new Array[PotentialExample](batchArray.size)
    batchArray.par.map { case(tree, i) =>
      if (i % params.PRINT_INTERVAL == 0) System.err.print("\r  example %d...[index contains %d elements].".format(i, index.size))
      val btree = tree.binarize().removeUnaryChains
      pexs(i % params.BATCH_SIZE) = getBracketFeatures(btree, index, params)
    }
    pexs.foreach { pex =>
      pex.writeToFile(out)
      out.write("\n")
    }
  }
  out.close()
  if (params.TIME) System.err.println("Finished Feature Extraction [%fs.]".format((System.currentTimeMillis() - startTime) / 1000.0))
}
*/