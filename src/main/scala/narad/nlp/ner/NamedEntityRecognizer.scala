package narad.nlp.ner

import narad.nlp.tagger.TagDictionary
import narad.bp.optimize.{OptimizerOptions, Optimizer}
import narad.nlp.parser.constituent.{TreebankStatistics, ConstituentLabelParser, ConstituentBracketParser, ConstituentParserParams}
import narad.bp.util.index.{ArrayIndex, HashIndex, Index}
import narad.io.tree.{TreebankReader, TreebankReaderOptions}
import collection.immutable.HashSet

//L2Regularizer
import narad.bp.util.PotentialReader
import narad.util.ArgParser
import narad.bp.structure.ModelOptions
import narad.io.onto._

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 12/11/12
 * Time: 8:34 PM
 * To change this template use File | Settings | File Templates.
 */
object NamedEntityRecognizer {

  def main(args: Array[String]) {
    val params = new NamedEntityParams(args)
    val ner = new NamedEntityModel(params)
    if (params.EXTRACT_FEATURES) {
      System.err.println("Extracting features...")
      val index = if (params.HASH_DICT) new HashIndex(params.PV_SIZE) else new ArrayIndex[String]()
      val reader = new OntoReader(params.TRAIN_NER_FILE, params.TRAIN_SYNTAX_FILE)
      val stats = TreebankStatistics.construct(reader.map(_.tree).iterator, params.PRUNE)
      val labels = reader.foldLeft(new HashSet[String])(_ ++ _.ner.labels.toSet)
      ner.extractFeatures(params.TRAIN_NER_FILE, params.TRAIN_SYNTAX_FILE, params.TRAIN_FEATURE_FILE, index, labels.toArray, stats, params)
      ner.extractFeatures(params.TEST_NER_FILE, params.TEST_SYNTAX_FILE, params.TEST_FEATURE_FILE, index, labels.toArray, stats, params)
      if (!params.HASH_DICT) index.writeToFile("feats.index")
    }
    if (params.TRAIN) {
      val optimizer = new Optimizer(ner, params)
      val data = new PotentialReader(params.TRAIN_FIDX_FILE)
      optimizer.train(data)
    }
    if (params.TEST) {
      val optimizer = new Optimizer(ner, params)
      val data = new PotentialReader(params.TEST_FIDX_FILE)
      optimizer.test(data)
    }
  }
}

/*
    val params = new ConstituentParserParams(args)
    //   println(params.MODEL)
    val parser = params.MODEL match {
      case "BRACK" => new ConstituentBracketParser(params)
      case "LABEL" => new ConstituentLabelParser(params)
    }
    if (params.EXTRACT_FEATURES) {
      System.err.println("Extracting features...")
      val index = if (params.HASH_DICT) new HashIndex(params.PV_SIZE) else new ArrayIndex[String]()
      val treeOptions = TreebankReaderOptions.fromCommandLine(new ArgParser(args))
      val reader = new TreebankReader(params.TRAIN_FILE, treeOptions)
      val stats = TreebankStatistics.construct(reader.iterator, params.PRUNE)
      stats.writeToFile("train.stats")
      parser.extractFeatures(params.TRAIN_FILE, params.TRAIN_FEATURE_FILE, stats, index, params)
      parser.extractFeatures(params.TEST_FILE, params.TEST_FEATURE_FILE, stats, index, params)
      if (!params.HASH_DICT) index.writeToFile("feats.index")
    }
    else if (params.TRAIN) {
      val optimizer = new Optimizer(parser, params) //with L2Regularizer
      val data = new PotentialReader(params.TRAIN_FIDX_FILE)
      optimizer.train(data)
    }
    else if (params.TEST) {
      val optimizer = new Optimizer(parser, params)
      val data = new PotentialReader(params.TEST_FIDX_FILE)
      optimizer.test(data)
    }
  }
}

*/
/*
  def main(args: Array[String]) = {
    val params = new NamedEntityParams(args)
    val ner = new NamedEntityModel(params)

    if (params.getBoolean("--extract.features")) {
      //val dict   = TagDictionary.construct(params.TRAIN_FILE, mode=params.MODE)
      //dict.toFile("tags.dict")
      //			println(dict.all.mkString("\n"))
      val reader = new OntoReader(params.TRAIN_NER_FILE, params.TRAIN_SYNTAX_FILE, params)
      val labels = reader.map(_.ner.entities.map(_.label)).flatten.toArray.distinct
      System.err.println("Using %d labels:\n%s".format(labels.size, labels.mkString("\n")))
      ner.extractFeatures(params.TRAIN_NER_FILE, params.TRAIN_SYNTAX_FILE, params.TRAIN_FEATURE_FILE, labels, 10, params)
      ner.extractFeatures(params.TEST_NER_FILE, params.TEST_SYNTAX_FILE, params.TEST_FEATURE_FILE, labels, 10, params)
    }
    else if (params.getBoolean("--train")) {
 //     val optimizer = new Optimizer(ner, params)// with L2Regularizer
 //     val data = new PotentialReader(params.TRAIN_FIDX_FILE)
 //     optimizer.train(data)
    }
    else if (params.getBoolean("--test")) {
 //     val optimizer = new Optimizer(ner, params)
 //     val data = new PotentialReader(params.TEST_FIDX_FILE)
 //     optimizer.test(data)
    }
  }
}
*/



