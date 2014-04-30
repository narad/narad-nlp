package narad.nlp.tagger.dependency

import narad.nlp.tagger._
import narad.bp.util.index.Index
import java.io.FileWriter
import narad.io.conll.{CoNLLDatum, CoNLLReader}
import narad.bp.util.{GZipWriter, Feature, PotentialExample, StringFeature}
import collection.mutable.{HashMap, ArrayBuffer, Map}
import narad.bp.structure.{FactorGraph, FactorGraphBuilder, ModelInstance, Potential}
import narad.nlp.parser.dependency.DependencyParseFeatures
import narad.nlp.tagger.factorial._
import util.matching.Regex

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 1/18/14
 * Time: 8:20 AM
 */


class DependencyTaggerModel(val params: MorphTaggerParams, val dict: MultiTagDictionary) extends TaggerModel(params) with TaggerFeatures
                            with DependencyTaggerDecoding with MorphFeatures with DependencyParseFeatures {
  val UNIGRAM_FAC_PATTERN  = """label\(([0-9]+),([0-9]+),([0-9]+)\)""".r
  val BIGRAM_FAC_PATTERN   = """bigram\(([0-9]+),([0-9]+),([0-9]+),([0-9]+)\)""".r
//  val BIGRAM_FAC_PATTERN2   = """rbigram\(([0-9]+),([0-9]+),([0-9]+),([0-9]+)\)""".r
  val SYNTAX_PATTERN       = """link\(([0-9]+),([0-9]+)\)""".r
  val CONNECT_PATTERN      = """connect\(([0-9]+),([0-9]+),([0-9]+),([0-9]+)\)""".r

  def extractFeatures2(trainFile: String, trainFeatureFile: String, index: Index[String], params: MorphTaggerParams) = {
    val in = trainFile
    val out = new GZipWriter(trainFeatureFile + ".gz")
    val reader = new CoNLLReader(in)
    var startTime = System.currentTimeMillis()
    val batchSize = params.FEATURE_BATCH_SIZE
    System.err.println("Feat batch size = " + batchSize)
    reader.zipWithIndex.grouped(batchSize).foreach { batch =>
      val batchArray = batch.toArray
      val pexs = new Array[PotentialExample](batchArray.size)
      batchArray.par.map { case(datum, i) =>
        if (i % params.PRINT_INTERVAL == 0) System.err.print("\r  example %d...[index contains %d elements].".format(i, index.size))
        pexs(i % batchSize) = getExample(datum, index)
      }
      pexs.foreach { pex =>
        pex.writeToFile(out)
        out.write("\n")
      }
    }
    out.close()
    System.err.println("\rFeature Extraction Finished in %fs.".format((System.currentTimeMillis() - startTime) / 1000.0))
  }

  def getExample(datum: CoNLLDatum, index: Index[String]): PotentialExample = {
    val ex = new PotentialExample
    val chains = params.ATTRIBUTES
    addAttributes(ex, datum, chains)
    addPotentials(ex, datum, chains, index)
    ex
  }


  def addAttributes(ex: PotentialExample, datum: CoNLLDatum, mattr: Array[String]) {
    ex.attributes("len") = datum.slen.toString
    ex.attributes("slen") = datum.slen.toString
    ex.attributes("chains") = mattr.size.toString
    ex.attributes("attrs") = mattr.mkString(" ")
    ex.attributes("words") = datum.words.mkString(" ")
    ex.attributes("model") = params.MODEL
    ex.attributes("heads") = datum.heads.mkString(" ")
    for (attr <- mattr) {
      ex.attributes(attr) = dict.tagsOfAttribute(attr).mkString(" ")
    }
  }

  def addPotentials(ex: PotentialExample, datum: CoNLLDatum, mattr: Array[String], index: Index[String]) {
    for (chain <- 0 until mattr.size) {
      val attr = mattr(chain)
      for (slice <- 1 to datum.slen) {
        val feats = unigramLexicalFeatures(datum.words.toArray, slice, params)
        val tags = if (params.SPARSE) {
          dict.tagsOfAttributeOrAll(datum.word(slice), attr)
        }
        else {
          dict.tagsOfAttribute(attr)
        }
        for (tag <- 0 until tags.size) {
          val potname = "%s(%d,%d,%d)".format(params.LABEL_NAME, chain, slice, tag)
          ex.potentials += new Potential(1.0, potname, tags(tag) == correct(slice, datum, attr))
          ex.features(potname) = feats.map { f =>
            val fname = "%s_%s_%s".format(attr, tags(tag), f)
            if (params.INTEGERIZE) {
              new Feature(index.index(fname))
            }
            else {
              new StringFeature(fname, index.index(fname))
            }
          }
        }
      }
    }
    val slen = datum.slen
    val chain = 0
    for (i <- 1 until slen; j <- 1 to slen if i != j) {
      val feats = groupedBigramLexicalFeatures(datum.words.toArray, i, j, params)
      val attr = mattr(0)
      val itags = if (params.SPARSE) dict.tagsOfAttributeOrAll(datum.word(i), attr) else dict.tagsOfAttribute(attr)
      val jtags = if (params.SPARSE) dict.tagsOfAttributeOrAll(datum.word(j), attr) else dict.tagsOfAttribute(attr)
      for (ti <- 0 until itags.size; tj <- 0 until jtags.size) {
        val potname = "bigram(%d,%d,%d,%d)".format(i, j, ti, tj)
//          val potname = if (i < j) {
//          "bigram(%d,%d,%d,%d)".format(i, j, ti, tj)
//        }
//        else {
//          "rbigram(%d,%d,%d,%d)".format(i, j, ti, tj)
//        }
        ex.potentials += new Potential(1.0, potname, itags(ti) == correct(i, datum, attr) && jtags(tj) == correct(j, datum, attr))
        ex.features(potname) = feats.map { f =>
          val fname = "%s_%s_%s_%s".format(attr, itags(ti), jtags(tj), f)
          if (params.INTEGERIZE) {
            new Feature(index.index(fname))
          }
          else {
            new StringFeature(fname, index.index(fname))
          }
        }
      }
    }
    for (i <- 0 to slen; j <- 1 to slen if i != j) {
      val feats = wordLevelDependencyFeatures(datum.words.toArray, i, j)
      val potname = "link(%d,%d)".format(i,j)
      ex.potentials += new Potential(1.0, potname, datum.head(j) == i)
      ex.features(potname) = feats.map { f =>
        if (params.INTEGERIZE) {
          new Feature(index.index(f))
        }
        else {
          new StringFeature(f, index.index(f))
        }
      }
    }
    for (i <- 1 until slen; j <- 1 to slen if i != j) {
      val feats = connectFeats(datum.words.toArray, i, j)
      val attr = mattr(0)
      val chain = 0
      val itags = if (params.SPARSE) dict.tagsOfAttributeOrAll(datum.word(i), attr) else dict.tagsOfAttribute(attr)
      val jtags = if (params.SPARSE) dict.tagsOfAttributeOrAll(datum.word(j), attr) else dict.tagsOfAttribute(attr)
      for (ti <- 0 until itags.size; tj <- 0 until jtags.size) {
        val potname = "connect(%d,%d,%d,%d)".format(i, j, ti, tj)
        ex.potentials += new Potential(1.0, potname, datum.head(j) == i && itags(ti) == correct(i, datum, attr) && jtags(tj) == correct(j, datum, attr))
        ex.features(potname) = feats.map { fstr =>
          val f = "%s-%s-%s".format(fstr, itags(ti), jtags(tj))
          if (params.INTEGERIZE) {
            new Feature(index.index(f))
          }
          else {
            new StringFeature(f, index.index(f))
          }
        }
      }
    }
  }

  def correct(i: Int, datum: CoNLLDatum, attribute: String): String = {
    datum.attribute(i, attribute)
  }

  override def constructFromExample(ex: PotentialExample, pv: Array[Double]): ModelInstance = {
    val pots = ex.exponentiated(pv)
    val fg   = new FactorGraphBuilder(pots)
    val slen    = ex.attributes.getOrElse("len", "-1").toInt
    val heads    = Array(-1) ++ ex.attributes.getOrElse("heads", "-1").split(" ").map(_.toInt)

    // Add Unigram/Label Variables and Factors
    val idxs = Array.ofDim[ArrayBuffer[Int]](slen+1)
    for (i <- 0 to slen) idxs(i) = new ArrayBuffer[Int]()
    for (pot <- pots) {
      pot.name match {
        case UNIGRAM_FAC_PATTERN(chain, slice, value) => {
          idxs(slice.toInt) += fg.addUnaryVariable("labelvar(%s,%s)".format(slice, value), "labelfac(%s,%s)".format(slice, value), pot)
        }
        case _ => {}
      }
    }
//    for (i <- 1 to slen if idxs(i).size > 1) {
//      fg.addExactly1FactorByIndices(idxs(i).toArray, "Exactly1(%d)".format(i))
//    }
    // Add Bigram Variables and Factors
    for (pot <- pots) {
      pot.name match {
        case BIGRAM_FAC_PATTERN(slice1, slice2, value1, value2) => {
            fg.addVariable("bigramvar(%s,%s,%s,%s)".format(slice1, slice2, value1, value2), 2)
            fg.addNand3FactorByName("labelvar(%s,%s)".format(slice1, value1),
                                    "bigramvar(%s,%s,%s,%s)".format(slice1, slice2, value1, value2),
                                    "labelvar(%s,%s)".format(slice2, value2),
                                    "bigramfac(%s,%s,%s,%s)".format(slice1, slice2, value1, value2), pot)
       }
        case _ => {}
      }
    }
    // Add Syntax Variables and Factors

    val observedSyntax = false
    for (pot <- pots) {
      pot.name match {
        case SYNTAX_PATTERN(hidx, kidx) => {
          fg.addUnaryVariable("linkVar(%s,%s)".format(hidx, kidx), "linkFac(%s,%s)".format(hidx, kidx), pot)
        }
        case _=> {}
      }
    }
    if (!observedSyntax) {
      fg.addProjectiveTreeFactor(new Regex("linkVar\\("), "PTREE", slen)
    }
    // Connect Syntax variables to Bigram Factors
    for (pot <- pots) {
      pot.name match {
        case CONNECT_PATTERN(hidx, kidx, value1, value2) => {
          fg.addNandFactorByName("bigramvar(%s,%s,%s,%s)".format(hidx, kidx, value1, value2),
                                 "linkVar(%s,%s)".format(hidx, kidx),
                                 "connect(%s,%s,%s,%s)".format(hidx, kidx, value1, value2), pot)
        }
        case _ => {}
      }
    }

 //   println(fg.toFactorGraph)
    return new DependencyTaggerModelInstance(fg.toFactorGraph, ex)
  }

  def connectFeats(words: Array[String], head: Int, dep: Int): Array[String] = {
    val dir  = if (dep > head) "R" else "L"
    val dist = scala.math.abs(head - dep)
    Array[String](
      "[connect-bias]",
      "[connect-dist]-%d".format(dist),
      "[connect-dir]-%s".format(dir),
      "[connect-dist-dir]-%d-%s".format(dist, dir),
      "[connect-word-word]-%s-%s".format(words(head-1), words(dep-1)),
      "[connect-word-word-dist]-%s-%s-%d".format(words(head-1), words(dep-1), dist),
      "[connect-word-word-dir]-%s-%s-%s".format(words(head-1), words(dep-1), dir)
    )
  }
}

class DependencyTaggerModelInstance(fg: FactorGraph, ex: PotentialExample) extends ModelInstance(fg, ex) {

  override def clampedFactors = {
    fg.factors.filter(f => f.name.startsWith("label") || f.name.startsWith("bigram")).iterator
//    fg.factors.iterator
  }
}
































  /*
// Add Bigram Variables and Factors
for (pot <- pots) {
pot.name match {
case BIGRAM_FAC_PATTERN(slice1, slice2, value1, value2) => {
fg.addVariable("bigramvar(%s,%s,%s,%s)".format(slice1, slice2, value1, value2), 2)
//          if (slice1 < slice2) {
fg.addNand3FactorByName("labelvar(%s,%s)".format(slice1, value1),
"bigramvar(%s,%s,%s,%s)".format(slice1, slice2, value1, value2),
"labelvar(%s,%s)".format(slice2, value2),
"bigramfac(%s,%s,%s,%s)".format(slice1, slice2, value1, value2), pot)
}
/*          else {
            fg.addNand3FactorByName("labelvar(%s,%s)".format(slice2, value1),
                                    "bigramvar(%s,%s,%s,%s)".format(slice2, slice1, value1, value2),
                                    "labelvar(%s,%s)".format(slice1, value2),
                                    "bigramfac(%s,%s,%s,%s)".format(slice1, slice2, value1, value2), pot)
          }
*/           //         println(pot.name)
//   val (firstIdx, lastIdx) = if (slice1.toInt > slice2.toInt) (slice2, slice1) else (slice1, slice2)
//            val firstIdx = slice1
//            val lastIdx = slice2
//            fg.addVariable("bigramvar(%s,%s,%s,%s)".format(slice1, slice2, value1, value2), 2)
//            fg.addNand3FactorByName("labelvar(%s,%s)".format(firstIdx, value1),
//              "bigramvar(%s,%s,%s,%s)".format(slice1, slice2, value1, value2),
//                                    "labelvar(%s,%s)".format(lastIdx, value2),
//                                    "bigramfac(%s,%s,%s,%s)".format(slice1, slice2, value1, value2), pot)
}
case _ => {}
}
}
// Add Syntax Variables and Factors


         */






















/*

  def addChainPotentials(ex: PotentialExample, datum: CoNLLDatum, mattr: Array[String], index: Index[String]) {
    val slen = datum.slen
    for (chain <- 0 until mattr.size) {
      for (head <- 1 to slen; dep <- 1 to slen if head != dep) {
        val feats = groupedBigramLexicalFeatures(datum.words.toArray, head, dep, params)
        val attr = mattr(chain)
        val itags = if (params.SPARSE) dict.tagsOfAttributeOrAll(datum.word(head), attr) else dict.tagsOfAttribute(attr)
        val jtags = if (params.SPARSE) dict.tagsOfAttributeOrAll(datum.word(dep), attr) else dict.tagsOfAttribute(attr)
        for (ti <- 0 until itags.size; tj <- 0 until jtags.size) {
          val potname = "chain(%d,%d,%d,%d,%d,%d)".format(chain, chain, head, dep, ti, tj)
          ex.potentials += new Potential(1.0, potname, itags(ti) == correct(head, datum, attr) && jtags(tj) == correct(dep, datum, attr))
          ex.features(potname) = feats.map { f =>
            val fname = "%s_%s_%s_%s".format(attr, itags(ti), jtags(tj), f)
            if (params.INTEGERIZE) {
              new Feature(index.index(fname))
            }
            else {
              new StringFeature(fname, index.index(fname))
            }
          }
        }
      }
    }
  }





  def addSyntaxPotentials(ex: PotentialExample, datum: CoNLLDatum, mattr: Array[String], index: Index[String]) {
    for (chain <- 0 until mattr.size) {
      for (head <- 1 until datum.slen; dep <- 1 to datum.slen if head != dep) {
          if (datum.head(dep) == head || datum.head(head) == dep) {
            addBigramPotentials(slice, head, chain, ex, datum, mattr, index)
          }
        }
      }
    }
  }


  def addBigramPotentials(i: Int, j: Int, chain: Int, ex: PotentialExample, datum: CoNLLDatum, mattr: Array[String], index: Index[String]) {
    val feats = groupedBigramLexicalFeatures(datum.words.toArray, i, j, params)
    val attr = mattr(chain)
    val itags = if (params.SPARSE) dict.tagsOfAttributeOrAll(datum.word(i), attr) else dict.tagsOfAttribute(attr)
    val jtags = if (params.SPARSE) dict.tagsOfAttributeOrAll(datum.word(j), attr) else dict.tagsOfAttribute(attr)
    for (ti <- 0 until itags.size; tj <- 0 until jtags.size) {
      val potname = "chain(%d,%d,%d,%d,%d,%d)".format(chain, chain, i, j, ti, tj)
      ex.potentials += new Potential(1.0, potname, itags(ti) == correct(i, datum, attr) && jtags(tj) == correct(j, datum, attr))
      ex.features(potname) = feats.map { f =>
        val fname = "%s_%s_%s_%s".format(attr, itags(ti), jtags(tj), f)
        if (params.INTEGERIZE) {
          new Feature(index.index(fname))
        }
        else {
          new StringFeature(fname, index.index(fname))
        }
      }
    }
  }
*/




//  override val BIGRAM_PATTERN   = """bigram\(([0-9]+),([0-9]+),([0-9]+),([0-9]+),([0-9]+)\)""".r
//  override val TRIGRAM_PATTERN  = """trigram\(([0-9]+),([0-9]+),([0-9]+),([0-9]+)\)""".r

/*
//    val attributes  = Map[String, String]()
//    val potentials = new ArrayBuffer[Potential]
//    val featureMap = new HashMap[String, Array[Feature]]

val chains = params.ATTRIBUTES
val SLICE_DEP_PATTERN = """\(([0-9]+),([0-9]+)\)""".r
val sliceDeps = new ArrayBuffer[(Int, Int)]()
if (params.SLICE_DEPENDENCIES == "ALL") {
for (i <- 0 until chains.size; j <- i+1 until chains.size) {
sliceDeps += ((i, j))
}
}
else if (!params.SLICE_DEPENDENCIES.isEmpty) {
SLICE_DEP_PATTERN.findAllIn(params.SLICE_DEPENDENCIES).matchData.foreach { m =>
sliceDeps += ((m.group(1).toInt, m.group(2).toInt))
}
}

addAttributes(ex, datum, chains)
addLabelPotentials(potentials, datum, chains, index, featureMap)
if (params.CHAIN_ORDER == 2) {
addChainPotentials(potentials, datum, chains, index, featureMap)
}
if (sliceDeps.size > 0) {
addSlicePotentials(potentials, datum, chains, sliceDeps.toArray, index, featureMap)
}
*/
//    ex //new PotentialExample(attributes, potentials, featureMap)
//  }

/*



    if (params.ORDER == 2 && params.OBSERVE_SYNTAX) {
         println("EEK")
    }
    else if (params.ORDER == 2) {
      for (i <- 1 until slen) {
        val j = i+1
        val feats = groupedBigramLexicalFeatures(datum.words.toArray, i, j, params)
        for (a <- 0 until mattr.size) {
          val attr = mattr(a)
          val itags = if (SPARSE) dict.tagsOfAttributeOrAll(datum.word(i), attr) else dict.tagsOfAttribute(attr)
          val jtags = if (SPARSE) dict.tagsOfAttributeOrAll(datum.word(j), attr) else dict.tagsOfAttribute(attr)
          //          for (ti <- 0 until itags.size; tj <- 0 until jtags.size) {
          for (tj <- 0 until jtags.size; ti <- 0 until itags.size) {
            val potname = "bigram(%d,%d,%d,%d,%d)".format(i, j, a, ti, tj)
            potentials += new Potential(1.0, potname, itags(ti) == correct(i, datum, attr) && jtags(tj) == correct(j, datum, attr))
            if (INTEGERIZE) {
              featureMap(potname) = feats.map{ f =>
                new Feature(index.index("%s_%s_%s_%s".format(attr, itags(ti), jtags(tj), f.name)), f.value, f.group)
              }
            }
            else {
              featureMap(potname) = feats.map{ f =>
                new StringFeature("%s_%s_%s_%s".format(attr, itags(ti), jtags(tj), f.name), f.value, f.group)
              }
            }
          }
        }
      }
    }
    else if (params.OBSERVE_SYNTAX) {
      val heads = -1 +: datum.heads.toArray
      for (i <- 1 to slen; j <- i+1 to slen) {
        if (heads(j) == i || heads(i) == j) {
          val head = if (heads(j) == i) i else j
          val dep  = if (heads(j) == i) j else i
          val feats = groupedBigramLexicalFeatures(datum.words.toArray, i, j, params)
          for (a <- 0 until mattr.size) {
            val attr = mattr(a)
            val itags = if (SPARSE) dict.tagsOfAttributeOrAll(datum.word(i), attr) else dict.tagsOfAttribute(attr)
            val jtags = if (SPARSE) dict.tagsOfAttributeOrAll(datum.word(j), attr) else dict.tagsOfAttribute(attr)
            for (tj <- 0 until jtags.size; ti <- 0 until itags.size) {
              val potname = "bigram(%d,%d,%d,%d,%d)".format(i, j, a, ti, tj)
              potentials += new Potential(1.0, potname, itags(ti) == correct(i, datum, attr) && jtags(tj) == correct(j, datum, attr))
              if (INTEGERIZE) {
                featureMap(potname) = feats.map{ f =>
                  new Feature(index.index("%s_%s_%s_%s".format(attr, itags(ti), jtags(tj), f.name)), f.value, f.group)
                }
              }
              else {
                featureMap(potname) = feats.map{ f =>
                  new StringFeature("%s_%s_%s_%s".format(attr, itags(ti), jtags(tj), f.name), f.value, f.group)
                }
              }
            }
          }
        }
      }
    }
    else if (params.HIDDEN_SYNTAX) {
      for (i <- 0 to slen; j <- 1 to slen if i != j) {
        val feats = groupedWordLevelDependencyFeatures(datum.words.toArray, i, j)
        val builder = new StringBuilder()
        for (f <- feats) builder.append(" " + f)
        val potname = "un(%d,%d)".format(i, j)
        potentials += new Potential(1.0, potname,  datum.head(j) == i)
        featureMap(potname) = feats.map(f => new Feature(index.index(f.name), f.value, f.group))
      }
      for (i <- 1 to slen; j <- 1 to slen; k <- Array(0,1)) {
        val feats = groupedBigramLexicalFeatures(datum.words.toArray, i, j, params)
        for (a <- 0 until mattr.size) {
          val attr = mattr(a)
          val itags = if (SPARSE) dict.tagsOfAttribute(datum.word(i), attr) else dict.tagsOfAttribute(attr)
          val jtags = if (SPARSE) dict.tagsOfAttribute(datum.word(j), attr) else dict.tagsOfAttribute(attr)
          for (tj <- 0 until jtags.size; ti <- 0 until itags.size) {
            val potname = "trigram(%d,%d,%d,%d,%d)".format(i, j, ti, tj, k)
            potentials += new Potential(1.0, potname, itags(ti) == correct(i, datum, attr)
              && jtags(tj) == correct(j, datum, attr)
              && ((datum.head(j) == i && k == 1) || (datum.head(j) != i && k == 0)))
            if (INTEGERIZE) {
              featureMap(potname) = feats.map{ f =>
                new Feature(index.index("%s_%s_%s_%s_%s".format(attr, itags(ti), jtags(tj), k, f.name)), f.value, f.group)
              }
            }
            else {
              featureMap(potname) = feats.map{ f =>
                new StringFeature("%s_%s_%s_%s_%s".format(attr, itags(ti), jtags(tj), k, f.name), f.value, f.group)
              }
            }
          }
        }
      }
    }

    new PotentialExample(attributes, potentials, featureMap)
  }


  override def constructFromExample(ex: PotentialExample, pv: Array[Double]): ModelInstance = {
    val pots = ex.exponentiated(pv)
    val len    = ex.attributes.getOrElse("len", "-1").toInt
    val chains = ex.attributes.getOrElse("chains", "-1").toInt
    val slen = len

    val pgroups = pots.groupBy(p => p.name.substring(0, p.name.indexOf("(")))

    val arityIndex = Array.ofDim[Int](len+1, chains+1)
    val groups = pgroups("label").groupBy { pot =>
      val LABEL_FAC_PATTERN(time, layer, value) = pot.name
      (time.toInt, layer.toInt)
    }
    val fg = new FactorGraphBuilder(pots)
    for (t <- 1 to len; k <- 0 until chains) {
      if (groups.contains((t, k))) {
        fg.addTable1Variable("labelvar(%d,%d)".format(t, k),
          "labelfac(%d,%d)".format(t, k),
          groups((t, k)))
        arityIndex(t)(k) = groups((t,k)).size
      }
    }

    if (params.ORDER == 2 && pgroups.contains("bigram")) {
      val bigrams = pgroups("bigram").groupBy { pot =>
        val BIGRAM_PATTERN(time1, time2, layer, val1, val2) = pot.name
        (time1.toInt, time2.toInt, layer.toInt)
      }
      for (t1 <- 1 to len; t2 <- 1 to len; k <- 0 until chains) { //} if t2 == t1+1) {
        if (bigrams.contains((t1, t2, k))) {
          fg.addTable2Factor("labelvar(%d,%d)".format(t1, k),
            "labelvar(%d,%d)".format(t2, k),
            arityIndex(t1)(k), arityIndex(t2)(k),
            "bigramfac(%d,%d,%d)".format(t1, t2, k),
            bigrams((t1, t2, k)))
        }
      }
    }

    if (params.HIDDEN_SYNTAX && pgroups.contains("un") && pgroups.contains("trigram")) {
      val pothash = pgroups("un").groupBy { p => val LINK_PATTERN(start, end) = p.name; (start.toInt, end.toInt) }
      for (dep <- 1 to slen; head <- 0 to slen if dep != head) {
        fg.addUnaryVariable("linkvar(%d,%d)".format(head, dep), "linkfac(%d,%d)".format(head, dep), pothash((head, dep))(0))
      }
      fg.addProjectiveTreeFactor(new Regex("linkvar\\("), "PTREE", slen)

      val trigrams = pgroups("trigram").groupBy { pot =>
        val TRIGRAM_PATTERN(time1, time2, layer, val1, val2, val3) = pot.name
        (time1.toInt, time2.toInt, layer.toInt)
      }
      for (t1 <- 1 to len; t2 <- 1 to len; c <- 0 until chains) { //} if t2 == t1+1) {
        if (trigrams.contains((t1, t2, c))) {
          fg.addTable3Factor("labelvar(%d,%d)".format(t1, c),
                             "labelvar(%d,%d)".format(t2, c),
                             "un(%d,%d)".format(t1, t2),
                             arityIndex(t1)(c), arityIndex(t2)(c), 2,
                             "bigramfac(%d,%d,%d)".format(t1, t2, c),
                             trigrams((t1, t2, c)))
        }
      }
    }
    return new FactorialTaggerModelInstance(fg.toFactorGraph, ex)
  }
*/

































//    val fg = super.constructFromExample(ex, pv).graph.toBuilder
//    addDependencySyntax(fg, slen, pots)
//    addConnectionFactors(fg, slen, pots)

/*
import narad.io.conll.CoNLLDatum
import narad.bp.util.index.Index
import narad.bp.util.{Feature, PotentialExample}
import collection.mutable.{HashMap, ArrayBuffer, Map}
import narad.bp.structure.{FactorGraphBuilder, ModelInstance, Potential}
import util.matching.Regex
import collection.mutable

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 2/20/13
 * Time: 11:48 AM
 * To change this template use File | Settings | File Templates.
 */
class OracleSyntaxTagger(params: MorphTaggerParams, dict: MultiTagDictionary) extends MorphTaggerModel(params, dict) with TaggerFeatures {
  override val BIGRAM_PATTERN  = """bigram\(([0-9]+),([0-9]+),([0-9]+),([0-9]+),([0-9]+)\)""".r

  /*
  override def getExample(datum: CoNLLDatum, mattr: Array[String], index: Index[String]): PotentialExample = {
    val slen = datum.slen
    val potentials = new ArrayBuffer[Potential]
    val featureMap = new HashMap[String, Array[Feature]]
    val words = datum.words.toArray


    var heads = -1 +: datum.heads.toArray
    /*
    new Array[Int](words.size+1)
    if (params.OBSERVE_SYNTAX) {
    }
    if (params.OBSERVE_BIGRAM) {
      for (i <- 0 until heads.size) {
        heads(i) = i-1
      }
    }
    */


    /*
    val heads = new mutable.HashSet[(Int, Int)]()
    for (i <- 0 to slen; j <- i to slen) {
      if (params.OBSERVE_BIGRAM && i == j-1) heads += ((i,j))
      if (params.OBSERVE_SYNTAX && i == datum.head(j)) heads += ((i,j))
    }
    */

    val attributes  = Map[String, String]()
    attributes("slen") = datum.slen.toString
    attributes("len") = datum.slen.toString
    attributes("chains") = "1"
    attributes("attrs") = mattr.mkString(" ")
    attributes("words") = words.mkString(" ")
    attributes("heads") = heads.mkString(" ")
    attributes("tags") = dict.tagsOfAttribute(mattr(0)).mkString(" ")


    val SPARSE = params.SPARSE
    for (i <- 1 to slen) {
      val feats = unigramFeatures(datum, i, useMorph=false, useSyntax=false)
      for (a <- 0 until mattr.size) {
        val attr = mattr(a)
        val tags = if (SPARSE) dict.tagsOfAttributeOrAll(datum.word(i), attr) else dict.tagsOfAttribute(attr)
        for (k <- 0 until tags.size) {
          val potname = "label(%d,%d,%d)".format(i, a, k)
          //val potname = "ulabel(%d,%d)".format(i,k)
          potentials += new Potential(1.0, potname, tags(k) == correct(i, datum, attr))
          featureMap(potname) = feats.map(f => new Feature(index.index(tags(k) + "_" + f), 1.0, 0))
        }
      }
    }


    if (params.ORDER == 2) {
      if (params.OBSERVE_BIGRAM && params.OBSERVE_SYNTAX) {
        for (i <- 1 to slen; j <- i+1 to slen) {
          if (heads(j) == i || heads(i) == j) { // syntax
          val head = if (heads(j) == i) i else j
            val dep  = if (heads(j) == i) j else i
            val feats = if (i == j+1) { // overlapping bigram/syntax case
              (bigramFeatures(datum, head, dep) ++ bigramFeatures(datum, i, j)).distinct
            }
            else {
              bigramFeatures(datum, head, dep)
            }
          }
          else {  // default bigram

          }
        }
      }
      for (i <- 1 to slen; j <- i+1 to slen) {
        //    for (j <- 2 to slen; i <- j-1 to slen if i != j) {
        if (heads(j) == i || heads(i) == j) {
          val head = if (heads(j) == i) i else j
          val dep  = if (heads(j) == i) j else i
          val feats = bigramFeatures(datum, head, dep, false, false)
          for (a <- 0 until mattr.size) {
            val attr = mattr(a)
            val itags = if (SPARSE) dict.tagsOfAttributeOrAll(datum.word(i), attr) else dict.tagsOfAttribute(attr)
            val jtags = if (SPARSE) dict.tagsOfAttributeOrAll(datum.word(j), attr) else dict.tagsOfAttribute(attr)
            //          for (ti <- 0 until itags.size; tj <- 0 until jtags.size) {
            for (tj <- 0 until jtags.size; ti <- 0 until itags.size) {
              val potname = "bigram(%d,%d,%d,%d,%d)".format(i, j, a, ti, tj)
              potentials += new Potential(1.0, potname, itags(ti) == correct(i, datum, attr) && jtags(tj) == correct(j, datum, attr))
              featureMap(potname) = feats.map(f => new Feature(index.index(itags(ti) + "_" + jtags(tj) + "_" + f), 1.0, 0))
            }
          }
        }
      }
    }
    new PotentialExample(attributes, potentials.toArray, featureMap)
  }
  */

  //    for (head <- 1 to slen; dep <- 1 to slen if dep != head) {
  //      if ((datum.head(dep) == head && MODE == "SYNTAX")|| (head+1 == dep && MODE == "BIGRAM")) {



  override def constructFromExample(ex: PotentialExample, pv: Array[Double]): ModelInstance = {

    //    val fg = super.constructFromExample(ex, pv).graph.toBuilder
    //    addDependencySyntax(fg, slen, pots)
    //    addConnectionFactors(fg, slen, pots)

    val pots = ex.exponentiated(pv)
    for (i <- 0 until pots.size if pots(i).name.startsWith("un")) {

    }
    val len    = ex.attributes.getOrElse("len", "-1").toInt
    val chains = ex.attributes.getOrElse("chains", "-1").toInt
    val slen = len //ex.attributes.getOrElse("slen", "-1").toInt

    val arityIndex = new Array[Int](len+1)
    val groups = pots.filter(_.name.startsWith("label")).groupBy { pot =>
      val LABEL_FAC_PATTERN(time, layer, value) = pot.name
      (time.toInt, layer.toInt)
    }
    val fg = new FactorGraphBuilder(pots)
    for (t <- 1 to len; k <- 0 until chains) {
      //   println("construct t = " + t)
      if (groups.contains((t, k))) {
        fg.addTable1Variable("labelvar(%d,%d)".format(t, k),
          "labelfac(%d,%d)".format(t, k),
          groups((t, k)))
        arityIndex(t) = groups((t,k)).size
      }
    }

    val bigrams = pots.filter(_.name.startsWith("bigram")).groupBy { pot =>
      val BIGRAM_PATTERN(time1, time2, layer, val1, val2) = pot.name
      (time1.toInt, time2.toInt, layer.toInt)
    }
    for (t1 <- 1 to len; t2 <- 1 to len; k <- 0 until chains) { //} if t2 == t1+1) {
      if (bigrams.contains((t1, t2, k))) {
        fg.addTable2Factor("labelvar(%d,%d)".format(t1, k),
          "labelvar(%d,%d)".format(t2, k),
          arityIndex(t1), arityIndex(t2),
          "bigramfac(%d,%d,%d)".format(t1, t2, k),
          bigrams((t1, t2, k)))
      }
    }
    //  println(fg.toFactorGraph)
    return new FactorialTaggerModelInstance(fg.toFactorGraph, ex)
  }
}

*/

/*
class SyntaxMorphTaggerModel(params: MorphTaggerParams, dict: MultiTagDictionary) extends MorphTaggerModel(params, dict) with DependencyParseFeatures {
  val LINK_PATTERN  = """un\(([0-9]+),([0-9]+)\)""".r
  override val TRIGRAM_PATTERN  = """trigram\(([0-9]+),([0-9]+),([0-9]+),([0-9]+)\)""".r

  override def getExample(datum: CoNLLDatum, mattr: Array[String], index: Index[String]): PotentialExample = {
    val slen = datum.slen
    val attributes  = Map[String, String]()
    val potentials = new ArrayBuffer[Potential]
    val featureMap = new HashMap[String, Array[Feature]]
    attributes("len") = datum.slen.toString
    attributes("chains") = "1"
    attributes("attrs") = mattr.mkString(" ")
    attributes("words") = datum.words.mkString(" ")
    attributes("heads") = "-1 " + datum.heads.mkString(" ")
    val SPARSE = true

    for (i <- 1 to slen) {
      val feats = unigramFeatures(datum, i, useMorph=false, useSyntax=false)
      for (j <- 0 until mattr.size) {
        val attr = mattr(j)
        val tags = if (SPARSE) dict.tagsOfAttribute(datum.word(i), attr) else dict.tagsOfAttribute(attr)
        for (k <- 0 until tags.size) {
          val potname = "label(%d,%d,%d)".format(i, j, k)
          potentials += new Potential(1.0, potname, tags(k) == correct(i, datum, attr))
          featureMap(potname) = feats.map(f => new Feature(index.index(tags(k) + "_" + f), 1.0, 0))
        }
      }
    }

    val oracle = params.OBSERVE_SYNTAX
    for (i <- 0 to slen; j <- 1 to slen if i != j) {
      val feats = if (oracle) Array("NONE") else dependencyFeatures(datum, i, j)
      val builder = new StringBuilder()
      for (f <- feats) builder.append(" " + f)
      val potname = "un(%d,%d)".format(i, j)
      potentials += new Potential(1.0, potname,  datum.head(j) == i)
      featureMap(potname) = feats.map(f => new Feature(index.index(f), 1.0, 0))
    }

    for (i <- 1 to slen; j <- 1 to slen; k <- Array(0,1)) {
//    for (j <- 1 to slen; i <- 1 to slen) {
      val feats = bigramFeatures(datum, i, j, useMorph=false, useSyntax=false)
      for (a <- 0 until mattr.size) {
        val attr = mattr(a)
        val itags = if (SPARSE) dict.tagsOfAttribute(datum.word(i), attr) else dict.tagsOfAttribute(attr)
        val jtags = if (SPARSE) dict.tagsOfAttribute(datum.word(j), attr) else dict.tagsOfAttribute(attr)
        for (tj <- 0 until jtags.size; ti <- 0 until itags.size) {
          val potname = "trigram(%d,%d,%d,%d,%d)".format(i, j, k, ti, tj)
          potentials += new Potential(1.0, potname, itags(ti) == correct(i, datum, attr)
                                        && jtags(tj) == correct(j, datum, attr)
                                        && ((datum.head(j) == i && k == 1) || (datum.head(j) != i && k == 0)))
          featureMap(potname) = feats.map(f => new Feature(index.index(itags(ti) + "_" + jtags(tj) + "_" + k + "_" + f), 1.0, 0))
        }
      }
    }


    new PotentialExample(attributes, potentials.toArray, featureMap)
  }


  override def constructFromExample(ex: PotentialExample, pv: Array[Double]): ModelInstance = {

//    val fg = super.constructFromExample(ex, pv).graph.toBuilder
//    addDependencySyntax(fg, slen, pots)
//    addConnectionFactors(fg, slen, pots)

    val pots = ex.exponentiated(pv)
    val len    = ex.attributes.getOrElse("len", "-1").toInt
    val chains = ex.attributes.getOrElse("chains", "-1").toInt
    val slen = len //ex.attributes.getOrElse("slen", "-1").toInt

    val oracle = true
    if (oracle) {
      for (pot <- pots if pot.name.startsWith("un")) {
        pot.value = if (pot.isCorrect) Double.PositiveInfinity else Double.NegativeInfinity
      }
    }

    println(len)
    val arityIndex = new Array[Int](len+1)
    println(arityIndex.size)
    val groups = pots.filter(_.name.startsWith("label")).groupBy { pot =>
      val LABEL_FAC_PATTERN(time, layer, value) = pot.name
      (time.toInt, layer.toInt)
    }
    val fg = new FactorGraphBuilder(pots)
    for (t <- 1 to len; k <- 0 until chains) {
      if (groups.contains((t, k))) {
        fg.addTable1Variable("labelvar(%d,%d)".format(t, k),
          "labelfac(%d,%d)".format(t, k),
          groups((t, k)))
      }
      arityIndex(t) = groups((t,k)).size
    }

    /*
    val dephash = pots.filter(_.name.startsWith("un")).groupBy { p => val LINK_PATTERN(start, end) = p.name; (start.toInt, end.toInt) }
    for (dep <- 1 to slen; head <- 0 to slen if dep != head) {
      fg.addVariable("%s(%d,%d)".format("linkvar", head, dep), 2)
      fg.addUnaryFactor("linkvar(%d,%d)".format(head, dep), "link\\(%d,%d\\)".format(head, dep), dephash((head, dep))(0))
      // bpdp code kept a matrix for link vars, would have links[dep][head] = the variable
    }
    if (!oracle)  fg.addProjectiveTreeFactor(new Regex("linkvar\\("), "PTREE", slen)


    val connecthash = pots.filter(_.name.startsWith("trigram")).groupBy { p => val TRIGRAM_PATTERN(head, dep, ht, dt) = p.name; (head.toInt, dep.toInt) }
    println("hash keys: " + connecthash.keys.mkString(", "))
    for (dep <- 1 to slen; head <- 0 to slen if dep != head && head > 0) {
   //   fg.addVariable("%s(%d,%d)".format("linkvar", head, dep), 2)
      fg.addTable3Factor("labelvar(%d,0)".format(head),
        "labelvar(%d,0)".format(dep),
        "linkvar(%d,%d)".format(head, dep),
        arityIndex(head), arityIndex(dep), 2,
        "trigram(%d,%d)".format(head, dep),
        connecthash((head, dep)))
    }
    */
    return new FactorialTaggerModelInstance(fg.toFactorGraph, ex)
 }
}

    /*
  def addDependencySyntax(fg: FactorGraphBuilder, slen: Int, pots: Array[Potential]) = {
    val pothash = pots.filter(_.name.startsWith("un")).groupBy { p => val LINK_PATTERN(start, end) = p.name; (start.toInt, end.toInt) }
    for (dep <- 1 to slen; head <- 0 to slen if dep != head) {
      fg.addVariable("%s(%d,%d)".format("linkvar", head, dep), 2)
      fg.addUnaryFactor("linkvar(%d,%d)".format(head, dep), "link\\(%d,%d\\)".format(head, dep), pothash((head, dep))(0))
      // bpdp code kept a matrix for link vars, would have links[dep][head] = the variable
    }
   // fg.addProjectiveTreeFactor(new Regex("linkvar\\("), "PTREE", slen)
  }

  def addConnectionFactors(fg: FactorGraphBuilder, slen: Int, pots: Array[Potential]) = {

      // bpdp code kept a matrix for link vars, would have links[dep][head] = the variable
    }
  }
  */
   */




















/*
val word = datum.word(i)
val feats = bigramFeatures(datum, i, j, useMorph=false, useSyntax=false)
for (j <- 0 until mattr.size) {
val attr = mattr(j)
val tags = dict.tagsOfAttribute(attr)
for (k <- 0 until tags.size) {
  val potname = "blabel(%d,%d,%d,%d)".format(i, j, ti, tj)
  potentials += new Potential(1.0, potname, tags(k) == correct(i, datum, attr))
  featureMap(potname) = feats.map(f => new Feature(index.index(tags(ti) + "_" + tags(tj) + "_" + f), 1.0, 0))
}
}
}
*/
/*
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
*/
//}

