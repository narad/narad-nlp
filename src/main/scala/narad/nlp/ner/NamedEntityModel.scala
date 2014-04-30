package narad.nlp.ner

import narad.bp.structure._
import narad.bp.inference.BeliefPropagation
import narad.io.onto._
import narad.io.tree.{OntoNotesTreebankReaderOptions, TreebankReader}
import collection.mutable.{HashMap, ArrayBuffer}
import narad.bp.util.index.Index
import scala.collection.mutable.{Map => Map}
import narad.bp.util.{GZipWriter, Feature, PotentialExample}
import narad.nlp.trees.{ConstituentTreeFactory, ConstituentTree}
import narad.nlp.parser.constituent._
import narad.bp.util.{Feature, StringFeature}
import narad.nlp.trees._
import narad.nlp.trees.PreterminalNode
import narad.io.ner.NamedEntityDatum
import narad.bp.structure.Potential
import narad.io.onto.OntoDatum
import narad.bp.util.Feature


class NamedEntityModel(val params: NamedEntityParams) extends FactorGraphModel[OntoDatum] with BeliefPropagation
with NamedEntityFeatures with ConstituentLabelFeatures with ConstituentParserPrediction with ConstituentParserDecoding
with NamedEntityDecoding with NamedEntityPrediction {

  val INDICES_PATTERN      = """.*\(([0-9]+),([0-9]+)[^0-9].*""".r
  val glabelPattern        = """.*label\(([0-9]+),.+""".r
  val BIGRAM_PATTERN       = """blabel\(([0-9]+),([0-9]+),(.+)\)""".r
  val NER_SPAN_PATTERN     = """nerbracket\(([0-9]+),([0-9]+)\)""".r
  val NER_LABEL_PATTERN    = """nerlabel\(([0-9]+),([0-9]+),(.+)\)""".r
  val NER_INDICES_PATTERN  = """ner.+\(([0-9]+),([0-9]+).+""".r
  val LABEL_PATTERN1       = """spanLabel(.*)\(([0-9]+),([0-9]+)\).*""".r
  val UNARY_LABEL_PATTERN1 = """unaryLabel(.*)\(([0-9]+),([0-9]+)\)""".r

  def extractFeatures(reader: Iterable[OntoDatum], featureFile: String, index: Index[String],
                      labels: Array[String], stats: TreebankStatistics, ostats: OntoStatistics, params: NamedEntityParams) = {
    val maxSeg = params.MAX_SEG
    println("Extracting label features (in batch sizes of %d)".format(params.BATCH_SIZE))
    val out = new GZipWriter(featureFile + ".gz")
    val cnerLabels = ostats.entityConstituents.toArray.sortBy(_.toString)
    var startTime = System.currentTimeMillis()
 //   println("Marg? " + params.MARGINALIZATION)
    reader.zipWithIndex.grouped(params.BATCH_SIZE).foreach { batch =>
      val batchArray = batch.toArray
      val pexs = new Array[PotentialExample](batchArray.size)
      batchArray.par.map { case(onto, i) =>
        if (i % params.PRINT_INTERVAL == 0) System.err.print("\r  example %d...[index contains %d elements].".format(i, index.size))
        val ner = onto.ner
        val tree = onto.tree.removeTop.removeNones()
        val btree = if (params.BINARIZE) {
          tree.binarize(params.BINARIZE_MODE).removeNones().removeUnaryChains()
        }
        else {
          tree.removeNones().removeUnaryChains()
        }
        val ex = getNamedEntityFeatures(onto, index, labels, maxSeg, params)
        if (params.MODEL == "JOINT" || params.MODEL == "HIDDEN" || params.MODEL == "NPJOINT" || params.MODEL == "ORACLE") {
          ex.attributes("nclabels") = cnerLabels.mkString(" ")
          ex += getBracketFeatures(btree, index, params)
          if (params.PREDICT_LABELS)  ex += getLabelFeatures(btree, stats, index, params)
          if (params.PREDICT_UNARIES) ex += getUnaryFeatures(tree, stats, index, params)
          if (params.CONNECT_NP) {
            ex += getLabeledConnectionFeatures(onto, index, maxSeg, cnerLabels, params)
          }
          else {
            ex += getConnectionFeatures(onto, index, maxSeg, params)
          }
        }
        pexs(i % params.BATCH_SIZE) = ex
      }
      pexs.foreach { pex =>
        pex.writeToFile(out)
        out.write("\n")
      }
    }
    out.close()
    if (params.TIME) System.err.println("Finished Feature Extraction [%fs.]".format((System.currentTimeMillis() - startTime) / 1000.0))
  }

  override def fromPotentialExample(ex: PotentialExample, pv: Array[Double]): OntoDatum = {
    val words = ex.attributes.getOrElse("words", "").split(" ")
    val labels = ex.attributes.getOrElse("labels", "").split(" ")
    val segs = new ArrayBuffer[(Int, Int, Int)]()
    ex.getPotentials.foreach { b =>
//      println(b)
      b.name match {
        case NER_LABEL_PATTERN(start, end, label) => {
          if (b.isCorrect) segs += ((start.toInt, end.toInt, label.toInt))
        }
        case _=>
      }
    }
    val ners = segs.filter(_._3 > 1).map{ seg =>
      new NamedEntity(labels(seg._3-2), 0, seg._1, seg._2, tokens = words.slice(seg._1, seg._2))
    }
    val datum = new NamedEntityDatum(words, ners.toArray)
    // Parse construction
    if (params.MODEL == "JOINT" || params.MODEL == "ORACLE") {
      val slen   = ex.attributes.getOrElse("slen", "-1").toInt
      val pots = ex.exponentiated(pv)
      val fg = new FactorGraphBuilder(pots)
      if (params.MIN_PARSE_LEN > 2) {
        val hash = addBracketPrediction(fg, pots, slen)
        if (params.PREDICT_LABELS) {
          addLabelPrediction(fg, pots, slen, hash)
        }
      }
      if (params.PREDICT_UNARIES) {
        addUnaryPrediction(fg, pots, slen)
      }
      val instance = new ConstituentParserModelInstance(fg.toFactorGraph, ex)
      instance.marginals.foreach { b => if (b.isCorrect) b.value = 1.0 else b.value = 0.0 }
      val tree = decodeTree(instance, params)
      return new OntoDatum(datum, tree)
    }
    return new OntoDatum(datum, new ConstituentTree(new NonterminalNode("X"), words.map(w => new ConstituentTree(new PreterminalNode("X", w))).toList))
  }

  def getNamedEntityFeatures(datum: OntoDatum, index: Index[String], labels: Array[String], maxSeg: Int, params: NamedEntityParams): PotentialExample = {
    val slen = datum.slen
    val ner = datum.ner
    var tree = datum.tree
    tree = tree.removeUnaryChains().removeNones().binarize()
    val tokens = datum.tokens.toArray
    val maxSeg = params.MAX_SEG
    val fmode = params.FEATURE_MODE
    val ex = new PotentialExample
    ex.attributes("slen") = slen.toString
    ex.attributes("maxseg") = params.MAX_SEG.toString
    ex.attributes("labels") = labels.mkString(" ")
    ex.attributes("words") = tokens.map(_.word).mkString(" ")
    ex.attributes("tags") = tokens.map(_.pos).mkString(" ")
    ex.attributes("feats") = fmode

    for (j <- slen to 1 by -1) {
      for (i <- scala.math.max(0, j-maxSeg) to j-1) {
        val width = j-i
        val feats = nerFeatures(tokens, i, j, fmode)

        val correctSpan = ner.containsSpan(i,j) || (width == 1 && !ner.coversSpan(i,j)) // "+" else ""
        val potname1 = "%s(%d,%d)".format("nerbracket", i, j)
        ex.potentials += new Potential(1.0, potname1, correctSpan)
        ex.features(potname1) = feats.map{ f =>
          if (params.INTEGERIZE) {
            new Feature(index.index(f), 1.0, 0)
          }
          else {
            new StringFeature(f, index.index(f), 1.0, 0)
          }
        }

        val correctLabel = (!ner.containsSpan(i,j) && width > 1) || (width == 1 && ner.coversSpan(i,j))
        val potname2 = "%s(%d,%d,%s)".format("nerlabel", i, j, "0")
        ex.potentials += new Potential(1.0, potname2, correctLabel)
        ex.features(potname2) = if (params.INTEGERIZE) {
          Array[Feature](new Feature(index.index("None"), 1.0, 0))
        }
        else {
          Array[Feature](new StringFeature("None", index.index("None"), 1.0, 0))
        }

        val label = "O"
        val potname3 = "%s(%d,%d,%s)".format("nerlabel", i, j, "1")
        val isCorrect1 = width == 1 && !ner.containsSpan(i,j) && !ner.coversSpan(i,j)
        ex.potentials += new Potential(1.0, potname3, isCorrect1)
        if (width > 1) {
          ex.features(potname3) = Array[Feature]()
        }
        else {
          ex.features(potname3) = feats.map{ f =>
            if (params.INTEGERIZE) {
              new Feature(index.index(label + "_" + f), 1.0, 0)
            }
            else {
              new StringFeature(label + "_" + f, index.index(label + "_" + f), 1.0, 0)
            }
          }
        }

        for (label <- labels) {
          val correctLabel = ner.containsSpanLabel(i,j,label)
          val potname4 = "%s(%d,%d,%s)".format("nerlabel", i, j, labels.indexOf(label)+2)
          ex.potentials += new Potential(1.0, potname4, correctLabel)
          ex.features(potname4) = feats.map { f =>
            if (params.INTEGERIZE) {
              new Feature(index.index(label + "_" + f), 1.0, 0)
            }
            else {
              new StringFeature(label + "_" + f, index.index(label + "_" + f), 1.0, 0)
            }
          }
        }
      }
    }
    ex
  }

  def getConnectionFeatures(datum: OntoDatum, index: Index[String], maxSeg: Int, params: NamedEntityParams): PotentialExample = {
    val ex = new PotentialExample
    val slen = datum.slen
    val ner = datum.ner
    var tree = datum.tree
    tree = tree.removeUnaryChains().binarize()
    val tokens = tree.tokens.toArray
    for ( width <- 2 to Math.min(maxSeg, slen); start <- 0 to (slen - width)) {
      val end = start + width
      val feats = connectionFeatures(tokens, start, end)
      val isCorrect = tree.containsSpan(start, end) && ner.containsSpan(start, end)
      val potname = "%s(%d,%d)".format("agree", start, end)
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

  def getLabeledConnectionFeatures(datum: OntoDatum, index: Index[String], maxSeg: Int, cnerLabels: Array[String], params: NamedEntityParams): PotentialExample = {
    val ex = new PotentialExample
    val slen = datum.slen
    val ner = datum.ner
    var tree = datum.tree
    tree = tree.removeUnaryChains().binarize()
    val tokens = tree.tokens.toArray
    for ( width <- 2 to Math.min(maxSeg, slen); start <- 0 to (slen - width)) {
      val end = start + width
      val feats = connectionFeatures(tokens, start, end)
      val isCorrect = cnerLabels.exists(l => tree.containsLabel(start, end, l)) && ner.containsSpan(start, end)
      val potname = "%s(%d,%d)".format("agree", start, end)
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

  def constructFromExample(ex: PotentialExample, pv: Array[Double]): ModelInstance = { //(pots: Array[Potential], slen: Int, useBigrams: Boolean = false, useSyntax: Boolean = false): TaggerModel = {
  val slen   = ex.attributes.getOrElse("slen", "-1").toInt
    val maxseg = ex.attributes.getOrElse("maxseg", "10").toInt
    val labels = ex.attributes.getOrElse("labels", "").trim.split(" ")
    val nclabels = ex.attributes.getOrElse("nclabels", "").trim.split(" ")
    val arity = labels.size
    val pots = ex.exponentiated(pv)
    val fg = new FactorGraphBuilder(pots)
    addNamedEntityPrediction(fg, pots, slen, maxseg)
    if (params.MODEL == "JOINT" || params.MODEL == "NPJOINT" || params.MODEL == "ORACLE" || params.MODEL == "HIDDEN") {
      val parsePots = pots.filter { p =>
        p.name.startsWith("brack") || p.name.startsWith("spanLabel") ||
        p.name.startsWith("unary")}

      val bidxs = addBracketPrediction(fg, parsePots, slen)
      if (params.PREDICT_LABELS) {
        addLabelPrediction(fg, parsePots, slen, bidxs)
      }
      if (params.PREDICT_UNARIES) {
        addUnaryPrediction(fg, parsePots, slen)
      }
      addConnectionPrediction(fg, pots, slen, nclabels, maxseg)
    }

//    println("GRAPH:")
//    println(fg.toFactorGraph)

    if (params.MARGINALIZATION) {
      if (params.MODEL == "HIDDEN") {
        new NamedEntityMargModelInstance(fg.toFactorGraph, ex)
      }
      else {
        new NamedEntityLatentBinarizationInstance(fg.toFactorGraph, ex)
      }
    }
    else {
      val mi = new NamedEntityModelInstance(fg.toFactorGraph, ex)
      if (params.MODEL == "ORACLE") {
        mi.graph.factors.foreach { f =>
          if (f.name.startsWith("brack")) f.clamp()
        }
      }
      mi
    }
  }

  def options = params
}
















/*
  def clean(str: String) = {
    str.replace("$", "").replace("^", "").replace("[", "$[$").replace("]", "$]$").replace("_", "\\_")
  }

 */




   /*

       }
    else if (params.MODEL == "ORACLE") {
      val mi = new NamedEntityJointModelInstance(fg.toFactorGraph, ex)
      mi.graph.factors.foreach { f =>
        if (f.name.startsWith("brack")) f.clamp()
      }
      mi
    }
    else if (params.TRAIN_MODE == "JOINT" || params.TRAIN_MODE == "NPJOINT") {
      new NamedEntityJointModelInstance(fg.toFactorGraph, ex)
    }
    else {
      new NamedEntityModelInstance(fg.toFactorGraph, ex)
    }


     def isExact = false


     override def usesClampedTraining = {
    params.TRAIN_MODE == "MARGINALIZE_SYNTAX"
  }

    */



//          if (params.BINARIZE_MODE == "LEFT") {
//            tree.binarize("LEFT_0MARKOV").removeNones().removeUnaryChains()
//          }
//          else {
//            tree.binarize("RIGHT_0MARKOV").removeNones().removeUnaryChains()
//          }
//        val tindex = index.setRange(low=10000000, high=params.PV_SIZE)
//        val cindex = index.setRange(low=0, high=9999999)



/*
 def getNamedEntityFeatures(datum: OntoDatum, index: Index[String], labels: Array[String], maxSeg: Int, params: NamedEntityParams): PotentialExample = {
    val slen = datum.slen
    val ner = datum.ner
    var tree = datum.tree
    tree.removeUnaryChains().removeNones().binarize()
    val tokens = datum.tokens.toArray
    val maxSeg = params.MAX_SEG
    val ex = new PotentialExample
    ex.attributes("slen") = slen.toString
    ex.attributes("maxseg") = params.MAX_SEG.toString
    ex.attributes("labels") = labels.mkString(" ")
    ex.attributes("words") = tokens.map(_.word).mkString(" ")
    ex.attributes("tags") = tokens.map(_.pos).mkString(" ")

    for (j <- slen to 1 by -1) {
//      var labeled = false
      for (i <- scala.math.max(0, j-maxSeg) to j-1) {
        val width = j-i
        val feats = nerFeatures(tokens, i, j)
/*        if (i == 0 && j == 3) {
          val cols = 3
          val sb = new StringBuilder
          for (x <- 0 until feats.size by cols) {
            for (y <- 0 until cols if x+y < feats.size) {
              sb.append(" & " + clean(feats(x+y)))
            }
            sb.append("\\\\ \n")
          }
          println(sb.toString)
        }*/
        val correctSpan = ner.containsSpan(i,j) || (width == 1 && !ner.coversSpan(i,j)) // "+" else ""
        val potname1 = "%s(%d,%d)".format("nerbracket", i, j)
        ex.potentials += new Potential(1.0, potname1, correctSpan)
        ex.features(potname1) = feats.map(f => new Feature(index.index(f), 1.0, 0))
        //          out.write("nerbracket(%d,%d)\t%s%s\n".format(i, j, correctSpan, feats.mkString(" ")))

        val correctLabel = (!ner.containsSpan(i,j) && width > 1) || (width == 1 && ner.coversSpan(i,j))
        val potname2 = "%s(%d,%d,%s)".format("nerlabel", i, j, "0")
        ex.potentials += new Potential(1.0, potname2, correctLabel)
        ex.features(potname2) = Array(new Feature(index.index("None"), 1.0, 0))
        //          out.write("nerlabel(%d,%d,0)\t%sNone\n".format(i, j, correctLabel))

        //         if (width == 1) {
        //            val builder = new StringBuilder()
        //            for (f <- feats) builder.append("O_" + f)
        val label = "O"
        val potname3 = "%s(%d,%d,%s)".format("nerlabel", i, j, "1")
        val isCorrect1 = width == 1 && !ner.containsSpan(i,j) && !ner.coversSpan(i,j)
        ex.potentials += new Potential(1.0, potname3, isCorrect1)
        if (width > 1) {
          ex.features(potname3) = Array[Feature]()
        }
        else {
          ex.features(potname3) = feats.map(f => new Feature(index.index(label + "_" + f), 1.0, 0))
        }
        //          }

        for (label <- labels) {
          //            val builder = new StringBuilder()
          //            for (f <- feats) builder.append(" " + label + "_" + f)
          val correctLabel = ner.containsSpanLabel(i,j,label)
          val potname4 = "%s(%d,%d,%s)".format("nerlabel", i, j, labels.indexOf(label)+2)
          ex.potentials += new Potential(1.0, potname4, correctLabel)
          ex.features(potname4) = feats.map(f => new Feature(index.index(label + "_" + f), 1.0, 0))
          //            out.write("nerlabel(%d,%d,%s)\t%s%s\n".format(i, j, labels.indexOf(label)+2, correctLabel, builder.toString.trim))
        }
      }
    }
    ex //new PotentialExample(attributes, potentials, featureMap)
  }
 */



/*
def addSyntacticPrediction(fg: FactorGraphBuilder, pots: Array[Potential], slen: Int) = {
  val groups = pots.filter(_.name.startsWith("brack")).groupBy{p =>
    val INDICES_PATTERN(start, end) = p.name
    (start.toInt, end.toInt)
  }
  val brackIdxs = Array.tabulate(slen+1, slen+1)( (x,y) => -1) //Array.ofDim[Int](slen+1, slen+1) //new ArrayBuffer[Int]()
  for (width <- 2 to slen; start <- 0 to (slen - width)) {
    val end = start + width
    val gpots = groups((start, end))
    brackIdxs(start)(end) = fg.addUnaryVariable("brackvar(%d,%d)".format(start, end),
      "brackfac(%d,%d)".format(start, end),
      gpots.head)
  }
  fg.addCKYFactorByIndices(brackIdxs.toArray.flatten.filter(_ >= 0), slen=slen)

  if (params.PREDICT_LABELS) {
    val lgroups = pots.filter(_.name.startsWith("spanLabel")).groupBy{p =>
      val INDICES_PATTERN(start, end) = p.name
      (start.toInt, end.toInt)
    }
    val labelIdxs = Array.fill[ArrayBuffer[Int]](slen+1, slen+1)(new ArrayBuffer[Int])
    for (width <- 2 to slen; start <- 0 to (slen - width)) {
      val end = start + width
      val gpots = lgroups((start, end))
      if (!gpots.isEmpty) {
        for (gpot <- gpots) {
          val LABEL_PATTERN1(label, s, e) = gpot.name
          labelIdxs(start)(end) += fg.addUnaryVariable("labelvar(%d,%d,%s)".format(start, end, label),
            "labelfac(%d,%d,%s)".format(start, end, label), gpot)
        }
        fg.addIsAtMost1FactorByIndices(brackIdxs(start)(end), labelIdxs(start)(end).toArray, "isAtMost(%d,%d)".format(start, end))
      }
    }
  }
}


def addUnaryPrediction(fg: FactorGraphBuilder, pots: Array[Potential], slen: Int) {
  val ugroups = pots.filter(_.name.contains("unary")).groupBy{p =>
    val INDICES_PATTERN(start, end) = p.name
    (start.toInt, end.toInt)
  }
  val brackIdxs = Array.ofDim[Int](slen+1, slen+1) //new ArrayBuffer[Int]()
  val labelIdxs = Array.fill[ArrayBuffer[Int]](slen+1, slen+1)(new ArrayBuffer[Int])
  for (start <- 0 until slen) {
    val end = start + 1
    val upots = ugroups((start, end))
    brackIdxs(start)(end) = fg.addUnaryVariable("unaryvar(%d,%d)".format(start, end),
      "unaryfac(%d,%d)".format(start, end),
      upots.head)
    if (upots.size > 1) {
      for (gpot <- upots.tail) {
        val UNARY_LABEL_PATTERN1(label, s, e) = gpot.name
        labelIdxs(start)(end) += fg.addUnaryVariable("unaryLabelvar(%d,%d,%s)".format(start, end, label),
          "unaryLabelfac(%d,%d,%s)".format(start, end, label),
          gpot)
      }
      fg.addIsAtMost1FactorByIndices(brackIdxs(start)(end), labelIdxs(start)(end).toArray, "isAtMost(%d,%d)".format(start, end))
    }
  }
}
*/




/*
 def inferSeg(graph: FactorGraph, slen: Int, maxSeg: Int, numLabels: Int=6, brackName: String="nerlabelvar"): Array[(Int, Int, String)] = { // }//[Int] = {
  //		val NER_LABEL_VARIABLE_PATTERN = new Regex("""nerlabelvar\(([0-9]+),([0-9]+),(.+)\)""")
  val NER_LABEL_VARIABLE_PATTERN = new Regex("""nerlabelvar\(([0-9]+),([0-9]+)\)""")

    val mu = new Array[Double](slen+1)
    val lens = new Array[Int](slen+1)
    val labs = new Array[Int](slen+1)
    val scores = Array.ofDim[Double](slen+1, slen+1, numLabels+1)

    println("# labels = " + numLabels)
    for (node <- graph.variables) {
      node.name match {
        case NER_LABEL_VARIABLE_PATTERN(ss, es) => {
          val b = node.getBeliefs(graph)
          System.err.println("BELIEFS of b: " + b.mkString(", "))
          val i = ss.toInt
          val k = es.toInt
          val noseg = Math.log(b(0)._2)
          for (lab <- 1 until numLabels) {
            val ans = Math.log(b(lab)._2) - noseg
            scores(i)(k)(lab) = ans
          }
        }
        case _=>
      }
    }

      mu(0) = 0
      for (k <- 1 to slen) {
        var best = Double.NegativeInfinity
        var bestLen = -1
        var bestLab = -1
        for (w <- 1 to Math.min(maxSeg, k)) {
          val i = k - w
          for (lab <- 1 until numLabels) {
            val cur = mu(i) + scores(i)(k)(lab)
            if ( cur > best ) {
              best = cur
              bestLen = w
              bestLab = lab
            }
          }
        }
        mu(k) = best
        lens(k) = bestLen
        labs(k) = bestLab
      }

    val res = new ArrayBuffer[(Int, Int, String)]
    var k = slen
    while (k > 0) {
      val w = lens(k)
      //			res += labs(k)
      //			res += (k)
      //			res += (k-w)
      res += ((k-w, k, labs(k).toString))
      k -= w
    }
    System.err.println("Ents found = " + res.size)
    return res.toArray
  }
 */
/*
    def addNamedEntityPrediction(model: FactorGraphBuilder, pots: Array[Potential], slen: Int, useSemiCRF: Boolean=true) = {
      var maxWidth = 0
      for (i <- 0 until pots.size) {
        pots(i).name match {
          case NER_SPAN_PATTERN(startstr, endstr) => {
            val start = startstr.toInt
            val end   = endstr.toInt
            val width = end-start
            if (width > maxWidth) maxWidth = width
            model.addVariable("nerspanvar(%d,%d)".format(start, end), 2)
            model.addUnaryFactor("nerspanvar(%d,%d)".format(start, end), "nerspanfac(%d,%d)".format(start, end), pots(i))
          }
          case NER_LABEL_PATTERN(startstr, endstr, label) => {
            val start = startstr.toInt
            val end   = endstr.toInt
            model.addVariable("nerlabelvar(%d,%d,%s)".format(start, end, label), 2)
            model.addUnaryFactor("nerlabelvar(%d,%d,%s)".format(start, end, label), "nerlabelfac(%d,%d,%s)".format(start, end, label), pots(i))
          }
          case _=> {
            					System.err.println("Could not match potential name: %s".format(pots(i).name))
          }
        }
      }
      for (start <- 0 until slen; end <- start+1 to slen if (end-start <= maxWidth)) {
        model.addIsAtMost1Factor(new Regex("nerspanvar\\(%d,%d\\)".format(start, end)), new Regex("nerlabelvar\\(%d,%d,.+\\)".format(start, end)), "nerIsAtMost(%d,%d)".format(start, end))
      }
      System.err.println("Order identified as " + maxWidth)
      if (useSemiCRF) model.addSegmentationFactor(new Regex("nerspanvar"), slen=slen, maxWidth=maxWidth)
    }
 */







/*
import narad.projects.bpdp._
import scala.collection.mutable.{ArrayBuffer, HashSet}
import scala.util.matching._
import narad.bp.structure._
import narad.bp.util._
import narad.nlp.parser.constituent._
import narad.nlp.ling._
import narad.nlp.trees.Token
import java.io.{File, FileWriter}
import narad.util._


class NamedEntityModel(var graph: FactorGraph) extends Model {
	val spanPattern   = """nerbracket\(([0-9]+),([0-9]+)\)""".r
	val labelPattern  = """nerlabel\(([0-9]+),([0-9]+),(.+)\)""".r
//	val indicesPattern   = """\(([0-9]+),([0-9]+)\)""".r


	def label(sidx: Int, eidx: Int, beliefs: Array[Potential]): String = {
		val lbeliefs = beliefs.filter(_.name.matches("nerlabel\\(%d,%d,(.+)\\)".format(sidx, eidx)))
		assert(lbeliefs.size > 0, "No label potentials present for span (%d,%d).".format(sidx, eidx))
		var maxv = Double.NegativeInfinity
		var maxl = new String
		for (b <- lbeliefs) {
			if (b.value > maxv) {
				val labelPattern(ai, pi, l) = b.name
				maxv = b.value
				maxl = l
			}
		}
		maxl
	}

	def decode(slen: Int): Array[(Int, Int, String)] = { //}: NamedEntityDatum = { //words: Array[String], tags: Array[String]): NamedEntityDatum = {
		System.err.println("decoding NER Model again...")
		val beliefs = graph.potentialBeliefs
		System.err.println("# of beliefs = " + beliefs.size)
		for (b <- beliefs) println(b)
		val maxSeg = 10
		val numLabels = 20
		inferSeg(slen, maxSeg, numLabels)
	}
/*
		val rents = new ArrayBuffer[(Int, Int, String)]
//		System.out.println("words = " + words.mkString(", "))
		val beliefs = graph.potentialBeliefs
//		val slen = words.size
		val ents = new ArrayBuffer[NamedEntity]
		for (b <- beliefs) {
			System.err.println("%s %s".format(b.name, b.value.toString))			
		}
		println
		for (b <- beliefs.filter(_.name.contains("nerbracket"))) {
			if (b.value > 0.5) {
//				println("NER ON at = " + b.name)
				val spanPattern(startstr, endstr) = b.name
				val start = startstr.toInt
				val end   = endstr.toInt
				val l = label(start, end, beliefs)
				rents += Tuple(start, end, l)
//				System.out.println("ent = " + words.slice(start, end).mkString(", "))
//				ents += new NamedEntity(words.slice(start, end), l, -1, start, end)
			}
		}
		rents.toArray
//		new NamedEntityDatum(slen, ents.toArray)
	}
	*/
	
	def inferSeg(slen: Int, maxSeg: Int, numLabels: Int=5, brackName: String="nerlabelvar"): Array[(Int, Int, String)] = { // }//[Int] = {
//		val NER_LABEL_VARIABLE_PATTERN = new Regex("""nerlabelvar\(([0-9]+),([0-9]+),(.+)\)""")
		val NER_LABEL_VARIABLE_PATTERN = new Regex("""nerlabelvar\(([0-9]+),([0-9]+)\)""")
		
		val mu = new Array[Double](slen+1)
		val lens = new Array[Int](slen+1)
		val labs = new Array[Int](slen+1)
		val scores = Array.ofDim[Double](slen+1, slen+1, numLabels+1)

		for (node <- graph.variables) {
			node.name match {
				case NER_LABEL_VARIABLE_PATTERN(ss, es) => {
					val b = node.getBeliefs(graph)
					System.err.println("BELIEFS of b: " + b.mkString(", "))
					val i = ss.toInt
					val k = es.toInt
					val noseg = Math.log(b(0)._2)
					for (lab <- 1 until numLabels) {
						scores(i)(k)(lab) = Math.log(b(lab)._2) - noseg
					}
				}
				case _=>
			}
			
			mu(0) = 0
			for (k <- 1 to slen) {
				var best = Double.NegativeInfinity
				var bestLen = -1
				var bestLab = -1
				for (w <- 1 to Math.min(maxSeg, k)) {
					val i = k - w
					for (lab <- 1 until numLabels) {
						val cur = mu(i) + scores(i)(k)(lab)
						if ( cur > best ) {
							best = cur
							bestLen = w
							bestLab = lab
						}
					}
				}
				mu(k) = best
				lens(k) = bestLen
				labs(k) = bestLab
			}
		}
	  
		val res = new ArrayBuffer[(Int, Int, String)]
		var k = slen
		while (k > 0) {
			val w = lens(k)
//			res += labs(k)
//			res += (k)
//			res += (k-w)
			res += Tuple(k-w, k, labs(k).toString)
			k -= w
		}
		System.err.println("Ents found = " + res.size)
		return res.toArray
	}
	
	/*
	vector<int> FactorGraph::infer_seg(int slen, const char *pref,
					   int maxSeg, int labels) {
	  char buf[BUFSIZ];
	  vector<double> mu(slen+1);
	  vector<int> lens(slen+1);
	  vector<int> labs(slen+1);
	  multi_array<double, 3> score(extents[slen+1][slen+1][labels+1]);
	  for ( int w = 1; w <= maxSeg && w <= slen; ++w ) {
	    for ( int i = 0; i <= (slen - w); ++i ) {
	      int k = i + w;
	      sprintf(buf, "%s(%d,%d)", pref, i, k);
	      Vertex v = get_node(buf);
	      Variable *var = static_cast<Variable *>(FG_[v].node);
	      int arity = var->arity();
	      dvec b(arity);
	      b = var->get_beliefs(v, FG_);
	      double noseg = log(b(0));
	      for ( int lab = 1; lab < labels; ++lab ) {
		score[i][k][lab] = log(b(lab)) - noseg;
	      }
	    }
	  }
	  mu[0] = 0;
	  for ( int k = 1; k <= slen; ++k ) {
	    double best = R_NegInf;
	    int bestLen = -1;
	    int bestLab = -1;
	    for ( int w = 1; w <= maxSeg && w <= k; ++w ) {
	      int i = k - w;
	      for ( int lab = 1; lab < labels; ++lab ) {
		double cur = mu[i] + score[i][k][lab];
		if ( cur > best ) {
		  best = cur;
		  bestLen = w;
		  bestLab = lab;
		}
	      }
	    }
	    mu[k] = best;
	    lens[k] = bestLen;
	    labs[k] = bestLab;
	  }

	  vector<int> res;
	  for ( int k = slen; k > 0; ) {
	    int w = lens[k];
	    res.push_back(labs[k]);
	    res.push_back(k);
	    res.push_back(k - w);
	    k -= w;
	  }

	  return res;
	}
	*/

/*
	def inferSeg(slen: Int, maxSeg: Int, numLabels: Int): Array[Int] = {
		val NER_LABEL_VARIABLE_PATTERN = """nerlabelvar\(([0-9]+),([0-9]+),(.+)\)""".r
		
		val mu = new Array[Double](slen+1)
		val lens = new Array[Int](slen+1)
		val labs = new Array[Int](slen+1)

		val scores = Array.ofDim[Double](slen+1, slen+1, numLabels+1)
		for (node <- graph.variables) {
			node.name match {
				case NER_LABEL_VARIABLE_PATTERN(ss, es, label) => {
					val b = node.getBeliefs(graph)
					val bvv = graph.variables.filter(_.name == "nerbrackvar(%s,%s)".format(ss, es))
					assert(bvv.size == 1)
					val bv = bvv(0)
					val bb = bv.getBeliefs(graph)
					val i = ss.toInt
					val k = es.toInt
					val noseg = Math.log(bb(0)._2)   //Math.log(b(0)._2)
					for (lab <- 1 until numLabels) {
						scores(i)(k)(lab) = Math.log(b(lab)._2) - noseg
					}
				}
				case _=>
			}
			
			mu(0) = 0
			for (k <- 1 to slen) {
				var best = Double.NegativeInfinity
				var bestLen = -1
				var bestLab = -1
				for (w <- 1 to Math.min(maxSeg, k)) {
					val i = k - w
					for (lab <- 1 until numLabels) {
						val cur = mu(i) + scores(i)(k)(lab)
						if ( cur > best ) {
							best = cur
							bestLen = w
							bestLab = lab
						}
					}
				}
				mu(k) = best
				lens(k) = bestLen
				labs(k) = bestLab
			}
		}
	  
		val res = new ArrayBuffer[Int]
		var k = slen
		while (k > 0) {
			val w = lens(k)
			res += labs(k)
			res += (k)
			res += (k-w)
			k -= w
		}
		return res.toArray
	}
*/	


	
	
	override def toString = graph.toString
}


object NamedEntityModel {
	val NER_SPAN_PATTERN           = """nerbracket\(([0-9]+),([0-9]+)\)""".r
	val NER_LABEL_PATTERN          = """nerlabel\(([0-9]+),([0-9]+),(.+)\)""".r
	val NER_INDICES_PATTERN        = """ner.+\(([0-9]+),([0-9]+).+""".r
	val SYNTAX_BRACK_PATTERN = """brack\(([0-9]+),([0-9]+)\)""".r
	val SYNTAX_LABEL_PATTERN = """brackLabel(.+)\(([0-9]+),([0-9]+)\)""".r
	val UNARY_BRACK_PATTERN  = """unary\(([0-9]+),([0-9]+)\)""".r
	val UNARY_LABEL_PATTERN  = """unaryLabel(.+)\(([0-9]+),([0-9]+)\)""".r

	def addNamedEntityPrediction(model: FactorGraphBuilder, pots: Array[Potential], slen: Int, useSemiCRF: Boolean) = {
		var maxWidth = 0
		for (i <- 0 until pots.size) {
			pots(i).name match {
				case NER_SPAN_PATTERN(startstr, endstr) => {
					val start = startstr.toInt
					val end   = endstr.toInt
					val width = end-start
					if (width > maxWidth) maxWidth = width
					model.addVariable("nerspanvar(%d,%d)".format(start, end), 2)
					model.addUnaryFactor("nerspanvar(%d,%d)".format(start, end), "nerspanfac(%d,%d)".format(start, end), Array(pots(i)))				
				}
				case NER_LABEL_PATTERN(startstr, endstr, label) => {
					val start = startstr.toInt
					val end   = endstr.toInt
					model.addVariable("nerlabelvar(%d,%d,%s)".format(start, end, label), 2)
					model.addUnaryFactor("nerlabelvar(%d,%d,%s)".format(start, end, label), "nerlabelfac(%d,%d,%s)".format(start, end, label), Array(pots(i)))									
				}
				case _=> {
//					System.err.println("Could not match potential name: %s".format(pots(i).name))
				}
			}
		}
		for (start <- 0 until slen; end <- start+1 to slen if (end-start <= maxWidth)) {
			model.addIsAtMost1Factor(new Regex("nerspanvar\\(%d,%d\\)".format(start, end)), new Regex("nerlabelvar\\(%d,%d,.+\\)".format(start, end)), "nerIsAtMost(%d,%d)".format(start, end))				
		}		
		System.err.println("Order identified as " + maxWidth)
		if (useSemiCRF) model.addSegmentationFactor(new Regex("nerspanvar"), slen=slen, maxWidth=maxWidth)
	}


	def addNamedEntityPredictionViaEPU(model: FactorGraphBuilder, pots: Array[Potential], slen: Int, maxWidth: Int) = {
//		System.err.println("In EPU construction method.")
//		System.err.println("slen = " + slen)
		val groups = pots.groupBy{pot => 
			val NER_INDICES_PATTERN(sidx, eidx) = pot.name
			(sidx.toInt, eidx.toInt)
		}		
//		System.err.println(groups.size + " groups found.")
		for (i <- 0 until slen; j <- i+1 to slen if (j-i) <= maxWidth) {
			val gpots = groups(Tuple2(i,j))
//			System.err.println("groups size = " + gpots.size)
//			System.err.println("gpots: \n" + gpots.mkString("\n"))
			val width = j-i
			val start = i
			val end = j
			model.addVariable("nerspanvar(%d,%d)".format(start, end), 2)
			model.addUnaryFactor("nerspanvar(%d,%d)".format(start, end), "nerspanfac(%d,%d)".format(start, end), Array(gpots(0)))				

			model.addVariable("nerlabelvar(%d,%d)".format(start, end), gpots.size-1)
			model.addTable1Factor("nerlabelvar(%d,%d)".format(start, end), "nerlabelfac(%d,%d)".format(start, end), gpots.slice(1, gpots.size))			
			model.addEPUFactor(new Regex("nerspanvar\\(%d,%d\\)".format(start, end)), 
												 new Regex("nerlabelvar\\(%d,%d\\)".format(start, end)), 
												 arity=gpots.size-1, "EPU(%d,%d)".format(start, end))
		}
		System.err.println("Order identified as " + maxWidth)
		if (maxWidth > 0) model.addSegmentationFactor(new Regex("nerspanvar"), slen=slen, maxWidth=maxWidth)
	}


	def construct(ex: PotentialExample, pots: Array[Potential]): NamedEntityModel = {
		println(ex.attributes.mkString("\n"))
		val slen = ex.attributes("@slen").toInt
		val maxWidth = 10 //ex.attributes.getOrElse("@order", "0").toInt
		val model = new FactorGraphBuilder(pots)
		addNamedEntityPredictionViaEPU(model, pots, slen=slen, maxWidth=maxWidth)			
//		addNamedEntityPrediction(model, pots, slen=slen, maxWidth > 0)			
//		model.check
		new NamedEntityModel(model.toFactorGraph)	
	}

}



	
	
	


class JointNamedEntityModel(var graph: FactorGraph) extends Model {
	val spanPattern   = """nerbracket\(([0-9]+),([0-9]+)\)""".r
	val labelPattern  = """nerlabel\(([0-9]+),([0-9]+),(.+)\)""".r

	def label(sidx: Int, eidx: Int, beliefs: Array[Potential]): String = {
		val lbeliefs = beliefs.filter(_.name.matches("nerlabel\\(%d,%d,(.+)\\)".format(sidx, eidx)))
		assert(lbeliefs.size > 0, "No label potentials present for span (%d,%d).".format(sidx, eidx))
		var maxv = Double.NegativeInfinity
		var maxl = new String
		for (b <- lbeliefs) {
			if (b.value > maxv) {
				val labelPattern(ai, pi, l) = b.name
				maxv = b.value
				maxl = l
			}
		}
		maxl
	}
	
		def decode(words: Array[String], tags: Array[String]): NamedEntityDatum = {
			System.out.println("words = " + words.mkString(", "))
			System.err.println("decoding NER Joint Model...")
			val beliefs = graph.potentialBeliefs
			val slen = words.size
			val ents = new ArrayBuffer[NamedEntity]
			for (b <- beliefs) {
				System.err.println("%s %s".format(b.name, b.value.toString))			
			}
			println
			for (b <- beliefs.filter(_.name.contains("nerbracket"))) {
				if (b.value > 0.5) {
	//				println("NER ON at = " + b.name)
					val spanPattern(startstr, endstr) = b.name
					val start = startstr.toInt
					val end   = endstr.toInt
					val l = label(start, end, beliefs)
					System.out.println("ent = " + words.slice(start, end).mkString(", "))
					ents += new NamedEntity(words.slice(start, end), l, -1, start, end)
				}
			}
			new NamedEntityDatum(slen, ents.toArray)
		}
	override def toString = graph.toString
}


object JointNamedEntityModel {
	val CONNECT_PATTERN = """connect\(([0-9]+),([0-9]+)\)""".r

	def construct(ex: PotentialExample, pots: Array[Potential]): JointNamedEntityModel = {
		val slen = ex.attributes("@slen").toInt
		val model = new FactorGraphBuilder(pots)
		NamedEntityModel.addNamedEntityPrediction(model, pots, slen, useSemiCRF=true)
		Parser.addBracketPrediction(model, pots, slen)
		addConnections(model, pots, slen)
		new JointNamedEntityModel(model.toFactorGraph)
	}
	
	def addConnections(model: FactorGraphBuilder, pots: Array[Potential], slen: Int): Unit = {
		for (i <- 0 until pots.size) {
			pots(i).name match {
				case CONNECT_PATTERN(startstr, endstr) => {
					val start = startstr.toInt
					val end = endstr.toInt
					if (end-start < slen) {
						model.addNandFactor(new Regex("nerspanvar\\(%d,%d\\)".format(start, end)),
						new Regex("brackvar\\(%d,%d\\)".format(start, end)),
						"connect(%d,%d)".format(start, end),
						Array(pots(i)))											
					}
				}
				case _=>
			}
		}
	}
}


	
	
	
	
	
	
// N NER brack vars with unary factors
// N label vars with L-arity named1 factors
// EPU factors between bvar and lvars
// add segment factor
	
/*
semi.ner.model <- function(pots, labels=20, add.brackets=FALSE, add.labels=FALSE) {
  slen <- attr(pots, "slen")
#  cat("@slen = ", slen, "\n")
  if ( add.brackets ) {
    if ( add.labels ) {
      p <- label.model(pots)
    } else {
      p <- brack.model(pots)
    }
  } else {
    p <- bpparser(exp(pots))
  }
  bidx <- grep("^nerbracket", names(pots))
  bvars <- sub("^nerbracket", "S", names(pots)[bidx])

  lidx <- grep("^nerlabel.*,0\\)", names(pots))
  lvars <- sub("^nerlabel\\((.*),0\\)", "L(\\1)", names(pots)[lidx])
  lfacs <- sub("^(nerlabel\\(.*),0\\)", "\\1,%d)", names(pots)[lidx])

  add.variables(p, bvars, arity=2)
  add.unary.factors(p, names(pots)[bidx], bvars)

  add.variables(p, lvars, arity=labels)
  add.named1.factors(p, lfacs, lvars)
  add.epu.factors(p, sub("^S", "epu", bvars), bvars, lvars)

  add.seg(p, slen, pref="S", max.seg=10)

  if ( add.brackets ) {
    spans <- bracket.spans(slen, max.span=10)

    ## Do we need implies instead of nand?
    ## It really shouldn't be called agree now
    add.implies.factors(p,
                        sprintf("agree(%d,%d)", spans$start, spans$end),
                        sprintf("S(%d,%d)", spans$start, spans$end),
                        sprintf("B(%d,%d)", spans$start, spans$end))
  }
  p
}
*/	
	      */




/*	
  def construct(pots: Array[Potential], slen: Int, useSemiCRF: Boolean): NamedEntityModel = {
    val model = new FactorGraphBuilder(pots)
    addNamedEntityPrediction(model, pots, slen, useSemiCRF)
    new NamedEntityModel(model.toFactorGraph)
  }
  */
/*		
		var maxWidth = 0
		for (i <- 0 until pots.size) {
			pots(i).name match {
				case NER_SPAN_PATTERN(startstr, endstr) => {
					val start = startstr.toInt
					val end   = endstr.toInt
					val width = end-start
					if (width > maxWidth) maxWidth = width
					fg.addVariable("nerspanvar(%d,%d)".format(start, end), 2)
					fg.addUnaryFactor("nerspanvar(%d,%d)".format(start, end), "nerspanfac(%d,%d)".format(start, end), Array(pots(i)))				
				}
				case NER_LABEL_PATTERN(startstr, endstr, label) => {
					val start = startstr.toInt
					val end   = endstr.toInt
					fg.addVariable("nerlabelvar(%d,%d,%s)".format(start, end, label), 2)
					fg.addUnaryFactor("nerlabelvar(%d,%d,%s)".format(start, end, label), "nerlabelfac(%d,%d,%s)".format(start, end, label), Array(pots(i)))									
				}
				case _=>
			}
		}
		for (start <- 0 until slen; end <- start+1 to slen if (end-start <= maxWidth)) {
			fg.addIsAtMost1Factor(new Regex("nerspanvar\\(%d,%d\\)".format(start, end)), new Regex("nerlabelvar\\(%d,%d,.+\\)".format(start, end)), "nerIsAtMost(%d,%d)".format(start, end))				
		}
//		if (useSemiCRF) fg.addSegmentationFactor(new Regex("nerspanvar"), slen=slen, maxWidth=maxWidth)
		new NamedEntityModel(fg.toFactorGraph)
	}		
*/



/*
    for (start <- 0 to slen-2; width <- 2 to slen) {
      val end = start + width
      model.addImpliesFactor(new Regex("nerspanvar(%d,%d)".format(start, end)),
                             new Regex("spanvar(%d,%d)".format(start, end)))
    }
*/


/*
if ( slen <= 2 ) return(data.frame(start=c(), end=c()))
subset(expand.grid(start=0:(slen-2), end=2:slen),
       (end - start) > 1 & (end - start) < slen & (end - start) <= max.span)

*/




