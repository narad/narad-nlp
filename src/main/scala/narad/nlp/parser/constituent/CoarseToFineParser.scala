package narad.nlp.parser.constituent
import narad.bp.optimize._
import narad.bp.structure._
import narad.bp.util._
import index.HashIndex
import narad.io.tree.TreebankReader
import narad.io.tree.StringToTreeOps
import java.io.FileWriter
import narad.bp.util.index._
import narad.nlp.trees.{Span, TreeFactory}
import collection.mutable.ArrayBuffer

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 3/19/13
 * Time: 10:16 PM
 * To change this template use File | Settings | File Templates.
 */


/*

class CoarseToFineParser {

}

object CoarseToFineParser {

  def main(args: Array[String]) {
    val params = new ConstituentParserParams(args)
    val model = new ConstituentBracketParser(params)

    val index = new HashIndex(params.PV_SIZE)

    if (params.EXTRACT_FEATURES) {
      val trees = new TreebankReader(params.TRAIN_FILE)
      val out = new FileWriter(params.TRAIN_FIDX_FILE)
      for (tree <- trees) {
        tree.annotateWithIndices()
        val btree = tree.removeNones().binarize()
        btree.annotateWithIndices()
        val pex = model.getBracketFeatures(btree, index, params)
        pex.attributes("tree") = btree.toString()
        pex.attributes("words") = btree.words.mkString(" ")
        pex.attributes("tags") = btree.tags.mkString(" ")
        pex.writeToFile(out)
      }
      out.close()
      System.err.println("Done extracting train features")

      val testtrees = new TreebankReader(params.TRAIN_FILE)
      val testout = new FileWriter(params.TEST_FIDX_FILE)
      for (tree <- testtrees) {
        tree.annotateWithIndices()
        val btree = tree.removeNones().binarize()
        btree.annotateWithIndices()
        val pex = model.getBracketFeatures(btree, index, params)
        pex.attributes("tree") = btree.toString()
        pex.attributes("words") = btree.words.mkString(" ")
        pex.attributes("tags") = btree.tags.mkString(" ")
        pex.writeToFile(testout)
      }
      testout.close()
      System.err.println("Done extracting test features")

    }

    val labels = new TreebankReader(params.TRAIN_FILE).map(_.nonterminals.map(_.label())).flatten.toArray.distinct
    println("LABELS(%d)\n%s".format(labels.size, labels.mkString("\n")))

    if (params.TRAIN) {
      val data = new PotentialReader(params.TRAIN_FIDX_FILE)
      val trainer = new CoarseToFineParserOptimizer(model, params)
      trainer.train(data, labels, index, params)
    }

    if (params.TEST) {
      val data = new PotentialReader(params.TEST_FIDX_FILE)
      val trainer = new CoarseToFineParserOptimizer(model, params)
      trainer.test(data, labels, index, params)
    }
  }
}


class CoarseToFineParserOptimizer(model: Model, cparams: ConstituentParserParams) extends Optimizer(model, cparams) with StringToTreeOps with ConstituentBracketFeatures with L2Regularizer {
  val brackModel = new ConstituentBracketParser(cparams)
  val labelModel = new ConstituentLabelParser(cparams)

  val BRACK_PATTERN = """brack\(([0-9]+),([0-9]+)\)""".r
  val LABEL_PATTERN = """spanLabel(.*)\(([0-9]+),([0-9]+)\).*""".r

  val INDICES_PATTERN = """.*\(([0-9]+),([0-9]+).*""".r
  val UNARY_LABEL_PATTERN = """unaryLabel(.*)\(([0-9]+),([0-9]+)\)""".r

  val THRESH = 0.5

  def train(data: Iterable[PotentialExample], labels: Array[String], index: Index[String], options: OptimizerOptions): Array[Double] = {
    var params = init(options.INIT_FILE, options.PV_SIZE)
    val verbose = options.VERBOSE
    val time = options.TIME
    System.err.print("About to calculate data size: ")
    val DATA_SIZE = data.size
    System.err.println(DATA_SIZE + ".")
    val NAN_CHECK = true

    for (i <- 0 until options.TRAIN_ITERATIONS) {
      var batchCount = 0
      println("iter " + i)
      var startTime = System.currentTimeMillis()
      for (batch <- order(data, i, options)) {
        batchCount += 1
        var batchTime = System.currentTimeMillis()
        var numPots = 0
        var numIters = 0
        val updates = new Array[ParameterUpdate](options.BATCH_SIZE)
        var batchIndex = 0
        batch.par.foreach { ex =>
          val instance = brackModel.constructFromExample(ex, params)
          println("\n\nBELIEFS-BEFORE:\n" + instance.marginals.mkString("\n"))
          val (converged, inferIters) = infer(instance, options)
          val beliefs = instance.marginals
          println("\n\nBELIEFS:\n" + beliefs.mkString("\n"))

          if (i < 5) {
            updates(batchIndex) = update(instance, options)
          }
          else {
            val tree = stringToTree(ex.attributes("tree"))
            tree.annotateWithIndices()
            System.err.println(beliefs.filter(_.value > THRESH).mkString("\n"))
            println("For sent len %d = %d".format(tree.slen, beliefs.filter(_.value > THRESH).size))
            println(beliefs.mkString("\n"))
            for (b <- beliefs.filter(_.value > THRESH)) {
              val BRACK_PATTERN(start, end) = b.name
              val feats = constituentSpanFeatures(tree.tokens(), start.toInt, end.toInt, cparams)
              for (label <- labels) {
                val correct = tree.containsSpan(start.toInt, end.toInt, label)
                val ff = feats.map(f => new Feature(index.index(label + "_" + f))).toArray
                ex.addPotential(Potential(1, "spanLabel%s(%s,%s)".format(label, start, end), correct), ff)
              }
            }
            System.err.println(ex.toString())
            System.err.println
            val ninstance = labelModel.constructFromExample(ex, params)
            System.err.println(ninstance.graph.toString())
            val (nconverged, ninferIters) = infer(ninstance, options)
            updates(batchIndex) = update(ninstance, options)
          }



          numPots += ex.potentials.size


 //         numIters += inferIters
          //          updates(i % options.BATCH_SIZE) = update(instance, options)
          batchIndex += 1
        }
        if (time) System.err.print("\rTRAINING: ...processing example %d/%d [Last took %fs. for %d pots. in %d BP iters]      ".format(
          batchCount*options.BATCH_SIZE, DATA_SIZE, (System.currentTimeMillis() - batchTime) / 1000.0, numPots, numIters))

        val avg = average(updates)
        params = updateParams(params, avg, scale=(-1.0 * options.RATE), variance=(options.VARIANCE * DATA_SIZE))
        if (NAN_CHECK) assert(!params.exists(_.isNaN), "NaN discovered at end of iteration %d.".format(i))

        if (verbose) System.err.println("PVV")
        if (verbose) params.zipWithIndex.foreach {case(p,pi) => System.err.println(pi + "\t" + p)}
      }
      if (time) {
        val etime = (System.currentTimeMillis() - startTime) / 1000.0
        if (etime > 60) {
          System.err.println("\rTRAINING: Finished Training Iteration %d [%fm.]                                       ".format(i, etime/60))
        }
        else {
          System.err.println("\rTRAINING: Finished Training Iteration %d [%fs.]                                       ".format(i, etime))
        }
        System.err.println("     Avg time of %fs per example.".format(etime / DATA_SIZE))
      }
      if (NAN_CHECK) assert(!params.exists(_.isNaN), "NaN discovered at end of iteration %d.".format(i))
      writeParams(params, i, options)
    }
    params
  }

  def test(data: Iterable[PotentialExample], labels: Array[String], index: Index[String], options: OptimizerOptions) {
    val THRESH = 0.5

    val params = init(options.INIT_FILE, options.PV_SIZE)
    for (ex <- data) {
//      val instance = model.constructFromExample(ex, params)
      val instance = brackModel.constructFromExample(ex, params)
      val slen    = instance.ex.attributes.getOrElse("slen", "-1").toInt
      val words   = instance.ex.attributes.getOrElse("words", "").trim.split(" ")
      val tags    = instance.ex.attributes.getOrElse("tags", "").trim.split(" ")
      val gtree    = stringToTree(ex.attributes("tree"))

      infer(instance, options)

      val beliefs = instance.marginals
      val ckybeliefs = labelModel.ckyBrackets(beliefs.filter(_.name.startsWith("brack(")), slen)
      for (b <- ckybeliefs.filter(_.value > THRESH)) {
        val BRACK_PATTERN(start, end) = b.name
        val feats = constituentSpanFeatures(gtree.tokens(), start.toInt, end.toInt, cparams)
        for (label <- labels) {
          val ff = feats.map(f => new Feature(index.index(label + "_" + f))).toArray
          ex.addPotential(Potential(1, "spanLabel%s(%s,%s)".format(label, start, end), false), ff)
        }
      }

      val mm = labelModel.constructFromExample(ex, params)
 //     labelModel.decode(mm)
      val spans = new ArrayBuffer[Span]

      val mbeliefs = mm.marginals

      val labelGroups = mbeliefs.filter(_.name.contains("spanLabel")).groupBy{p =>
        val INDICES_PATTERN(start, end) = p.name
        (start, end)
      }

      for (b <- mbeliefs.filter(p => p.value > 0.5 && p.name.startsWith("brack"))) {
        val BRACK_PATTERN(start, end) = b.name
        val label = if (labelGroups.contains((start, end))) {
          maxLabel(labelGroups((start, end)))
        }
        else {
          "EHH"
        }
        spans += new Span(start.toInt, end.toInt, label)
      }

      val tree = TreeFactory.constructFromSpans(spans.toArray.filter(s => !s.label.contains("@") && s.width != slen), slen)
      tree.annotateWithIndices(0)

      tree.setYield(words, tags)
  //    System.out.println(tree.toString() + "\n")

      val OUTPUT_FILE = "test.parsed"
      if (OUTPUT_FILE != null) {
        val out = new FileWriter(OUTPUT_FILE, true)
        out.write(tree.toString + "\n")
        out.close()
      }
    }
  }

  def maxLabel(pots: Array[Potential]): String = {
   		println("label potentials size = %d".format(pots.size) + "\n" + pots.mkString("\n"))
    var max = pots(0) //Double.NegativeInfinity
    for (pot <- pots) {
      if (pot.value > max.value) {
        max = pot
      }
    }
    //    println("max pot = " + max)
    if (max.name.contains("unary")) {
      val UNARY_LABEL_PATTERN(label, start, end) = max.name
      return label
    }
    else {
      val LABEL_PATTERN(label, start, end) = max.name
      return label
    }
  }
}

*/