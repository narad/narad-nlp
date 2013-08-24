package narad.nlp.tagger

import narad.bp.structure._
import narad.bp.inference._
import narad.bp.optimize._
import narad.bp.util._
import narad.bp.util.index._
import narad.io.conll._
import java.io._
import collection.mutable.{HashMap, ArrayBuffer, HashSet}

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 2/10/13
 * Time: 10:20 AM
 * To change this template use File | Settings | File Templates.
 */

/*
object CoarseToFineTagger extends TaggerFeatures {

  def run(params: TaggerParams): TaggerClassifier = {
    val tagger =  new UnigramTaggerModel(params)
    val dict   = TagDictionary.construct(new CoNLLReader(params.TRAIN_FILE), mode=params.MODE)
    val tags = dict.all

    val index  = new HashIndex(params.PV_SIZE)


    var train = new CoNLLReader(params.TRAIN_FILE)
    val optimizer = new CoarseToFineOptimizer(tagger, params) with L2Regularizer
    val pv = optimizer.train(train.toIterable, dict, index, params)

    val test = new CoNLLReader(params.TEST_FILE)
    optimizer.test(test.toIterable, dict, index, pv, params)

    new TaggerClassifier(pv, dict, index, params)
  }
}

class CoarseToFineOptimizer(model: UnigramTaggerModel, options: OptimizerOptions) extends Optimizer(model, options) {
  val LABEL_PATTERN  = """ulabel\(([0-9]+),(.+)\)""".r

  def train(data: Iterable[CoNLLDatum], dict: TagDictionary, index: Index[String], options: OptimizerOptions): Array[Double] = {
    val DATA_SIZE = data.size
    var params = init(options.INIT_FILE, options.PV_SIZE)
    println(params.mkString("\n"))
    val tags = dict.all
    val uout = new FileWriter("unigrams.fidx.txt")
    val bout = new FileWriter("bigrams.fidx.txt")

    val bigram = new BigramTaggerModel(model.options)
    for (i <- 0 until options.TRAIN_ITERATIONS) {
      var batchCount = 0
      var processed = 0
      for (batch <- order(data, i, options)) {
        val updates = batch.map { datum =>
          val ex = model.getExample(datum, tags, index, model.options)
          ex.writeToFile(uout)
          val instance = if (i < 3) {
            model.constructFromExample(ex, params)
          }
          else {
            val margThreshold = 0.05
            val tmpInstance = model.constructFromExample(ex, params)
            infer(tmpInstance, options)

            val beliefs = tmpInstance.marginals
            val groups = beliefs.filter(_.name.startsWith("ulabel")).groupBy{pot =>
              val LABEL_PATTERN(widx, lidx) = pot.name
              widx.toInt
            }
            val fg = tmpInstance.graph.toBuilder
            val newpots = new ArrayBuffer[Potential]()
            newpots ++= ex.potentials

            val featureMap = new HashMap[String, Array[Feature]]
            featureMap ++= ex.features

            for (i <- 1 until datum.slen) {
              val bestCurr = groups(i).filter(f => f.value > margThreshold || f.isCorrect)
              val bestNext = groups(i+1).filter(f => f.value > margThreshold || f.isCorrect)


              if (!bestCurr.isEmpty && !bestNext.isEmpty) {
              // get features for factor
              val bfeats = model.bigramFeatures(datum, i, i+1, model.options)
              for (bl <- bestCurr; al <- bestNext) {
                val correct = (datum.postag(i) == bl && datum.postag(i+1) == al)
                val feats = bfeats.view.map(f => index.index("%s-%s-%s".format(bl, al, f)))
               // newpots += Potential //bfeats.view.map(f => index.index("%s-%s-%s".format(bl, al, f)))
                  val potname = "blabel(%d,%d,%s,%s)".format(i, i+1, bl, al)
                  newpots += new Potential(1.0, potname, correct)
                  featureMap(potname) = feats.map(f => new Feature(index.index(bl + "-" + al + "_" + f), 1.0, 0)).toArray
                // add binary factor
                //fg.addTable2Factor("ulabel(%d,%d)")
              }


              }
              //      fg.addTable2Factor(varName1, varName2, arity1, arity2, "bigramFac(%s,%s)".format(v1, v2), bgroup._2)
            }
            System.err.println("ADDING %d BIGRAM POTS".format(newpots.size))
            val nex = new PotentialExample(ex.attributes, newpots, featureMap)
            nex.writeToFile(bout)

            bigram.constructFromExample(nex, params)
            //new ChainTaggerModelInstance(fg.toFactorGraph, ex)
          }
//          val instance = model.constructFromExample(ex, params)
          val (converged, inferIters) = infer(instance, options)
          update(instance, options)
        }
        val avg = average(updates)
        params = updateParams(params, avg, scale=(-1.0 * options.RATE), variance=(options.VARIANCE * DATA_SIZE))
        processed += batch.size
        System.err.println("Iter %d: %d/%d".format(i, processed, DATA_SIZE))
      }
      writeParams(params, i, options)
    }
    params
  }

  def test(data: Iterable[CoNLLDatum], dict: TagDictionary, index: Index[String], params: Array[Double], options: OptimizerOptions) {
    val DATA_SIZE = data.size
    //var params = init(options.INIT_FILE, options.PV_SIZE)
    val out = new FileWriter("test.fidxs.txt")
    val tags = dict.all
    for (datum <- data) {
      val ex = model.getExample(datum, tags, index, model.options)
      ex.writeToFile(out)
      val instance = model.constructFromExample(ex, params)
      val (converged, inferIters) = infer(instance, options)
      model.decode(instance)
    }
  }
}




/*
    val verbose = options.VERBOSE
    val time = options.TIME
    System.err.println("About to calculate data size:")
    val DATA_SIZE = data.size
    System.err.println("Finished.")
    System.err.println
    for (i <- 0 until options.TRAIN_ITERATIONS) {
      var batchCount = 0
      var startTime = System.currentTimeMillis()
      for (batch <- order(data, i, options)) {
        batchCount += 1
        var batchTime = System.currentTimeMillis()
        var numPots = 0
        var numIters = 0
        val updates = new Array[ParameterUpdate](options.BATCH_SIZE)
        var batchIndex = 0
        batch.par.foreach { ex =>
          val instance = model.constructFromExample(ex, params)
          val beliefs = instance.marginals
          numPots += ex.potentials.size

          val (converged, inferIters) = infer(instance, options)
          numIters += inferIters
          //          updates(i % options.BATCH_SIZE) = update(instance, options)
          updates(batchIndex) = update(instance, options)
          batchIndex += 1
        }

        val avg = average(updates)
        params = updateParams(params, avg, scale=(-1.0 * options.RATE), variance=(options.VARIANCE * DATA_SIZE))
      }
      writeParams(params, i, options)
    }
  }

}

*/
*/