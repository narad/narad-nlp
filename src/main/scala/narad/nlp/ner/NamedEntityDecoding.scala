package narad.nlp.ner

import narad.bp.structure.{FactorGraph, ModelInstance}
import narad.io.onto.OntoDatum
import narad.io.ner.NamedEntityDatum
import narad.nlp.trees.{ConstituentTreeFactory, Span}
import java.io.FileWriter
import util.matching.Regex
import collection.mutable.ArrayBuffer

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 4/10/14
 * Time: 6:53 PM
 */
trait NamedEntityDecoding {

  self: NamedEntityModel =>

  def decode(instance: ModelInstance): OntoDatum = {
    val slen    = instance.ex.attributes.getOrElse("slen", "-1").toInt
    val labels  = instance.ex.attributes.getOrElse("labels", "").split(" ")
    val words  = instance.ex.attributes.getOrElse("words", "").split(" ")
    val tags    = instance.ex.attributes.getOrElse("tags", "").trim.split(" ")
    val numLabels = labels.size + 2
    val beliefs = instance.marginals
    val segs = inferSeg(instance.graph, slen, params.MAX_SEG, numLabels=numLabels)
    val ners = segs.filter(_._3 > 1).map{ seg =>
      new NamedEntity(labels(seg._3-2), 0, seg._1, seg._2, tokens = words.slice(seg._1, seg._2))
    }
    val datum = new NamedEntityDatum(words, ners)
    val spans = if (params.MODEL == "JOINT" || params.MODEL == "ORACLE") {
      if (slen > params.MIN_PARSE_LEN) {
        labelDecoding(beliefs, slen) ++ unaryDecoding(beliefs)
      }
      else {
        unaryDecoding(beliefs)
      }
    }
    else {
      Array[Span]()
    }
    //    println("SPANS = " + spans.mkString("\n"))
    val tree = if (params.UNBINARIZE) {
      ConstituentTreeFactory.constructFromSpans(spans.toArray.filter(s => !s.label.contains("@")), slen, words, tags)
    }
    else {
      ConstituentTreeFactory.constructFromSpans(spans.toArray, slen, words, tags)
    }
    if (params.OUTPUT_NER_FILE != null) {
      val out = new FileWriter(params.OUTPUT_NER_FILE, true)
      out.write(datum.toString + "\n")
      out.close()
    }
    if (params.MODEL == "JOINT" && params.OUTPUT_SYNTAX_FILE != null) {
      val out = new FileWriter(params.OUTPUT_SYNTAX_FILE, true)
      out.write(tree.toString + "\n")
      out.close()
    }
    //   println("TREE = " + tree.toString)
    new OntoDatum(datum, tree)  //null.asInstanceOf[OntoDatum]
  }

  def inferSeg(graph: FactorGraph, slen: Int, maxSeg: Int, numLabels: Int=7, brackName: String="nerlabelvar"): Array[(Int, Int, Int)] = { // }//[Int] = {
  //		val NER_LABEL_VARIABLE_PATTERN = new Regex("""nerlabelvar\(([0-9]+),([0-9]+),(.+)\)""")
  val NER_LABEL_VARIABLE_PATTERN = new Regex("""nerlabelvar\(([0-9]+),([0-9]+)\)""")

    val mu = new Array[Double](slen+1)
    val lens = new Array[Int](slen+1)
    val labs = new Array[Int](slen+1)
    val scores = Array.ofDim[Double](slen+1, slen+1, numLabels+1)

    //    println("# labels = " + numLabels)
    val beliefs = graph.potentialBeliefs
//    for (bb <- beliefs) println(bb)
    for (node <- graph.variables) {
      node.name match {
        case NER_LABEL_VARIABLE_PATTERN(ss, es) => {
          val b = node.getBeliefs(graph)
          //         System.err.println("BELIEFS of b: " + b.mkString(", "))
          val i = ss.toInt
          val k = es.toInt
          val noseg = Math.log(b(0)._2)
          //          println("no seg = " + noseg)
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
        //         println("i = %d; k = %d; w = %d".format(i, k, w))
        for (lab <- 1 until numLabels) {
          val cur = mu(i) + scores(i)(k)(lab)
          //           println("  lab " + lab + " = " + cur)
          if ( cur > best ) {
            best = cur
            bestLen = w
            bestLab = lab
          }
        }
      }
      //        println("mu(" + k + ") = " + best)
      //        println("lens(" + k + ") = " + bestLen)
      //        println("labs(" + k + ") = " + bestLab)
      mu(k) = best
      lens(k) = bestLen
      labs(k) = bestLab
    }

    val res = new ArrayBuffer[(Int, Int, Int)]
    var k = slen
    while (k > 0) {
      val w = lens(k)
      //			res += labs(k)
      //			res += (k)
      //			res += (k-w)
      res += ((k-w, k, labs(k)))
      k -= w
    }
    //    System.err.println("Ents found = " + res.size)
    return res.toArray
  }

}
