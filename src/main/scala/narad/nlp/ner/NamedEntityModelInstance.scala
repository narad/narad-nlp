package narad.nlp.ner

import narad.bp.structure.{ModelInstance, FactorGraph}
import narad.bp.util.PotentialExample

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 8/17/13
 * Time: 5:27 PM
 */
class NamedEntityModelInstance(graph: FactorGraph, ex: PotentialExample) extends ModelInstance(graph, ex) {}

class NamedEntityMargModelInstance(graph: FactorGraph, ex: PotentialExample) extends NamedEntityModelInstance(graph, ex) {

  override def clampedFactors = {
    println("Latent Syntax Marginalization")
    graph.factors.filter{ f =>
      f.name.startsWith("nerlabel")
    }.iterator
  }
}

class NamedEntityLatentBinarizationInstance(graph: FactorGraph, ex: PotentialExample) extends NamedEntityModelInstance(graph, ex) {

  override def clampedFactors = {
    println("Latent Binarization")
    graph.factors.filter{ f =>
      f.name.startsWith("nerlabel") || (f.name.startsWith("brack") && f.isCorrect)
    }.iterator
  }
}









// class NamedEntityJointModelInstance(graph: FactorGraph, ex: PotentialExample) extends NamedEntityModelInstance(graph, ex) {}
