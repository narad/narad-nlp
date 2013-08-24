package narad.nlp.srl

import narad.bp.structure.{ModelInstance, FactorGraph}
import narad.bp.util.PotentialExample

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 8/16/13
 * Time: 9:19 AM
 */
class SRLModelInstance(fg: FactorGraph, ex: PotentialExample) extends ModelInstance(fg, ex)

class SRLHiddenModelInstance(fg: FactorGraph, ex: PotentialExample) extends ModelInstance(fg, ex) {

  override def hiddenVariableFactors = {
    fg.factors.filter{ f =>
      f.name.startsWith("linkFac") || f.name.startsWith("sslink")
    }
  }
}