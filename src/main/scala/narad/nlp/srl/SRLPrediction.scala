package narad.nlp.srl
import narad.nlp.parser.constituent.{ConstituentParserPrediction, ConstituentBracketFeatures}
import narad.bp.structure.{Potential, FactorGraphBuilder}
import collection.mutable.{HashMap, ArrayBuffer, HashSet}
import util.matching.Regex
import scala.Array

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 10/9/13
 * Time: 3:58 PM
 */
trait SRLPrediction {
  private val SENSE_PATTERN      = """sense\(([0-9]+),([0-9]+)\)""".r
  private val SENSE_ARG_PATTERN  = """senseHasArg\(([0-9]+),([0-9]+),([0-9]+)\)""".r
  private val SENSE_ROLE_PATTERN = """senseHasRole\(([0-9]+),([0-9]+),([0-9]+),([0-9]+)\)""".r
  private val ARG_PATTERN     = """hasArg\(([0-9]+),([0-9]+)\)""".r
  private val ROLE_PATTERN    = """hasRole\(([0-9]+),([0-9]+),(.+)\)""".r
  private val CONNECT_PATTERN2 = """sslink\(([0-9]+),([0-9]+)\)""".r
  private val CONNECT_PATTERN3 = """sslink\(([0-9]+),([0-9]+),([0-9]+)\)""".r

  implicit def string2Int(s: String): Int = augmentString(s).toInt

  def addSRLPrediction(graph: FactorGraphBuilder, pots: Iterable[Potential], slen: Int, params: SRLParams) {
    val roles = new HashSet[Int]
    val pidxs = new HashSet[Int]
    val aidxs = Array.ofDim[Int](slen+1, slen+1)
    val ridxs = Array.ofDim[Int](slen+1, slen+1, 1000)
    for (i <- 0 to slen; j <- 0 to slen) aidxs(i)(j) = -1
    for (i <- 0 to slen; j <- 0 to slen; k <- 0 until 1000) ridxs(i)(j)(k) = -1
    val sensePots = new ArrayBuffer[(Int, Potential)]()
    val senseArgPots = new ArrayBuffer[(Int, Potential)]()
    val senseRolePots = new ArrayBuffer[(Int, Int, Potential)]()
    val lastArg = new Array[Int](slen+1)
    val maxDist = params.MAX_DIST
    for (pot <- pots) {
      pot.name match {
        case SENSE_PATTERN(spidx, ssidx) => {
          pidxs += spidx //.toInt
          sensePots += ((spidx, pot))
        }
        case SENSE_ARG_PATTERN(spidx, soidx, ssidx) => {
          senseArgPots += ((spidx, pot))
        }
        case SENSE_ROLE_PATTERN(spidx, sridx, soidx, ssidx) => {
          senseRolePots += ((spidx, sridx, pot))
        }
        case ARG_PATTERN(pidx, aidx) => {
          if (params.MODEL_ARGS) {
            if (aidx > lastArg(pidx)) lastArg(pidx) = aidx
            aidxs(pidx)(aidx) = graph.addUnaryVariable("srlArgVar(%s,%s)".format(pidx, aidx),
                                                       "srlArgFac(%s,%s)".format(pidx, aidx), pot)
          }
        }
        case ROLE_PATTERN(pidx, aidx, ridx) => {
          if (params.MODEL_ROLES) {
            ridxs(pidx)(aidx)(ridx) = graph.addUnaryVariable("srlLabelVar(%s,%s,%s)".format(pidx, aidx, ridx),
                                                             "srlLabelFac(%s,%s,%s)".format(pidx, aidx, ridx), pot)
            roles += ridx
          }
        }
        case _=> {}
      }
    }
    for (pidx <- pidxs) {
      graph.addTable1Variable("senseVar(%d)".format(pidx), "senseFac(%d)".format(pidx), sensePots.filter(_._1 == pidx).map(_._2).toArray)
      for (aidx <- 1 to slen if aidxs(pidx)(aidx) >= 0) {   //Math.abs(pidx-aidx) <= maxDist) {
          graph.addIsAtMost1FactorByIndex(aidxs(pidx)(aidx), ridxs(pidx)(aidx).filter(_ >= 0), "srlIsAtMost(%d,%d)".format(pidx, aidx))
      }
    }
    // ROLE VALENCY
    if (params.MODEL_ROLE_VALENCY) {
      for (pidx <- pidxs) {
        for (ridx <- 0 until Math.min(roles.size, params.NUM_VALENCY_ROLES)) {
          var prevCidx = -1
          for (aidx <- 1 to slen if ridxs(pidx)(aidx)(ridx) > -1) {
            val cidx = graph.addVariable("%s(%d,%d,%d)".format("R", pidx, aidx, ridx), 3)
            if (prevCidx > -1) {
              graph.addTable3FactorByIndex(prevCidx, ridxs(pidx)(aidx)(ridx), cidx, 3, 2, 3, "rchainFac(%d,%d,%d)".format(pidx, aidx, ridx), transitionMatrix(3))  // 0, 1, 1
              if (aidx == lastArg(pidx)) {   // End of Chain
              val fpots = senseRolePots.filter(p => p._1 == pidx && p._2 == ridx).map(_._3).toArray
                graph.addTable2Factor("R(%d,%d,%d)".format(pidx, aidx, ridx), "%s(%d)".format("senseVar", pidx),
                  3, fpots.size / 3, "roleSenseTether(%d,%d)".format(pidx, ridx), fpots)
              }
            }
            else { // Start of Chain
              graph.addTable2FactorByIndex(ridxs(pidx)(aidx)(ridx), cidx, 2, 3,
                "vchainFac(%d,%d,%d)".format(pidx, aidx, ridx), transitionMatrix(2))
            }
            prevCidx = cidx
          }
        }
      }
    }
    // ARG VALENCY
    if (params.MODEL_ARG_VALENCY) {
      val numBins = params.NUM_VALENCY_ARGS
      for (pidx <- pidxs) {
        var prevCidx = -1
        for (aidx <- 1 to slen if aidxs(pidx)(aidx) > -1) {
          val cidx = graph.addVariable("A(%d,%d)".format(pidx, aidx), numBins)
          if (prevCidx > -1) {
            graph.addTable3FactorByIndex(prevCidx, aidxs(pidx)(aidx), cidx, numBins, 2, numBins, "achainFac(%d,%d)".format(pidx, aidx), atransitionMatrix(3, params.NUM_VALENCY_ARGS))  // 0, 1, 1
            if (aidx == lastArg(pidx)) {   // End of Chain
            val fpots = senseArgPots.filter(p => p._1 == pidx).map(_._2).toArray
              graph.addTable2Factor("A(%d,%d)".format(pidx, aidx), "%s(%d)".format("senseVar", pidx),
                numBins, fpots.size / numBins, "argSenseTether(%d)".format(pidx), fpots)
            }
          }
          else { // Start of Chain
            graph.addTable2FactorByIndex(aidxs(pidx)(aidx), cidx, 2, numBins,
              "avchainFac(%d,%d)".format(pidx, aidx), atransitionMatrix(2, numBins))
          }
          prevCidx = cidx
        }
      }
    }
  }


  def transitionMatrix(dim: Int): Array[Potential] = {
    if (dim == 3) {
      //                                       Ri  L  Ri+1
      Array(new Potential(1.0, "0,0,0", false),  // 0, 0, 0
        new Potential(0.0, "0,0,1", false),  // 0, 0, 1
        new Potential(0.0, "0,0,2", false),  // 0, 0, 1
        new Potential(0.0, "0,1,0", false),  // 0, 1, 0
        new Potential(1.0, "0,1,1", false),  // 0, 1, 0
        new Potential(0.0, "0,1,2", false),  // 0, 1, 1

        new Potential(0.0, "1,0,0", false),  // 0, 0, 0
        new Potential(1.0, "1,0,1", false),  // 0, 0, 1
        new Potential(0.0, "1,0,2", false),  // 0, 0, 1
        new Potential(0.0, "1,1,0", false),  // 0, 1, 0
        new Potential(0.0, "1,1,1", false),  // 0, 1, 0
        new Potential(1.0, "1,1,2", false),  // 0, 1, 1

        new Potential(0.0, "2,0,0", false),  // 0, 0, 0
        new Potential(0.0, "2,0,1", false),  // 0, 0, 1
        new Potential(1.0, "2,0,2", false),  // 0, 0, 1
        new Potential(0.0, "2,1,0", false),  // 0, 1, 0
        new Potential(0.0, "2,1,1", false),  // 0, 1, 0
        new Potential(1.0, "2,1,2", false))  // 0, 1, 1
    }
    else {
      Array(new Potential(1.0, "0,0", false),
        new Potential(0.0, "0,1", false),
        new Potential(0.0, "0,2", false),
        new Potential(0.0, "1,0", false),
        new Potential(1.0, "1,1", false),
        new Potential(0.0, "1,2", false))
    }
  }

  // havent add exception for exceeding last bin, ie, 2,1,2
  def atransitionMatrix(dim: Int, bins: Int): Array[Potential] = {
    val ab = new ArrayBuffer[Potential]()
    if (dim == 3) {
      for (i <- 0 until bins; j <- 0 to 1; k <- 0 until bins) {
        val pot = if ((j == 0 && k == i) || (j == 1 && k == i+1) || (i == bins-1 && k == bins-1)) 1.0 else 0.0
        ab += new Potential(pot, "%d,%d,%d".format(i,j,k), false)
      }
    }
    if (dim == 2) {
      for (i <- 0 until bins; k <- 0 until bins) {
        val pot = if (k == i) 1.0 else 0.0
        ab += new Potential(pot, "%d,%d".format(i,k), false)
      }
    }
    ab.toArray
  }


  def addConnectionPrediction(graph: FactorGraphBuilder, pots: Iterable[Potential]) {
    for (pot <- pots) {
      pot.name match {
        case CONNECT_PATTERN2(hidx, kidx) => {
          graph.addNandFactor(new Regex("srlArgVar\\(%s,%s\\)".format(hidx, kidx)),
                              new Regex("linkVar\\(%s,%s\\)".format(hidx, kidx)),
                              "sslink(%s,%s)".format(hidx, kidx), pot)
        }
        case CONNECT_PATTERN3(hidx, kidx, split) => {
          graph.addNand3Factor(new Regex("srlArgVar\\(%s,%s\\)".format(hidx, kidx)),
                               new Regex("linkVar\\(%s,%s\\)".format(hidx, split)),
                               new Regex("linkVar\\(%s,%s\\)".format(split, kidx)),
                               "sslink(%s,%s,%s)".format(hidx, kidx, split), pot)
        }
        case _=> {}
      }
    }
  }
}














//      graph.addExactly1Factor(new Regex("senseVar\\(%d,.+\\)".format(pidx)), "senseAtMost(%d)".format(pidx))
