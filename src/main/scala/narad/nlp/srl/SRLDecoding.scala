package narad.nlp.srl

import narad.bp.structure.{ModelInstance, Potential}
import java.io.FileWriter

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 10/9/13
 * Time: 3:48 PM
 */
trait SRLDecoding {
  self: SRLModel =>

  private val SENSE_PATTERN  = """sense\(([0-9]+),([0-9]+)\)""".r
  private val SENSE_ROLE_PATTERN  = """senseHasArg\(([0-9]+),([0-9]+),([0-9]+),([0-9]+)\)""".r
  private val ARG_PATTERN    = """hasArg\(([0-9]+),([0-9]+)\)""".r
  private val ROLE_PATTERN  = """hasRole\(([0-9]+),([0-9]+),(.+)\)""".r
  private val DEP_SYNTAX_PATTERN = """un\(([0-9]+),([0-9]+)\)""".r
//  val CONNECT_PATTERN = """sslink\(([0-9]+),([0-9]+)\)""".r

  def decode(instance: ModelInstance, dict: SRLDictionary, params: SRLParams): SRLDatum = {
    val beliefs = instance.marginals

    val words  = instance.ex.attributes.getOrElse("words", "").split(" ")
    val tags   = instance.ex.attributes.getOrElse("tags", "").split(" ")
    val lemmas = instance.ex.attributes.getOrElse("lemmas", "").split(" ")
    //      val roles = instance.ex.attributes.getOrElse("roles", "").split(" ")
    val threshold = 0.5

    val pruneRolesByCount = params.PRUNE_ROLES_BY_COUNT
    val roleThreshold = params.ROLE_PRUNE_THRESHOLD

    val slen = words.size
    val preds = new Array[String](slen)
    val args = Array.ofDim[String](slen+1, slen+1)

    for (i <- 0 until slen) preds(i) = "_"
    for (i <- 0 to slen; j <- 0 to slen) args(i)(j) = "_"

    val groups = beliefs.groupBy { b =>
      b.name match {
        case SENSE_PATTERN(pidx, sidx) => ("SENSE", pidx, -1)
        case ARG_PATTERN(pidx, aidx) => ("ARG", pidx, aidx)
        case ROLE_PATTERN(pidx, aidx, label) => ("LABEL", pidx, aidx)
        case DEP_SYNTAX_PATTERN(hidx, cidx) => ("DP_CHILDREN", cidx, -1) // Grouped by child index
        case _=> -1
      }
    }

    for (i <- 0 to slen if groups.contains(("SENSE", i.toString, -1))) {
      val lemma = lemmas(i-1)
      val senses = dict.senses(lemma)
      if (senses.isEmpty) {
        preds(i-1) = dict.getDefaultSense(lemma)
      }
      else {
        val SENSE_PATTERN(pidx, sidx) = groups("SENSE", i.toString, -1).maxBy(_.value).name
        preds(i-1) = senses(sidx)
      }
      for (j <- 0 to slen if groups.contains("ARG", i.toString, j.toString)) {
        val agroup = groups("ARG", i.toString, j.toString)
        if (agroup.size > 0 && agroup(0).value > threshold) {
          val ROLE_PATTERN(lp, li, ll) = groups("LABEL", i.toString, j.toString).maxBy(_.value).name
          val roles = if (pruneRolesByCount) {
            dict.getRoles(roleThreshold)
          }
          else {
            dict.getRoles()
          }
          args(i)(j) = roles(ll)
        }
      }
    }

    // Syntactic decoding
//    println(beliefs.filter(_.name.startsWith("un")).mkString("\n"))
//    println(groups.keys.mkString("\n"))
    val heads = new Array[Int](slen)
    for (i <- 1 to slen) {
      heads(i-1) = groups.get("DP_CHILDREN", i.toString, -1) match {
        case Some(x) => {
          val DEP_SYNTAX_PATTERN(head, child) = x.maxBy(_.value).name
 //         println("head for " + i + " is " + head)
          head
        }
        case _=> {
  //        println("Head Not Found")
          0
        }
      }
    }

    // ID FORM LEMMA PLEMMA POS PPOS FEAT PFEAT HEAD PHEAD DEPREL PDEPREL FILLPRED PRED APREDs
    val pidxs = preds.zipWithIndex.filter(p => p._1 != "_").map(p => p._2 + 1)

    val grid = Array.ofDim[String](slen, 14 + pidxs.size)
    for (i <- 1 to slen) grid(i-1)(0) = i.toString
    for (i <- 0 until slen) grid(i)(1) = words(i)
    for (i <- 0 until slen) grid(i)(2) = lemmas(i)
    for (i <- 0 until slen) grid(i)(3) = lemmas(i)
    for (i <- 0 until slen) grid(i)(4) = tags(i)
    for (i <- 0 until slen) grid(i)(5) = tags(i)
    for (i <- 0 until slen) grid(i)(6) = "_"
    for (i <- 0 until slen) grid(i)(7) = "_"
    for (i <- 0 until slen) grid(i)(8) = heads(i).toString
    for (i <- 0 until slen) grid(i)(9) = heads(i).toString
    for (i <- 0 until slen) grid(i)(10) = "DEPREL"
    for (i <- 0 until slen) grid(i)(11) = "PDEPREL"
    for (i <- 0 until slen) grid(i)(12) = if (preds(i) == "_") "_" else "Y"
    for (i <- 0 until slen) grid(i)(13) = preds(i)
    var pcount = 1
    for (i <- pidxs) {
      for (j <- 1 to slen) {
        grid(j-1)(13 + pcount) = args(i)(j)
      }
      pcount += 1
    }

    val datum = new SRLDatum(grid)

    if (params.OUTPUT_FILE != null) {
      val out = new FileWriter(params.OUTPUT_FILE, true)
      out.write(datum.toString + "\n\n")
      out.close()
    }
    datum
  }

  def label(aidx: Int, pidx: Int, beliefs: Array[Potential]): String = {
    val lbeliefs = beliefs.filter(_.name.matches("hasRole\\(%s,%s,(.+)\\)".format(aidx, pidx)))
    var maxv = lbeliefs.map(_.value).max
    val labels = lbeliefs.filter(_.value == maxv).map { b =>
      val ROLE_PATTERN(ai, pi, l) = b.name; l
    }
    assert(labels.size > 0, "No labels found in SRL Decoding for %d, %d.".format(aidx, pidx))
    labels.head
  }
}
