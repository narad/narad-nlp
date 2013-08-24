package narad.io.disfluency

import collection.mutable.{ArrayBuffer, HashSet}
import narad.nlp.ling.{TaggedToken => Token}

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 1/24/13
 * Time: 12:37 PM
 * To change this template use File | Settings | File Templates.
 */
class DisfluencyDatum(line: String) {
  private val DISFLUENCY_PATTERN = """\[ ([^\]]+) \+ (\{[^\]]*\})* ?([^\]]*) \]""".r
  private val NESTED_DISFLUENCY_PATTERN = """\[([^\]]+)\[""".r
  private val FILLPAUSE_PATTERN = """\{F ([^\}]+) \}""".r
  private val ALIGNMENT_PATTERN = """.*\<([^\>]+)\>.*""".r
  private val FULL_PATTERN = """\[ ([^\]]+) \+ (\{[^\]]*\})* ?([^\]]*) \] \<([^\>]+)\>""".r
  private val ALIGN_PATTERN = """([^ ]+) = ([^ ]+)""".r

  private val reparIndex  = new HashSet[(Int, Int)]()
  private val repairIndex = new HashSet[(Int, Int)]()
  private val interIndex  = new HashSet[(Int, Int)]()
  private val alignIndex  = new HashSet[(Int, Int)]()
  var indexed = false

  def slen = tokens.size

  def wordIsDisfluent(i: Int): Boolean = {
    if (!indexed) index()
    reparIndex.exists(p => p._1 <= i && p._2 > i)
  }

  def wordIsFillPause(i: Int): Boolean = {
    if (!indexed) index()
    interIndex.exists(p => p._1 <= i && p._2 > i)
  }

  def setFillPause(j: Int, k: Int) = {
    interIndex += ((j, k))
  }

  def setReparandum(i: Int, j: Int) = {
    reparIndex += ((i, j))
  }

  def setRepair(k: Int, l: Int) = {
    repairIndex += ((k, l))
  }

  def lline = line

  def intIndex = interIndex

  def repas = reparIndex

  def repairs = repairIndex

  def alignments = alignIndex

  def hasReparandum(i: Int, j: Int): Boolean = {
    if (!indexed) index()
    reparIndex.contains((i,j))
  }

  def hasInterregnum(i: Int, j: Int): Boolean = {
    if (!indexed) index()
    interIndex.contains((i,j))
  }

  def hasRepair(i: Int, j: Int): Boolean = {
    if (!indexed) index()
    repairIndex.contains((i,j))
  }

  def hasFillPause(i: Int, j: Int): Boolean = {
    if (!indexed) index()
    repairIndex.contains((i,j))
  }

  def hasDisfluency(i: Int, j: Int, k: Int, l: Int): Boolean = {
    hasReparandum(i,j) && hasInterregnum(j,k) && hasRepair(i,j)
  }

  def hasAlignment(i: Int, j: Int): Boolean = {
    if (!indexed) index()
    alignIndex.contains((i, j))
  }

  def index() {
//    System.err.println("Indexing :" + line)
    if (NESTED_DISFLUENCY_PATTERN.pattern.matcher(line).matches()) {
      // Do Nested Processing
   //      System.err.println("Ignoring due to nested disfluency: %s".format(line))
    }
    (DISFLUENCY_PATTERN findAllIn line).matchData.foreach {m =>
//      System.err.println("FOUND DIS: " + m.group(0))
      val reparStart = tokenOffset(m.start(1))
      val reparEnd   = tokenOffset(m.end(1))

      val repairStart = tokenOffset(m.start(3))
      val repairEnd   = tokenOffset(m.end(3))
      reparIndex  += ((reparStart, reparEnd))
      repairIndex += ((repairStart, repairEnd))
      if (reparEnd != repairStart) interIndex += ((reparEnd, repairStart))
    }
    (FILLPAUSE_PATTERN findAllIn line).matchData.foreach { m =>
      val iStart = tokenOffset(m.start(1))
      val iEnd   = tokenOffset(m.end(1))
      interIndex += ((iStart, iEnd))
    }
    indexAlignments()
    indexed = true
  }

  def indexAlignments() {
    val l = extractOriginalString(line)
    val aligns = (FULL_PATTERN findAllIn line).matchData.toArray.map { m =>
      line.substring(m.start(4), m.end(4))
    }

    val rtups = (DISFLUENCY_PATTERN findAllIn l).matchData.toArray.map { m =>
      (line.substring(m.start(1), m.end(1)), line.substring(m.start(3), m.end(3)), tokenOffset(m.start(1), str=l), tokenOffset(m.start(3), str=l))
    }
    assert(aligns.size == rtups.size, "# of alignments did not equal number of disfluencies.")
    rtups.zip(aligns).foreach {case(r, a) =>
      alignIndex ++= getAlignments(r._1, r._2, r._3, r._4, a)
    }
  }

  def isRealizedToken(str: String): Boolean = {
    str.contains("/")
  }

  def tokenOffset(i: Int, str: String=line): Int = {
    val toks = str.split(" ")
    var processed = 0
    var realTokIdx = 0
    for (t <- 0 until toks.size) {
      if (processed + toks(t).size > i) return realTokIdx
      if (isRealizedToken(toks(t))) realTokIdx += 1
      processed += toks(t).size + 1
    }
    -1
  }

  def tokens: Iterator[Token] = {
    line.split(" ").filter(isRealizedToken(_)).map{t => val c = t.split("/"); Token(c(0), c(1))}.iterator
  }

  def words = tokens.map(_.word)

  def tags = tokens.map(_.pos)

  def slice(start: Int, end: Int): DisfluencyDatum = {
 //   println("slice %d -> %d".format(start, end))
    val ll = line.split(" ")
    var s = -1
    var e = -1
    var i = 0
    for (j <- 0 until ll.size) {
      val l = ll(j)
      if (l.contains("/")) {
        if (start == i) s = j
        if (end == i) e = j
        i += 1
      }
    }
    if (e == -1) e = ll.size
    assert(s != -1 && e != -1, "Index not found in Disfluency slice.")
//    println("slice string = " + ll.slice(s, e).mkString(" "))
    new DisfluencyDatum(ll.slice(s,e).mkString(" "))
  }

  def extractOriginalString(str: String): String = {
    var l = str
    (FULL_PATTERN findAllIn l).matchData.toArray.reverse.foreach { m =>
      l = l.substring(0, m.start(4)-2) + l.substring(m.end(4)+1)
    }
    l
  }

  def getAlignments(repar: String, repair: String, start1: Int, start2: Int, alignStr: String): Array[(Int, Int)] = {
    val r1 = repar.split(" ").map(_.split("/")(0))
    val r2 = repair.split(" ").map(_.split("/")(0))
    //    println("REPAR: " + repar)
    //    println("REPAIR: " + repair)
    //    println("ALIGN: " + alignStr)
    val ab = new ArrayBuffer[(Int, Int)]
    (ALIGN_PATTERN findAllIn alignStr).matchData.foreach { m =>
      val s1 = alignStr.substring(m.start(1), m.end(1)).trim
      val s2 = alignStr.substring(m.start(2), m.end(2)).trim
      //      println("S1 = " + s1)
      //      println("S2 = " + s2)
      val a1 = r1.indexOf(s1)
      val a2 = r2.indexOf(s2)
      if (a1 > -1 && a2 > -1)  ab += ((start1 + a1, start2 + a2-1))
    }
    ab.toArray
  }

  override def toString = {
    val sb = new StringBuilder
    val toks = tokens.toArray
//    System.err.println("DECODING: " + interIndex.mkString(" "))
//    System.err.println("REPS: " + reparIndex.mkString(" "))
//    System.err.println("INTS: " + interIndex.mkString(" "))
//    System.err.println("REPAIRS: " + repairIndex.mkString(" ")  )
    for (i <- 0 until slen) {
//      System.err.println(i)
      if (reparIndex.exists(_._1 == i)) sb.append("[ ")
      if (reparIndex.exists(_._2 == i)) sb.append("+ ")
      if (interIndex.exists(_._1 == i)) sb.append("{F ")
      sb.append(toks(i).word + "/" + toks(i).pos + " ")
      if (interIndex.exists(_._2 == i+1)) sb.append("} ")
      if (repairIndex.exists(_._2 == i+1)) sb.append("] ")
    }
    if (interIndex.exists(_._2 == slen)) sb.append("} ")
    if (repairIndex.exists(_._2 == slen)) sb.append("] ")
    sb.toString
  }
}