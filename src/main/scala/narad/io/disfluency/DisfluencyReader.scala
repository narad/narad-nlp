package narad.io.disfluency

import narad.io.tree.TreebankReader
import narad.nlp.trees.{ConstituentTree => Tree}
import collection.mutable.{ArrayBuffer, Stack}
import collection.mutable
import java.io.File

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 1/9/13
 * Time: 7:14 PM
 * To change this template use File | Settings | File Templates.
 */
class DisfluencyReader(disfluencyDir: String, treebankDir: String, hasHeader: Boolean = false) extends Iterable[(DisfluencyDatum, Tree)] {
  lazy private val datums = populate

  def populate: Array[(DisfluencyDatum, Tree)] = {
    processDirs(disfluencyDir, treebankDir)
  }

  def processDirs(dDir: String, tDir: String): Array[(DisfluencyDatum, Tree)] = {
    println("Processing directory...")
    //    println("dDir = " + dDir)
    //    println("tDir = " + tDir)
    val buff = new ArrayBuffer[(DisfluencyDatum, Tree)]()
    System.err.print("Reading Disfluency Directory: " + dDir)
    val dfiles = new File(dDir).listFiles().filter(f => !f.isDirectory && f.getName().endsWith(".dps"))
    System.err.println(dfiles.size + " instance.s")
    System.err.print("Reading Syntax Directory: " + tDir)
    val tfiles = new File(tDir).listFiles().filter(f => !f.isDirectory && f.getName().endsWith(".mrg"))
    System.err.println(tfiles.size + " instances.")


    for (dfile <- dfiles) {
      val basename  = dfile.getName().substring(0, dfile.getName().lastIndexOf('.'))
      if (tfiles.map(_.getName).contains(basename + ".mrg")) {
        buff ++= processFile(dfile.toString, tDir + basename + ".mrg")
      }
    }
    buff.toArray
  }

  def processFile(disfluencyFile: String, treebankFile: String): Array[(DisfluencyDatum, Tree)] = {
    println("Processing (%s, %s)".format(disfluencyFile, treebankFile))
    val buff = new ArrayBuffer[(DisfluencyDatum, Tree)]()
    val ureader = new UtteranceReader(disfluencyFile, hasHeader)
    val treader = new TreebankReader(treebankFile).filter(_.children(0).label != "CODE").toArray
    println("    BEFORE FILTERING: %d utterances vs %d trees.".format(ureader.size, treader.size))

    /*
    val afile = new File(alignFile)
    val aligns = if (afile.exists()) {
      io.Source.fromFile(afile).getLines().toArray.filter(!_.isEmpty())
    }
    else {
      Array[String]()
    }
    */

    val trees = filterTrees(treader)
//    println("filtered trees = " + trees.size)
    val dis = filterDisfluencies(ureader.iterator.toArray, trees)
//    val als = filterAlignments(dis, aligns)
//    println(trees.size + " vs. " + dis.size + " vs. " + aligns.size)
    if (trees.size == dis.size) {
      for (i <- 0 until trees.size) {
        val d = dis(i)
      //  dis.setAlignments(aligns(i))
        buff += ((dis(i), trees(i)))
      }
    }
    else {
      System.err.println("ERROR PROCESSING <%s>: %d uterances vs %d trees  --  IGNORING.".format(disfluencyFile, dis.size, trees.size))
    }
    buff.toArray
  }

  def iterator: Iterator[(DisfluencyDatum, Tree)] = {
    datums.iterator
  }

  /*
  def filterAlignments(dis: Array[DisfluencyDatum], aligns: Array[String]): Array[Array[(Int, Int)]] = {
    val APATTERN = """\[ID=([0-9]+) TOKS=([0-9]+)\]: (.*)""".r
    val stack = new Stack[String]()
    aligns.reverse.foreach(a => stack.push(a))

    val buff = new ArrayBuffer[(Int, Int)]()
    while (!stack.isEmpty) {
      val t = stack.pop()

      }
    }
    */

  def filterTrees(treader: Iterable[Tree]): Array[Tree] = {
    val stack = new Stack[Tree]()
    treader.iterator.toArray.reverse.foreach(t => stack.push(t))

    val trees = new ArrayBuffer[Tree]()
    while (!stack.isEmpty) {
      val t = stack.pop()
      var split = false
      for (st <- t) {
        if (st.label() == "-DFL-" && (st.word() == "E_S" || st.word() == "N_S") && st.end() != t.slen && !split) {
          trees += t.slice(0, st.end())
          stack.push(t.slice(st.end(), t.slen))
          split = true
        }
      }
      if (!split) trees += t
    }
    trees.toArray
  }

  def filterDisfluencies(dis: Array[DisfluencyDatum], trees: Array[Tree]): Array[DisfluencyDatum] = {
    val stack = Stack[DisfluencyDatum]()
    dis.reverse.foreach(stack.push(_))

    val da = new ArrayBuffer[DisfluencyDatum]()
    var i = 0
//    System.err.println("unfiltered disfluencies = " + stack.size)
    try {
    while (!stack.isEmpty) {
      val d = stack.pop()
      val t = trees(i)
      val twords = t.tokens.filter(_.pos != "-DFL-").map(_.word)
      val dwords = d.words.toArray
 //     println("T: " + twords.mkString(" "))
 //     println("D: " + dwords.mkString(" "))
 //     println
      if (twords.size < dwords.size) {
        da += d.slice(0, twords.size)
        stack.push(d.slice(twords.size, dwords.size))
      }
      else {
        da += d
      }
      i += 1
    }
    }
    catch {
      case e: Exception => { System.err.println("Error filtering disfluencies..."); da.clear() }
    }
//    System.err.println("filtered disfluencies = " + da.size)
    da.toArray
  }
}
