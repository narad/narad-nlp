package narad.io.disfluency

import narad.io.tree.{DefaultTreebankReaderOptions, TreebankReader, TreebankReaderOptions}
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

  def iterator: Iterator[(DisfluencyDatum, Tree)] = {
    datums.iterator
  }

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
    System.err.print("\rProcessing (%s, %s)              ".format(disfluencyFile, treebankFile))
    val buff = new ArrayBuffer[(DisfluencyDatum, Tree)]()
    val ureader = new UtteranceReader(disfluencyFile, hasHeader)
    var trees = new TreebankReader(treebankFile, new DisfluencyTreebankReaderOptions()).toArray
    trees = trees.filter(_.getChildren(0).label != "CODE").toArray
  //     for (t <- trees)  println("OTREE = " + t)
    trees = filterTrees(trees)
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



  def filterTrees(treader: Iterable[Tree]): Array[Tree] = {
    val stack = new Stack[Tree]()
    treader.iterator.toArray.reverse.foreach(t => stack.push(t))
    val trees = new ArrayBuffer[Tree]()
    while (!stack.isEmpty) {
      val t = stack.pop()
      var split = false
      val tokens = t.tokens.toArray
//      println("Tree: " + t.toString)
//      println("Tokens: " + tokens.mkString(" "))
//      println("Leaves: " + t.leaves.mkString(" "))
//      println
      for (i <- 0 until tokens.size if !split) {
        if (tokens(i).pos == "-DFL-" && (tokens(i).word == "E_S" || tokens(i).word == "N_S")) {
          trees += t.slice(0, i+1)
          stack.push(t.slice(i+1, t.length))
          split = true
        }
      }
      if (!split) trees += t
    }
    trees.toArray.filter(_.words.size > 0)
  }

  def filterDisfluencies(dis: Array[DisfluencyDatum], trees: Array[Tree]): Array[DisfluencyDatum] = {
    val stack = Stack[DisfluencyDatum]()
    dis.reverse.foreach{d => if (d.words.size > 0) stack.push(d)}

    val da = new ArrayBuffer[DisfluencyDatum]()
    var i = 0
    //    System.err.println("unfiltered disfluencies = " + stack.size)
    try {
      while (!stack.isEmpty) {
        val d = stack.pop()
        val t = trees(i)
        val twords = t.tokens.filter(_.pos != "-DFL-").map(_.word).toArray
        val dwords = d.words.toArray
//        println("TTTREE: " + t)
//        println("T: " + twords.zipWithIndex.mkString(" "))
//        println("D: " + dwords.zipWithIndex.mkString(" "))
//        println
//        println(twords.size + " vs. " + dwords.size)
        if (twords.size < dwords.size) {
//          println("adding to DA: " + d.slice(0, twords.size).words.mkString(" "))
//          println("pushing: " + d.slice(twords.size, dwords.size).words.mkString(" "))
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
      case e: Exception => {
        System.err.println("Error filtering disfluencies...")
        println(e.getStackTrace.mkString("\n"))
        da.clear()
      }
    }
//    println("comps:")
    for (i <- 0 until da.size) {
//      println(i + ":")
//      println(i + ": " + da(i).words.mkString(" "))
//      println(i + ": " + trees(i).words.mkString(" "))
    }
        System.err.println("filtered disfluencies = " + da.size)
    da.toArray
  }
}



object DisfluencyReader {

  def main(args: Array[String]) {
//    class DisfluencyReader(disfluencyDir: String, treebankDir: String, hasHeader: Boolean = fals
//    val
  }
}















//    System.err.println("    BEFORE FILTERING: %d utterances vs %d trees.".format(ureader.size, treader.size))

/*
val afile = new File(alignFile)
val aligns = if (afile.exists()) {
  io.Source.fromFile(afile).getLines().toArray.filter(!_.isEmpty())
}
else {
  Array[String]()
}
*/


/*
      for (st <- t.depthFirstSearch) {
             if (st.label == "-DFL-" && (st.word == "E_S" || st.word == "N_S") && st.end != t.length && !split) {

        }
      }
*/

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