package narad.util
import java.io.FileWriter
import scala.util.Random
/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 11/25/12
 * Time: 12:47 AM
 * To change this template use File | Settings | File Templates.
 */
object RandomModelTester {

  def main(args: Array[String]) {
    val options = new ArgParser(args)
    val pvsize = options.getInt("--pv.size", 10)
    val nrexes = options.getInt("--nr.examples", 1)
    val exsize = options.getInt("--ex.size", 10)
    val model  = options.getString("--model", "UNIGRAM")
    for (i <- 1 to nrexes) {
      if (model == "UNIGRAM") {
        generateUnigramTaggerExample(exsize, pvsize)
      }
      else if (model == "BIGRAM") {
        generateBigramTaggerExample(5, pvsize)
      }
    }
//    val params = Seq.fill(pvsize)(0.3135)
    val params = Seq.fill(pvsize)(Random.nextDouble())
    val out = new FileWriter("model.pv")
    out.write(params.mkString("\n") + "\n")
    out.close()
  }

  def generateUnigramTaggerExample(size: Int, pvsize: Int) {
    val out = new FileWriter("train.fidx")
    out.write("@slen\t%d\n".format(size))
    for (i <- 1 to size) {
      val arity = Random.nextInt(20)+1
      val correct = Random.nextInt(arity)+1
      for (j <- 1 to arity) {
        val numfeats = Random.nextInt(20)
        val label = if (j == correct) "+" else ""
        out.write("ulabel(%d,%d)\t%s%s\n".format(i, j, label, Seq.fill(numfeats+1)(Random.nextInt(pvsize-1)+1).mkString(" ")))
      }
    }
    out.write("\n")
    out.close()
  }

  def generateBigramTaggerExample(size: Int, pvsize: Int) {
    val out = new FileWriter("train.fidx")
    out.write("@slen\t%d\n".format(size))
    out.write("@bigram\tTRUE\n")
    var prevarity = -1
    var prevcorrect = -1
    for (i <- 1 to size) {
      val arity =  40 //Random.nextInt(10)+1
      val correct = Random.nextInt(arity)+1
      for (j <- 1 to arity) {
        val numfeats = Random.nextInt(10)
        val label = if (j == correct) "+" else ""
        out.write("ulabel(%d,%d)\t%s%s\n".format(i, j-1, label, Seq.fill(numfeats+1)(Random.nextInt(pvsize-1)+1).mkString(" ")))
      }
      if (i > 1) {
        for (j <- 1 to arity; k <- 1 to prevarity) {
          val numfeats = Random.nextInt(10)
          val label = if (j == correct && k == prevcorrect) "+" else ""
          out.write("blabel(%d,%d,%d)\t%s%s\n".format(i, j-1, k-1, label, Seq.fill(numfeats+1)(Random.nextInt(pvsize-1)+1).mkString(" ")))
        }
      }
      prevarity = arity
      prevcorrect = correct
    }
    out.write("\n")
    out.close()
  }
}

object ParamVectorComparison {

  def main(args: Array[String]) {
    var i = 0
    val file1 = io.Source.fromFile(args(0))
    val file2 = io.Source.fromFile(args(1))
    val prec = 5
    val valid = new Array[Boolean](prec+1)
    file1.getLines.zip(file2.getLines).foreach {case(s1, s2) =>
      val ss1 = s1.substring(0, prec)
      val ss2 = s2.substring(0, prec)
      if (ss1 != ss2 ) {
        System.out.println("ERROR @ line %d: [%s] != [%s]".format(i, ss1, ss2))
      }
      i += 1
    }

  }
}