package narad.util
import java.io.FileWriter
import scala.util.Random
import narad.io.conll.CoNLLReader
import narad.io.onto._

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
    val treebank = options.getString("--treebank")
    val conll    = options.getString("--conll.file")
    val ner      = options.getString("--ner.file", "data/onto/train.name")
    val model  = options.getString("--model", "UNIGRAM")
    for (i <- 1 to nrexes) {
      if (model == "UNIGRAM") {
        generateUnigramTaggerExample(exsize, pvsize)
      }
      else if (model == "BIGRAM") {
        generateBigramTaggerExample(5, pvsize)
      }
      else if (model == "TRIGRAM") {
        generateTable3Example(5, pvsize)
      }
      else if (model == "CPARSE") {
        generateCParserExample(treebank, pvsize)
      }
      else if (model == "DPARSE") {
        generateDParserExample(conll, pvsize)
      }
      else if (model == "NER") {
        generateNERExample(ner, pvsize)
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
    for (i <- 1 to 10) {
      val arity = Random.nextInt(20)+1
      val correct = Random.nextInt(arity)+1
      for (j <- 1 to 5) {
        val numfeats = Random.nextInt(20)
        val label = if (j == correct) "+" else ""
        out.write("ulabel(%d,%d)\t%s%s\n".format(i, j-1, label, Seq.fill(numfeats+1)(Random.nextInt(pvsize-1)+1).mkString(" ")))
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
      val arity =  Random.nextInt(10)+1
      val correct = Random.nextInt(arity)+1
      for (j <- 1 to arity) {
        val numfeats = Random.nextInt(10)
        val label = if (j == correct) "+" else ""
        out.write("ulabel(%d,%d)\t%s%s\n".format(i, j-1, label, Seq.fill(numfeats+1)(Random.nextInt(pvsize-1)+1).mkString(" ")))
      }
      if (i > 1) {
        for (j <- 1 to arity; k <- 1 to prevarity) {
          val numfeats = Random.nextInt(10)+1
          val label = if (j == correct && k == prevcorrect) "+" else ""
          out.write("blabel(%d,%d,%d,%d)\t%s%s\n".format(i-1, i, j-1, k-1, label, Seq.fill(numfeats)(Random.nextInt(pvsize-1)+1).mkString(" ")))
        }
      }
      prevarity = arity
      prevcorrect = correct
    }
    out.write("\n")
    out.close()
  }

  def generateCParserExample(treebank: String, pvsize: Int) {
    val trees = narad.io.reader.TreebankReader.read(treebank, new ArgParser(Array[String]())).filter(_.size > 5).toArray
    val t = trees(Random.nextInt(trees.size-1)+1).binarize()
    t.annotateWithIndices(0)
    val size = t.slen
    val out = new FileWriter("train.fidx")
    out.write("@slen\t%d\n".format(size))
    for ( width <- 2 to size; start <- 0 to (size - width)) {
      val end = start+width
      val correct = if (t.containsSpan(start, end)) "+" else ""
      val numfeats = Random.nextInt(20)+1
      out.write("brack(%d,%d)\t%s%s\n".format(start, end, correct, Seq.fill(numfeats)(Random.nextInt(pvsize-1)+1).mkString(" ")))
    }
    out.write("\n")
    out.close()
  }

  def generateDParserExample(conll: String, pvsize: Int) {
    val reader = new CoNLLReader(conll)
    val trees = reader.toArray.filter(s => s.slen >= 5 && s.slen <= 40)
    val t = trees(Random.nextInt(trees.size-1)+1)
    val size = t.slen
    val out = new FileWriter("train.fidx")
    out.write("@slen\t%d\n".format(size))
    for (i <- 0 to size; j <- 1 to size if i != j) {
      val correct = if (t.head(j) == i) "+" else ""
      val numfeats = Random.nextInt(20)+1
      out.write("un(%d,%d)\t%s%s\n".format(i, j, correct, Seq.fill(numfeats)(Random.nextInt(pvsize-1)+1).mkString(" ")))
    }
    out.write("\n")
    out.close()
  }

  def generateNERExample(ner: String, pvsize: Int) {
    val reader = new NamedEntityReader(ner)
    val ners = reader.toArray.filter(s => s.slen >= 5 && s.slen <= 40)
    val t = ners(Random.nextInt(ners.size-1)+1)
    val size = t.slen
    val out = new FileWriter("train.fidx")
    out.write("@slen\t%d\n".format(size))

    out.write("\n")
    out.close()
  }

  def generateTable3Example(size: Int, pvsize: Int) {
    val out = new FileWriter("train.fidx")
    out.write("@slen\t%d\n".format(size))
    out.write("@dependency\ttrue\n")
    val i = 1

    val numfeats = Random.nextInt(20)+1

    val label = ""
    out.write("ulabel(%d,%d)\t%s%s\n".format(1, 0, label, Seq.fill(numfeats+1)(Random.nextInt(pvsize-1)+1).mkString(" ")))
    out.write("ulabel(%d,%d)\t%s%s\n".format(1, 1, "+", Seq.fill(numfeats+1)(Random.nextInt(pvsize-1)+1).mkString(" ")))

    out.write("ulabel(%d,%d)\t%s%s\n".format(4, 0, label, Seq.fill(numfeats+1)(Random.nextInt(pvsize-1)+1).mkString(" ")))
    out.write("ulabel(%d,%d)\t%s%s\n".format(4, 1, label, Seq.fill(numfeats+1)(Random.nextInt(pvsize-1)+1).mkString(" ")))
    out.write("ulabel(%d,%d)\t%s%s\n".format(4, 2, "+", Seq.fill(numfeats+1)(Random.nextInt(pvsize-1)+1).mkString(" ")))
    out.write("ulabel(%d,%d)\t%s%s\n".format(4, 3, label, Seq.fill(numfeats+1)(Random.nextInt(pvsize-1)+1).mkString(" ")))

    out.write("un(%d,%d,%d)\t%s%s\n".format(1, 4, 0, "+", Seq.fill(numfeats)(Random.nextInt(pvsize-1)+1).mkString(" ")))
    out.write("un(%d,%d,%d)\t%s%s\n".format(1, 4, 1, label, Seq.fill(numfeats)(Random.nextInt(pvsize-1)+1).mkString(" ")))

    out.write("tlabel(%d,%d,%d,%d,%d)\t%s%s\n".format(1, 4, 0, 0, 0, "", Seq.fill(numfeats)(Random.nextInt(pvsize-1)+1).mkString(" ")))
    out.write("tlabel(%d,%d,%d,%d,%d)\t%s%s\n".format(1, 4, 0, 0, 1, "", Seq.fill(numfeats)(Random.nextInt(pvsize-1)+1).mkString(" ")))
    out.write("tlabel(%d,%d,%d,%d,%d)\t%s%s\n".format(1, 4, 0, 0, 2, "", Seq.fill(numfeats)(Random.nextInt(pvsize-1)+1).mkString(" ")))
    out.write("tlabel(%d,%d,%d,%d,%d)\t%s%s\n".format(1, 4, 0, 0, 3, "", Seq.fill(numfeats)(Random.nextInt(pvsize-1)+1).mkString(" ")))
    out.write("tlabel(%d,%d,%d,%d,%d)\t%s%s\n".format(1, 4, 0, 1, 0, "", Seq.fill(numfeats)(Random.nextInt(pvsize-1)+1).mkString(" ")))
    out.write("tlabel(%d,%d,%d,%d,%d)\t%s%s\n".format(1, 4, 0, 1, 1, "", Seq.fill(numfeats)(Random.nextInt(pvsize-1)+1).mkString(" ")))
    out.write("tlabel(%d,%d,%d,%d,%d)\t%s%s\n".format(1, 4, 0, 1, 2, "+", Seq.fill(numfeats)(Random.nextInt(pvsize-1)+1).mkString(" ")))
    out.write("tlabel(%d,%d,%d,%d,%d)\t%s%s\n".format(1, 4, 0, 1, 3, "", Seq.fill(numfeats)(Random.nextInt(pvsize-1)+1).mkString(" ")))


    out.write("tlabel(%d,%d,%d,%d,%d)\t%s%s\n".format(1, 4, 1, 0, 0, "", Seq.fill(numfeats)(Random.nextInt(pvsize-1)+1).mkString(" ")))
    out.write("tlabel(%d,%d,%d,%d,%d)\t%s%s\n".format(1, 4, 1, 0, 1, "", Seq.fill(numfeats)(Random.nextInt(pvsize-1)+1).mkString(" ")))
    out.write("tlabel(%d,%d,%d,%d,%d)\t%s%s\n".format(1, 4, 1, 0, 2, "", Seq.fill(numfeats)(Random.nextInt(pvsize-1)+1).mkString(" ")))
    out.write("tlabel(%d,%d,%d,%d,%d)\t%s%s\n".format(1, 4, 1, 0, 3, "", Seq.fill(numfeats)(Random.nextInt(pvsize-1)+1).mkString(" ")))
    out.write("tlabel(%d,%d,%d,%d,%d)\t%s%s\n".format(1, 4, 1, 1, 0, "", Seq.fill(numfeats)(Random.nextInt(pvsize-1)+1).mkString(" ")))
    out.write("tlabel(%d,%d,%d,%d,%d)\t%s%s\n".format(1, 4, 1, 1, 1, "", Seq.fill(numfeats)(Random.nextInt(pvsize-1)+1).mkString(" ")))
    out.write("tlabel(%d,%d,%d,%d,%d)\t%s%s\n".format(1, 4, 1, 1, 2, "", Seq.fill(numfeats)(Random.nextInt(pvsize-1)+1).mkString(" ")))
    out.write("tlabel(%d,%d,%d,%d,%d)\t%s%s\n".format(1, 4, 1, 1, 3, "", Seq.fill(numfeats)(Random.nextInt(pvsize-1)+1).mkString(" ")))

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
//    val valid = new Array[Boolean](prec+1)
    file1.getLines().zip(file2.getLines()).foreach {case(s1, s2) =>
      val ss1 = s1.substring(0, prec)
      val ss2 = s2.substring(0, prec)
      if (ss1 != ss2 ) {
        System.out.println("ERROR @ line %d: [%s] != [%s]".format(i, ss1, ss2))
      }
      i += 1
    }

  }
}