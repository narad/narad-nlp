package narad.nlp.disfluency
import narad.util.ArgParser
import narad.io.disfluency.{DisfluencyDatum, DisfluencyReader}
import narad.nlp.trees.{ConstituentTree => Tree}
import javax.management.remote.rmi._RMIConnection_Stub

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 1/24/13
 * Time: 4:43 PM
 * To change this template use File | Settings | File Templates.
 */
object DisfluencyEval {

  def main(args: Array[String]) {
    val options = new ArgParser(args)
    val goldFile = options.getString("--gold.syntaxfile")
    val testFile = options.getString("--test.file")
    val goldReader = new DisfluencyReader(options.getString("--gold.disfluency.file"),
                                          options.getString("--gold.syntax.file"),
                                          hasHeader = true)
    val testReader = new DisfluencyReader(options.getString("--test.disfluency.file"),
                                          options.getString("--test.syntax.file"),
                                          hasHeader = false)
    evaluate(goldReader, testReader, verbose=true)
  }

  def evaluate(golds: Iterable[(DisfluencyDatum, Tree)], tests: Iterable[(DisfluencyDatum, Tree)], verbose: Boolean=false): Double = {
    var reparCorrect = 0
    var reparTest = 0
    var reparGold = 0

    var pauseCorrect = 0
    var pauseGold = 0
    var pauseTest = 0

    var editCorrect = 0
    var editTest = 0
    var editGold = 0

    println(golds.size)
    println(tests.size)
    assert(golds.size == tests.size, "Number of gold instances (%d) not equal to number of test instances (%d)".format(golds.size, tests.size))
    golds.zip(tests).foreach { case(gold, test) =>
      val gdis = gold._1
      val gtree = gold._2.toSpans.filter(_.label == "EDITED")
      val tdis = test._1
      val ttree = test._2.toSpans.filter(_.label == "EDITED")
  //    for (gs <- gtree) if (ttree.contains(gs)) editCorrect += 1
      assert(false, "should not run code without fixing above line")
      editGold += gtree.size
      editTest += ttree.size
      println
      println("GINTS: " + gdis.intIndex.mkString("; "))
      println("TINTS: " + tdis.intIndex.mkString("; "))

      println("GREPS: " + gdis.repas.mkString("; "))
      println("TREPS: " + tdis.repas.mkString("; "))

      for (i <- 1 to tdis.slen) {
        if (tdis.wordIsFillPause(i)) pauseTest += 1
        if (gdis.wordIsFillPause(i)) pauseGold += 1
        if (tdis.wordIsFillPause(i) && gdis.wordIsFillPause(i)) pauseCorrect += 1

        if (tdis.wordIsDisfluent(i) && gdis.wordIsDisfluent(i)) reparCorrect += 1
        if (tdis.wordIsDisfluent(i)) reparTest += 1
        if (gdis.wordIsDisfluent(i)) reparGold += 1
      }
    }
    printF1(reparCorrect, reparTest, reparGold, label="Reparandum")
    printF1(pauseCorrect, pauseTest, pauseGold, label="Fill Pause")
    printF1(editCorrect, editTest, editGold, label="EDITED")
    return 0
  }

  def printF1(correct: Int, tcount: Int, gcount: Int, label: String=""): Double = {
    val prec = if (tcount == 0) 0 else correct * 1.0 / tcount
    val rec  = if (gcount == 0) 0 else correct * 1.0 / gcount
    val F1 = if(prec == 0 && rec == 0) 0 else 2 * (prec * rec / (prec + rec))
    System.err.println("%s Precision (%d/%d) = %f".format(label, correct, tcount, prec))
    System.err.println("%s Recall (%d/%d) = %f".format(label, correct, gcount, rec))
    System.err.println("%s F1 = %f".format(label, F1))
    F1
  }
}



/*
    System.out.println("Edit Span F1        = %f")
    System.out.println("Edit Span Precision = %f")
    System.out.println("Edit Span Recall    = %f")
    System.err.println("Disfluency Word Accuracy = %f".format(wordCorrect * 1.0 / wordTotal * 1.0))

    val fpPrec = if (pauseTest == 0) 0 else pauseCorrect * 1.0 / pauseTest
    val fpRec  = if (pauseGold == 0) 0 else pauseCorrect * 1.0 / pauseGold
    val fpF1 = if(fpPrec == 0 && fpRec == 0) 0 else 2 * (fpPrec * fpRec / fpPrec + fpRec)
    System.err.println("Fill Pause Precision (%d/%d) = %f".format(pauseCorrect, pauseTest, fpPrec))
    System.err.println("Fill Pause Recall (%d/%d) = %f".format(pauseCorrect, pauseGold, fpRec))
    System.err.println("Fill Pause F1 = %f".format(fpF1))

 */