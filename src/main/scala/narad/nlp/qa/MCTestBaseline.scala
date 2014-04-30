package narad.nlp.qa

import narad.io.mctest.{BookPassage, MCBookReader, MCTestReader}

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 1/31/14
 * Time: 9:50 AM
 */
class MCTestBaseline {

  // Sliding Window
  def baseline1(question: MultipleChoiceQuestion, passage: String, window: Int=4): ExamAnswer = {
    narad.util.Functions.argmax[ExamAnswer](slidingWindowScore(_, question.text, passage, window), question.answers)
  }

  def baseline2(question: MultipleChoiceQuestion, passage: String, window: Int=4): ExamAnswer = {
    narad.util.Functions.argmax[ExamAnswer](slidingWindowScore(_, question.text, passage, window), question.answers)
  }

  def slidingWindowScore(a: ExamAnswer, question: String, passage: String, window: Int): Double = {
    val pwords = passage.split(" ")
    val s = a.text.split(" ") ++ question.split(" ")
    var max = 0.0
    for (j <- 0 until pwords.size) {
      var c = 0.0
      for (w <- 0 until s.size) {
        val pw = pwords(j)
        if (s.contains(pw)) c += IC(pw, s)
      }
      if (c > max) max = c
    }
    max
  }

  def distanceScore(a: ExamAnswer, question: String, passage: String, stopwords: Seq[String]): Double = {
    val sq = question.split(" ").filter { t =>
      passage.contains(t) && !stopwords.contains(t) // May be too lenient if t is a common substring
    }
    val sa = a.text.split(" ").filter { t =>
      passage.contains(t) && !question.contains(t) && !stopwords.contains(t)
    }
    if (sq.isEmpty || sa.isEmpty) {
      return 1
    }
    else {
      val min = 0.0
      return 1.0 / (passage.split(" ").size - 1)
    }
  }

//    for (i <- 1 to 4) {
//      val u =
//      for (w <- pwords.sliding(window)) {
//
//      }
//    }
//  }

  def count(str: String, seq: Seq[String]): Int = seq.count(_ == str)

  def IC(str: String, seq: Seq[String]) = math.log(1 + (1 / count(str, seq)))

}

object MCTestBaseline {

  def main(args: Array[String]) {
    val questionFile = args(0)
//    val bookFile = args(1)
//    val headerFile = args(2)
//    val passages = new MCBookReader(bookFile, headerFile).iterator.toArray
    val baseline = new MCTestBaseline
    for (d <- new MCTestReader(questionFile)) {
      val answers = d.questions.map { q =>
        baseline.baseline1(q, d.passage)
      }
      println(answers.map(_.label).mkString("\t"))
    }
  }
}

object MCEval {

  def main(args: Array[String]) {
    val goldFile = args(0)
    val testFile = args(1)
    var numCorrect = 0.0
    var numQuestions = 0.0
    io.Source.fromFile(goldFile).getLines().zip(io.Source.fromFile(testFile).getLines()).foreach { case(g,t) =>
      val gs = g.split("\t")
      val ts = t.split("\t")
      assert(gs.size == ts.size, "Unequal number of answers for lines:\nGold:%s\nTest:%s".format(g,t))
      gs.zip(ts).foreach { case(ge, te) =>
        if (ge == te) numCorrect += 1
        numQuestions += 1
      }
    }
    println("Exam Accuracy: %.2f".format(numCorrect / numQuestions))
    println("%.0f questions, %.0f correct answers.".format(numQuestions, numCorrect))
  }
}