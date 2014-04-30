package narad.nlp.qa

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 1/31/14
 * Time: 9:13 AM
 */

abstract class ExamQuestion(question: String) {

  def isMultipleChoiceQuestion: Boolean

  def isShortAnswerQuestion: Boolean
}

class MultipleChoiceQuestion(val text: String, val answers: Array[ExamAnswer]) extends ExamQuestion(text) {

  def isMultipleChoiceQuestion = true

  def isShortAnswerQuestion = false

  override def toString = {
    (Array(text) ++ answers.map { a =>
      " (%s) [%s]\t%s".format(a.label, if (a.isCorrect) "X" else " ", a.text)
    }).mkString("\n")
  }

}

class ShortAnswerQuestion(question: String) extends ExamQuestion(question) {

  def isMultipleChoiceQuestion = false

  def isShortAnswerQuestion = true
}

class ExamAnswer(val label: String, val text: String, correct: Boolean) {

  def isCorrect = correct
}

