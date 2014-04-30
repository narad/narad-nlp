package narad.io.mctest

import narad.nlp.qa._
import scala.util.matching.Regex
/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 1/30/14
 * Time: 4:09 PM
 */
class MCTestReader(filename: String) extends Iterable[MCTestDatum]{
  private val MC_LABELS = Array("A", "B", "C", "D")

  def iterator: Iterator[MCTestDatum] = {
    val reader = io.Source.fromFile(filename).getLines()
    reader.map{ l =>
      val fields = l.split("\t")
      val id = fields(0)
      val author = fields(1)
      val passage = fields(2).replaceAll("\\\\newline", " ")
      val questions = fields.slice(3, fields.size).grouped(5).map { g =>
        val gi = g.head.split(": ")
        val (qt, q) = (gi(0), gi(1))
        val as = g.tail.zipWithIndex.map { case(a,i) =>
          new ExamAnswer(MC_LABELS(i), a, false)
        }
        new MultipleChoiceQuestion(q, as)
      }.toIterable
      new MCTestDatum(id, author, passage, questions)
    }
  }
}

object MCTestReader {

  def main(args: Array[String]) {
    val reader = new MCTestReader(args(0))
    for (q <- reader) {
      println(q)
    }
    println(reader.size)
  }
}


case class MCTestDatum(id: String, author: String, passage: String, questions: Iterable[MultipleChoiceQuestion]) {

  override def toString = {
    passage + "\n" + questions.mkString("\n")
  }
}



/*
    //  private val QT_PATTERN = new Regex("(one|multiple|)\: (.*)")


        g.head match {
          case QT_PATTERN(qt, q) => {

          }
          case _=> {}
        }
        val (qt, q) = QT_PATTERN(g.head)
        val q = g.head
 */


/*    // Accidentally wrote the code for the regents data set
class MCTestReader(filename: String) extends Iterable[ExamQuestion]{
  val MC_QUESTION_PATTERN = """(.*) \(1\) (.*) \(2\) (.*) \(3\) (.*) \(4\) (.*)""".r

  def iterator: Iterator[ExamQuestion] = {
    val reader = io.Source.fromFile(filename).getLines()
    reader.map{ l =>
      println
      val fields = l.split("\t")
      fields(9) match {
        case MC_QUESTION_PATTERN(quest, one, two, three, four) => {
          val correct = fields(3).toInt
          new MultipleChoiceQuestion(quest, Array(new ExamAnswer("1", one,   correct == 1),
                                                  new ExamAnswer("2", two,   correct == 2),
                                                  new ExamAnswer("3", three, correct == 3),
                                                  new ExamAnswer("4", four,  correct == 4)))
        }
        case _=> {
          new ShortAnswerQuestion(fields(9))
        }
      }
    }
  }
}
*/