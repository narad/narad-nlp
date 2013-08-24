package narad.io.disfluency

import util.matching.Regex
import narad.nlp.ling.{TaggedToken => Token}
import collection.mutable.HashSet

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 1/9/13
 * Time: 7:13 PM
 * To change this template use File | Settings | File Templates.
 */
class UtteranceReader(filename: String, hasHeader: Boolean = true) extends Iterable[DisfluencyDatum] {
  private var throughComments = false
  private val COMMENT_DELIM = new Regex("""=+""")
  private val EMPTY_PATTERN = """[ \n\t]*""".r //new Regex("""[ \n\t]*""")
  private val COMMENT_STRING = "============================================================================="

  def iterator: Iterator[DisfluencyDatum] = {
    throughComments = false
    val lines = scala.io.Source.fromFile(filename).getLines()
    Iterator.continually(next(lines)).takeWhile(_ != null)
  }

  def next(lines: Iterator[String]) : DisfluencyDatum = {
    if (!hasHeader) throughComments = true
    while (lines.hasNext) {
      val line = lines.next
//    Iterator.continually(lines.next()).takeWhile{line => lines.hasNext}.foreach { line =>
      line.trim match {
        case str if (str.startsWith("=====")) => throughComments = true // COMMENT_STRING => {println("Comment string found!"); throughComments = true}
        case str if (str.size == 0) => {}
        case _=> {
          if (throughComments) {
            val d = new DisfluencyDatum(line)
            d.index()
            return d
            // return new DisfluencyDatum(line)
          }
        }
      }
    }
    null.asInstanceOf[DisfluencyDatum]
  }
}

object UtteranceReader {

  def main(args: Array[String]) {
    val reader = new UtteranceReader(args(0))
    for (r <- reader) {
      println(r)
    }
  }
}

