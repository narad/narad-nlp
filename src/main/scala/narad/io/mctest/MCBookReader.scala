package narad.io.mctest

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 1/31/14
 * Time: 11:47 AM
 */
class MCBookReader(bookFilename: String, headerFilename: String) extends Iterable[BookPassage] {

  def iterator: Iterator[BookPassage] = {
    val reader = io.Source.fromFile(bookFilename).getLines()
    reader.map { line =>
      val cells = line.split("\t")
      new BookPassage(new BookHeader(cells(0)), cells(1))
    }
  }
}

object MCBookReader {

  def main(args: Array[String]) {
    for (b <- new MCBookReader(args(0), args(1))) {
      println(b)
    }
  }
}

case class BookPassage(header: BookHeader, text: String) {

  override def toString = {
    text
  }
}

case class BookHeader(str: String) {}