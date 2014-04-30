package narad.io.re
import scala.util.parsing.json._
import scala.language.dynamics

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 4/9/14
 * Time: 11:06 AM
 */
class GoogleRelationReader(filename: String) extends Iterable[GoogleRelationSentence] {

  class CC[T] { def unapply(a:Any):Option[T] = Some(a.asInstanceOf[T]) }

  object M extends CC[Map[String, Any]]
  object L extends CC[List[Any]]
  object S extends CC[String]
  object D extends CC[Double]
  object B extends CC[Boolean]

  case class DatumConverter(pred: String, sub: String, obj: String, evidences: String) {}

  def iterator: Iterator[GoogleRelationSentence] = {
    println("Reading " + filename + " ...")
//    val json_string =
    for (datum <- scala.io.Source.fromFile(filename).getLines) {
      val data = JsonElement.parse(datum).get
      println(data)
      val cc = DatumConverter(data.pred, data.sub, data.obj, data.evidences.at(0).snippet)
      // println(cc)
    }
 //   println(json_string)

  //  val json_string = """{"pred":"/people/person/education./education/education/degree","sub":"/m/0g6g2m","obj":"/m/02h4rq6","evidences":[{"url":"http://en.wikipedia.org/wiki/Adam_Hecktman","snippet":"Prior to Microsoft, Adam was a consultant with Andersen Consulting for three years. While at Andersen Consulting, Adam worked with clients including those in financial services, government, and utilities. Adam received a ((NAM: Bachelor of Science)) in commerce and business administration from the University of Illinois at Urbana-Champaign. He also holds a Master of Business Administration degree."}],"judgments":[{"rater":"1701217270337547159","judgment":"yes"},{"rater":"16812935633072558077","judgment":"yes"},{"rater":"5521403179797574771","judgment":"yes"},{"rater":"8046943553957200519","judgment":"yes"},{"rater":"9448866739620283545","judgment":"yes"}]}"""

  //  val dd = JSON.parseFull(json_string) map (JsonElement(_))
  //  val data = scala.util.parsing.json.JSON.parseRaw(json_string)
  //  println(data.size)
  //  println(data.mkString("\n"))
//
//    data match {
//      case Some(m: Map[String, Any]) => m("pred") match {
//        case s: String => {
//          println(s)
//          s
//        }
//      }
//      case _=> {}
//    }

    println()
    return null.asInstanceOf[Iterator[GoogleRelationSentence]]
  }

}

object GoogleRelationReader {

  def main(args: Array[String]) {
    val reader = new GoogleRelationReader(args(0))
    for (r <- reader) {
      println(r)
    }
  }
}

class GoogleRelationSentence {


}



trait JsonElement extends Dynamic{ self =>
  def selectDynamic(field: String) : JsonElement = EmptyElement
  def applyDynamic(field: String)(i: Int) : JsonElement = EmptyElement
  def toList : List[String] = sys.error(s"is not a list.")
  def asString: String = sys.error(s"has no string representation.")
  def length$ : Int = sys.error(s"has no length")
}


object JsonElement{

  def ^(s: String) = {
    require(!s.isEmpty, "Element is empty")
    s
  }

  implicit def toString(e: JsonElement) : String = e.asString
  implicit def toBoolean(e: JsonElement) : Boolean = (^(e.asString)).toBoolean
  implicit def toBigDecimal(e: JsonElement) : BigDecimal = BigDecimal(^(e.asString))
  implicit def toDouble(e: JsonElement) : Double = ^(e.asString).toDouble
  implicit def toFloat(e: JsonElement) : Float = ^(e.asString).toFloat
  implicit def toByte(e: JsonElement) : Byte = ^(e.asString).stripSuffix(".0").toByte
  implicit def toShort(e: JsonElement) : Short = ^(e.asString).stripSuffix(".0").toShort
  implicit def toInt(e: JsonElement) : Int = ^(e.asString).stripSuffix(".0").toInt
  implicit def toLong(e: JsonElement) : Long = ^(e.asString).stripSuffix(".0").toLong
  implicit def toList(e: JsonElement) : List[String] = e.toList



  def parse(json: String) = JSON.parseFull(json) map (JsonElement(_))

  def apply(any : Any) : JsonElement = any match {
    case x : Seq[Any] => new ArrayElement(x)
    case x : Map[String, Any] => new ComplexElement(x)
    case x => new PrimitiveElement(x)
  }
}

case class PrimitiveElement(x: Any) extends JsonElement{
  override def asString = x.toString
}

case object EmptyElement extends JsonElement{
  override def asString = ""
  override def toList = Nil
}

case class ArrayElement(private val x: Seq[Any]) extends JsonElement{
  private lazy val elements = x.map((JsonElement(_))).toArray

  override def applyDynamic(field: String)(i: Int) : JsonElement = elements.lift(i).getOrElse(EmptyElement)
  override def toList : List[String] = elements map (_.asString) toList
  override def length$ : Int = elements.length
}

case class ComplexElement(private val fields : Map[String, Any]) extends JsonElement{
  override def selectDynamic(field: String) : JsonElement = fields.get(field) map(JsonElement(_)) getOrElse(EmptyElement)
}