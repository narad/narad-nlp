package narad.util
import scala.util.matching.Regex

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 2/27/13
 * Time: 10:17 PM
 * To change this template use File | Settings | File Templates.
 */
trait StringPatterns {
  val NUMBER_PATTERN = new Regex(".*[0-9]+.*")

  def isCapitalized(str: String): Boolean = {
    str.toUpperCase == str
  }

}
