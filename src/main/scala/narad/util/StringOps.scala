package narad.util

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 5/29/13
 * Time: 4:39 PM
 */
object StringOps {

  def pad(str: String, len: Int, dir: String="RIGHT"): String = {
    val builder = new StringBuilder(str)
    dir match {
      case "LEFT" => {
        while (builder.size < len) {
          builder.insert(0, " ")
        }
      }
      case "RIGHT" => {
        while (builder.size < len) {
          builder.append(" ")
        }
      }
    }
    return builder.toString()
  }
}
