package narad.io.util
import java.io.File
import scala.util.matching.Regex
import scala.io.Source
import collection.mutable.ArrayBuffer

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 3/21/13
 * Time: 4:17 PM
 * To change this template use File | Settings | File Templates.
 */
class DirectoryReader() {

  def listAllFiles(f: File): Array[File] = {
    if (f.isDirectory) {
      val files = new ArrayBuffer[File]()
      for (sf <- f.listFiles()) {
        files ++= listAllFiles(sf)
      }
      return files.toArray
    }
    else {
        return Array(f)
    }
  }

  def listAllFiles(f: File, pattern: String): Array[File] = {
    if (f.isDirectory) {
      val files = new ArrayBuffer[File]()
      for (sf <- f.listFiles()) {
        files ++= listAllFiles(sf, pattern)
      }
      return files.toArray
    }
    else {
      if (f.getName().matches(pattern)) {
        Array(f)
      }
      else {
        return Array()
      }
    }
  }
}
