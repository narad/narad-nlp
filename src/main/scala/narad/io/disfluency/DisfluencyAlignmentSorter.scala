package narad.io.disfluency

import narad.util.ArgParser
import java.io.FileWriter
import collection.mutable.ArrayBuffer

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 3/1/13
 * Time: 9:35 AM
 * To change this template use File | Settings | File Templates.
 */
object DisfluencyAlignmentSorter {                // [^ ]+ = [^ ]+
  private val DISFLUENCY_PATTERN = """\[ ([^\]]+) \+ (\{[^\]]*\})* ?([^\]]*) \]""".r
  private val ALIGNMENT_PATTERN = """.*\<([^\>]+)\>.*""".r
  private val FULL_PATTERN = """\[ ([^\]]+) \+ (\{[^\]]*\})* ?([^\]]*) \] \<([^\>]+)\>""".r
  private val ALIGN_PATTERN = """([^ ]+) = ([^ ]+)""".r

  private val NESTED_DISFLUENCY_PATTERN = """.*\[([^\]]+)\[.*""".r

  def main(args: Array[String]) {
    val options = new ArgParser(args)
    val alignmentFilename = options.getString("--alignment.file")
    val outputDirName = options.getString("--output.dir")

    var curFilename = new String
    var out = null.asInstanceOf[FileWriter]
    var scount = 0
    for (line <- io.Source.fromFile(alignmentFilename).getLines()) {
      if (line.startsWith("<<") && line.endsWith(">>")) {
        curFilename = line.substring(line.lastIndexOf("/")+1, line.size-2)
        if (out != null) out.close()
        out = new FileWriter(curFilename)
      }
      else {
        out.write(line + "\n")
      }
    }
    out.close
  }

    /*
      else if (NESTED_DISFLUENCY_PATTERN.pattern.matcher(line).matches()) {
//        println("DOUBLE DIS: " + line)
        out.write("[ID=%d TOKS=%d]: \n".format(scount, -1))
        scount += 1
      }
      else {
//        out.write(line + "\n")
        val l = extractOriginalString(line)
        val aligns = (FULL_PATTERN findAllIn line).matchData.toArray.map { m =>
           line.substring(m.start(4), m.end(4))
        }

        val rtups = (DISFLUENCY_PATTERN findAllIn l).matchData.toArray.map { m =>
          (line.substring(m.start(1), m.end(1)), line.substring(m.start(3), m.end(3)), tokenOffset(l, m.start(1)), tokenOffset(l, m.start(3)))
        }
        assert(aligns.size == rtups.size, "# of alignments did not equal number of disfluencies.")
        val alignments = rtups.zip(aligns).map {case(r, a) =>
          getAlignments(r._1, r._2, r._3, r._4, a)
        }.flatten
        out.write("[ID=%d;TOKS=%d]: %s\n".format(scount, l.trim.split(" ").size, alignments.mkString(" ")))
        out.write("\n")
        scount += 1
      }
    }
  }
*/


          /*
      else {
        var l = line
        (ALIGNMENT_PATTERN findAllIn line).matchData.toArray.reverse.foreach { m =>
          l = l.substring(0, m.start(1)) + l.substring(m.end(1))
        }
        out.write("[ID=%d;TOKENS=%d]:\t%s\n".format(scount, 0, l))
      }
      */


  def tokenOffset(line: String, i: Int): Int = {
    val toks = line.split(" ")
    var processed = 0
    var realTokIdx = 0
    for (t <- 0 until toks.size) {
      if (processed + toks(t).size > i) return realTokIdx
      if (isRealizedToken(toks(t))) realTokIdx += 1
      processed += toks(t).size + 1
    }
    -1
  }

  def isRealizedToken(str: String): Boolean = {
    str.contains("/")
  }
}


/*
. map{ m => m.start(1), m.end(1) }.reverse.foreach { m =>

        }

        .foreach { m =>
          val start = tokenOffset(m.start(1))
          val endnd   = tokenOffset(m.end(1))

        }
        ALIGNMENT_PATTERN findAllIn line
        line match {
          case ALIGNMENT_PATTERN(align) => {
            out.write("[ID=%d;TOKENS=%d]:\t%s".format(scount, 0, align))
          }
          case _ => {
            out.write("[ID=%d;TOKENS=%d]:\t".format(scount, 0))
          }
*/