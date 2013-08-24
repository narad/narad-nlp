package narad.io.srl

import narad.io.util.ChunkReader
import narad.nlp.srl.SRLDatum
import narad.util.ArgParser

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 1/4/13
 * Time: 7:40 PM
 * To change this template use File | Settings | File Templates.
 */
class SRLReader(filename: String) extends Iterable[SRLDatum]{

  def iterator: Iterator[SRLDatum] = {
    val reader = new ChunkReader(filename)
    reader.iterator.map(s => SRLDatum.constructFromCoNLL(s.split("\n")))
  }
}

object SRLReader {

  def main(args: Array[String]) {
    val options = new ArgParser(args)
    val print  = options.getBoolean("--print", true)
    val filter = options.getBoolean("--filter", false)
    val sort   = options.getBoolean("--sort", false)
    val min    = options.getInt("--min", 0)
    val max    = options.getInt("--max", 10000)
    val reader = if (sort) {
      new SRLReader(options.getString("--srl.file")).toList.sortBy(_.length)
    }
    else {
      new SRLReader(options.getString("--srl.file"))
    }
    for (datum <- reader) {
      if (print && (!filter || datum.predicates.size > 0) && datum.size > min && datum.size < max) {
        println(datum)
        println
      }
    }
  }
}