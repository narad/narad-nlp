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
    val reader = new SRLReader(options.getString("--srl.file"))
    val print  = options.getBoolean("--print", true)
    val filter = options.getBoolean("--filter", false)
    for (datum <- reader) {
      if (print && (!filter || datum.predicates.size > 0)) {
        println(datum)
        println
      }
    }
  }
}