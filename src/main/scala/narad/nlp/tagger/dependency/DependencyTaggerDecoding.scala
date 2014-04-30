package narad.nlp.tagger.dependency

import narad.bp.structure.{Potential, ModelInstance}
import narad.nlp.tagger.MultiTaggedSentence
import collection.mutable.ArrayBuffer
import java.io.FileWriter
import narad.nlp.tagger.factorial.FactorialTaggerModel

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 2/9/14
 * Time: 7:24 AM
 */
trait DependencyTaggerDecoding {

  self: DependencyTaggerModel =>

  override def decode(instance: ModelInstance): MultiTaggedSentence = {
    val sparse = params.SPARSE
    val beliefs = instance.marginals
    val words   = instance.ex.attributes.getOrElse("words", "").split(" ")
    val attrs   = instance.ex.attributes.getOrElse("attrs", "").split(" ")
    val len = instance.ex.attributes.getOrElse("len", "-1").toInt

    println(words.mkString(" "))
    println(beliefs.mkString("\n") + "\n")

    val groups = beliefs.filter(_.name.startsWith(params.LABEL_NAME)).groupBy{pot =>
      val UNIGRAM_FAC_PATTERN(chain, slice, value) = pot.name
      (chain.toInt, slice.toInt)
    }

    val mts = new MultiTaggedSentence
    for (chain <- 0 until attrs.size) {
      val tags = new ArrayBuffer[String]()
      for (slice <- 1 to len) {
        val pots = groups((chain, slice))
//        println("POTS:")
//        println(pots.mkString("\n"))
        assert(!pots.isEmpty, "Pots for group %d are empty in decoding?".format(slice))
        val maxpot = narad.util.Functions.argmax[Potential](_.value, pots)
//        println("max pot = " + maxpot)
        val UNIGRAM_FAC_PATTERN(mchain, mslice, mvalue) = maxpot.name
        if (sparse) {
          tags += dict.tagsOfAttributeOrAll(words(slice.toInt-1), attrs(mchain.toInt))(mvalue.toInt)
        }
        else {
//          println("mchain: " + mchain)
//          println("mvalue: " + mvalue)
//          println("attr: " + attrs(mchain.toInt))
          tags += dict.tagsOfAttribute(attrs(mchain.toInt))(mvalue.toInt)
        }
      }
      val out = new FileWriter("test.tagged.%s".format(attrs(chain)), true)
      out.write(tags.mkString("\n") + "\n")
      out.write("\n")
      out.close()
      mts.addTags(attrs(chain), tags.toArray)
    }
    mts
  }
}
