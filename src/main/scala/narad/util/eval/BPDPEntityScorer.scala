package narad.util.eval

import narad.io.ner._
import narad.nlp.ner._

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 11/21/13
 * Time: 4:58 PM
 */
object BPDPEntityConverter {
  private val ENTITY_PATTERN = """([0-9]+)\t([0-9]+)\t([0-9]+)\t([0-9]+)""".r

  def main(args: Array[String]) {
    val bpdpFile = io.Source.fromFile(args(0))
    val labels = io.Source.fromFile(args(1)).getLines().toArray
    val nerFile = new narad.io.ner.NamedEntityReader(args(2))
    val segs = io.Source.fromFile(args(0)).getLines.foldLeft(Map[Int, List[String]]()){ case(acc, line) =>
      line match {
        case ENTITY_PATTERN(ex, start, end, label) => acc + (ex.toInt -> (acc.getOrElse(ex.toInt, List[String]()) ::: List(line)))
        case _=> acc
      }
    }
    nerFile.zipWithIndex.foreach { case(ner, i) =>
//      println(new NamedEntityDatum(ner.words, Array()))
      val test = new NamedEntityDatum(ner.words, segs.getOrElse(i+1, List()).toArray.map { line =>
        line match {
          case ENTITY_PATTERN(ex, start, end, label) => {
            new NamedEntity(labels(label.toInt-1), ex.toInt, start.toInt, end.toInt)
          }
        }
      }.filter(_.label != "O"))
      println(test)
    }
  }
}

//
        // //segs(i+1).toArray.map { line =>
//      println(new NamedEntityDatum(ner.words, segs(i+1).toArray.map { line =>
//        line match {
//          case ENTITY_PATTERN(ex, start, end, label) => {
//            new NamedEntity(labels(label.toInt), ex.toInt, start.toInt, end.toInt)
//          }
//        }
//      }))






















  /*
      .toArray.groupBy { line =>
      line match {
        case ENTITY_PATTERN(ex, start, end, label) => {
          ex.toInt
        }
        case _ => {
          -1
        }
      }
    }
    nerFile.zipWithIndex.foreach { case(ner, i) =>
      println(ner)
      println(segs(i).mkString("\n"))
    }
  }
}


.foldLeft(Map[String, List[String]]()){ case (acc, name) =>
findNamespace(name) match {
case Some(ns) => acc + (ns -> (name :: acc.get(ns).getOrElse(Nil)))
case _ => acc
}
}

         */





















    /*

   private val SCORE_PATTERN  = """acc  ([0-9]+) ([0-9]+) ([0-9]+).*""".r

    val bpdp
    var correct = 0.0
    var test = 0.0
    var gold = 0.0
    for (line <- io.Source.fromFile(args(0)).getLines) {
      line match {
        case ENTITY_PATTERN(ex, start, ) => {
          correct += correctStr.toInt
          test += testStr.toInt
          gold += goldStr.toInt
        }
        case _ => {}
      }
    }
    println("Precision = (%f/%f) = %f".format(correct, test, correct / test))
    println("Recall = (%f/%f) = %f".format(correct, gold, correct / gold))
    val prec = correct / test
    val rec = correct / gold
    println("F1 = %f".format(2 * (prec * rec) / (prec + rec)))
  }
}

  */















/*

  def main(args: Array[String]) {
    var correct = 0.0
    var test = 0.0
    var gold = 0.0
    for (line <- io.Source.fromFile(args(0)).getLines) {
      line match {
        case SCORE_PATTERN(correctStr, testStr, goldStr) => {
          correct += correctStr.toInt
          test += testStr.toInt
          gold += goldStr.toInt
        }
        case _ => {}
      }
    }
    println("Precision = (%f/%f) = %f".format(correct, test, correct / test))
    println("Recall = (%f/%f) = %f".format(correct, gold, correct / gold))
    val prec = correct / test
    val rec = correct / gold
    println("F1 = %f".format(2 * (prec * rec) / (prec + rec)))
  }

  */