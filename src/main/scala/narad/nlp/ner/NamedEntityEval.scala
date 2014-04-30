package narad.nlp.ner

import narad.io.ner.{F1Container, NamedEntityReader, NamedEntityDatum}
import java.io.FileWriter

//import narad.io.onto.{NamedEntityDatum, NamedEntityReader}
import narad.util.ArgParser
import narad.io.onto.OntoReader

object NamedEntityEval {

  def main(args: Array[String]) {
    val options = new ArgParser(args)
    val coarsen = options.getBoolean("--coarsen.labels")
    val gners = new NamedEntityReader(options.getString("--gold.file"), coarsen).iterator.toArray
    val tners = new NamedEntityReader(options.getString("--test.file"), coarsen).iterator.toArray
    assert(gners.size == tners.size, "# of gold predictions(%d) does not match # of test predictions(%d).".format(gners.size, tners.size))
    var ec = new F1Container
    for (i <- 0 until gners.size) {
      ec = ec.combine(gners(i).score(tners(i)))
    }
    System.err.println(ec.toVerboseString)

    if (options.getString("--syntax.file") != null) {
      findSyntacticMismatches(options.getString("--gold.file"),
        options.getString("--test.file"),
        options.getString("--syntax.file"))
    }

    val conllFile = options.getString("--conll.file")
    if (conllFile != null) toConllEval(gners, tners, conllFile)
  }

  def toConllEval(gners: Array[NamedEntityDatum], tners: Array[NamedEntityDatum], conllFile: String) {
    val cout = new FileWriter(conllFile)
    gners.zip(tners).foreach { case(gner, tner) =>
      for (i <- 0 until gner.size) {
        cout.write(Array(gner.words(i), "TAG", gner.getLabel(i), tner.getLabel(i)).mkString(" ") + "\n")
      }
      cout.write("\n")
    }
    cout.close()
  }

  def findSyntacticMismatches(goldFile: String, testFile: String, syntaxFile: String) {
    val gners = new OntoReader(goldFile, syntaxFile).iterator.toArray
    val tners = new OntoReader(testFile, syntaxFile).iterator.toArray
    gners.zip(tners).foreach { case(gonto, tonto) =>
      val tner = tonto.ner
      val gner = gonto.ner
      val tree = tonto.tree
      for (i <- 0 to tner.size; j <- 0 to tner.size) {
        if (gner.containsSpan(i,j) && !tner.containsSpan(i,j) && tree.containsSpan(i,j)) {
          println("i = %d, j = %d: %s".format(i, j, gner.words.slice(i,j).mkString(" ")))
          println("GOLD:\n" + gner)
          println("TEST:\n" + tner)
          println(tree)
          println
        }
      }
    }
  }
}










//gners.zipWithIndex(tners) //.foldLeft(new F1Container)   //{   case(gner, tner) =>









/*
object NamedEntityEval {

	def stats(gdatum: NamedEntityDatum, tdatum: NamedEntityDatum): F1Container = {
		val correct  = gdatum.entities.filter{e => tdatum.entities.exists(e==_)}.size
		val ucorrect = gdatum.entities.filter{e => tdatum.entities.exists(t => e.start == t.start && e.end == t.end)}.size
		val pden = tdatum.entities.size
		val rden = gdatum.entities.size
		return F1Container(correct, ucorrect, pden, rden)
	}
	
	def eval(gdatums: Array[NamedEntityDatum], tdatums: Array[NamedEntityDatum]): Double = {
		assert(gdatums.size == tdatums.size, "Number of datums in evaluation are not identical.")
		var correct = 0.0
		var ucorrect = 0.0
		var pden = 0.0
		var rden = 0.0
		gdatums.zip(tdatums).foreach { p =>
			val tups = stats(p._1, p._2)
			correct += tups.correct
			pden += tups.pden
			rden += tups.rden
		}
		val prec = if (pden == 0) 0.0 else correct / pden
		val rec  = if (rden == 0) 0.0 else correct / rden
		val f1 = F1(prec, rec)
		println("Prec = (%f / %f) = %f".format(correct, pden, prec))
		println("Recall = (%f / %f) = %f".format(correct, rden, rec))
		println("Prec = (%f / %f) = %f".format(prec, rec, f1))
		f1
	}
	
	def F1(prec: Double, rec: Double): Double = if (prec + rec == 0) 0 else 2 * (prec * rec) / (prec + rec)

  def main(args: Array[String]) = {
		val options = new ArgParser(args)
		val nerFile = options.getString("--ner.file")
		val tabFile = options.getString("--tab.file")
		if (nerFile != null && tabFile != null) {
			val gners = NamedEntityReader.read(options.getString("--ner.file"), options).toArray
			val tners = readTabFile(tabFile) 
			assert(gners.size == tners.size)
			eval(gners, tners)
		}
	}
	
	def readTabFile(filename: String): Array[NamedEntityDatum] = {
		val ners = new ArrayBuffer[NamedEntityDatum]
		val ents = new ArrayBuffer[NamedEntity]
		for (line <- io.Source.fromFile(filename).getLines) {			
			val cols = line.split("\t")
			if (line.isEmpty) {
				ners += new NamedEntityDatum(-1, ents.toArray)
				ents.clear
			}
			else {
				if (cols.size == 4) {
					ents += new NamedEntity(Array[String](), cols(3), cols(0).toInt, cols(1).toInt, cols(2).toInt)
				}
			}
		}
		ners.toArray
	}
}

case class F1Container(correct: Int, ucorrect: Int, pden: Int, rden: Int) {}
	
	
	               */
	
	
	