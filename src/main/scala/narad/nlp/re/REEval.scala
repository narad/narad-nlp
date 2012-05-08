package narad.nlp.re
import narad.util.{ArgParser, ZippedReader}
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

object REEval {
	val relPattern = new Regex("rel\\(([0-9]+),([0-9]+)\\) +(.*)")
	val labPattern = new Regex("label\\(([0-9]+),([0-9]+),([0-9]+)\\).*\\+.*")
	
	def extractRelationsFIDX(lines: Array[String]): Array[DatumRelation] = {
		val rels = new ArrayBuffer[DatumRelation]
		for (line <- lines) {
			line match {
				case labPattern(i,j,label) => {
					rels += DatumRelation(label, "blah", i.toInt, j.toInt)
				}
				case _ =>
			}
		}
		rels.toArray
	}
	
	def extractRelationsBPDP(lines: Array[String]): Array[DatumRelation] = {
		val rels = new ArrayBuffer[DatumRelation]
		for (line <- lines) {
			line match {
				case relPattern(i,j, w) => {
					val weight = w.toDouble
					if (weight > 0.5) {
						var max = 0.0
						var maxLabel = "N/A"
						val lPattern = new Regex("label\\(%s,%s,([0-9]+)\\) +(.*)".format(i,j))
						for (lline <- lines) {
							lline match {
								case lPattern(label, w) => {
									val lweight = w.toDouble
									if (lweight >= max) {
										max = lweight
										maxLabel = label
									}
								}
								case _ =>
							}
						}
						rels += DatumRelation(maxLabel, "blah", i.toInt, j.toInt)
					}
				}
				case _ =>
			}
		}
		return rels.toArray
	}
	
	def main(args: Array[String]) {
		val options = new ArgParser(args)
		var ucorrect = 0.0; var lcorrect = 0.0
		var nrTest = 0.0
		var nrGold = 0.0
		
		val goldFile = options.getString("--gold.file")
		val testFile = options.getString("--test.file")
		for (pair <- ZippedReader.read(goldFile, testFile)) {
			val grels = extractRelationsFIDX(pair._1.split("\n"))
			val trels = extractRelationsBPDP(pair._2.split("\n"))
			var uc = 0.0
			var lc = 0.0
			for (grel <- grels) {
				for (trel <- trels) {
					if (grel.arg1 == trel.arg1 && grel.arg2 == trel.arg2) {
						uc += 1
						if (grel.label == trel.label) {
							lc += 1
						}
					}
				}
			}
			ucorrect += uc
			lcorrect += lc
			
			nrGold += grels.size
			nrTest += trels.size
		}
		
		val lp  = if (nrTest == 0) 0 else lcorrect / nrTest
		val lr  = if (nrGold == 0) 0 else lcorrect / nrGold
		val lf1 = if ((lp + lr) == 0) 0 else 2 * ((lp * lr) / (lp + lr))
		println("Labeled precision: %f".format(lp))
		println("Labeled recall: %f".format(lr))
		println("Labeled F1: %f".format(lf1))
		
		val up  = if (nrTest == 0) 0 else ucorrect / nrTest
		val ur  = if (nrGold == 0) 0 else ucorrect / nrGold
		val uf1 = if ((up + ur) == 0) 0 else 2 * ((up * ur) / (up + ur))
		println("Unlabeled precision: %f".format(up))
		println("Unlabeled recall: %f".format(ur))
		println("Unlabeled F1: %f".format(uf1))
	}
	
}