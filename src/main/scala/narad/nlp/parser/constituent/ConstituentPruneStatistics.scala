package narad.nlp.parser.constituent
import narad.io.tree.{TreebankReader, TreebankReaderOptions}
import narad.nlp.trees.{ConstituentTree => Tree}
import narad.util.ArgParser
import collection.mutable.{ArrayBuffer, HashSet}
import java.io.FileWriter
import collection.mutable


class TreebankStatistics(labels: HashSet[String], unaries: HashSet[String], vpots: Array[HashSet[String]]) {

	def constituentLabels: Iterator[String] = {
		labels.iterator
	}
	
	def constituentLabelsOfSize(size: Int): Iterator[String] = {
		vpots(size).iterator
	}

  def tagset: Iterator[String] = {
    constituentLabelsOfSize(1)
  }

	def unaryLabels: Iterator[String] = {
		unaries.iterator
	}

  def bin(binSize: Int, numBins: Int) {
    var binCount = 1
    val minBinIdx = 2
    while (binCount <= numBins) {
      val lset = new HashSet[String]()
      for (i <- (((binCount-1) * binSize)+1+minBinIdx) to (binCount * binSize)+minBinIdx) {
        lset ++= constituentLabelsOfSize(i)
      }

      if (binCount == numBins) {
        for (i <- (((binCount-1) * binSize)+1+minBinIdx) until vpots.size) {
          vpots(i) = lset
        }
      }
      else {
        for (i <- (((binCount-1) * binSize)+1+minBinIdx) to (binCount * binSize)+minBinIdx) {
          vpots(i) = lset
        }
      }
      binCount += 1
    }
  }

  def writeToFile(filename: String) {
    val out = new FileWriter(filename)
    for (i <- 0 to 1000) {
      val lls = constituentLabelsOfSize(i).toArray
      out.write(i + "(%d): %s\n".format(lls.size, lls.mkString(" ")))
    }
    out.close
  }
}


object TreebankStatistics {
	
	def main(args: Array[String]) = {
		var options = new ArgParser(args)
		val treebankFile = options.getString("--treebank")
    val treeOptions = TreebankReaderOptions.fromCommandLine(new ArgParser(args))
		val stats = construct(TreebankReader.read(treebankFile, options), treeOptions)
	}

	def construct(trees: Iterator[Tree], options: TreebankReaderOptions): TreebankStatistics = {
    val maxSpan = 1000
		val vpots = Array.fill[HashSet[String]](maxSpan+1)(new HashSet[String])
		val labels  = new HashSet[String]
		val unaries = new HashSet[String]
    var tcount = 0
		for (tree <- trees) {
      tcount += 1
      val ctree = tree //.removeTop
			val btree = if (options.BINARIZE_MODE == "MARG") {
          tree.removeNones().removeUnaryChains()
        }
      else {
          tree.removeNones().removeUnaryChains().binarize(options.BINARIZE_MODE)
        }
//      println("ctree = " + ctree)
//      println("btree = " + btree)
//      println
 //     println(ctree.spans.mkString("\n"))
      ctree.toSpans.filter(!_.isUnary).foreach(labels += _.label)
      ctree.toSpans.filter(_.isUnary).foreach(unaries += _.label)

			for (bspan <- btree.toSpans) {
//        println("adding label " + bspan.label)
				labels += bspan.label
				vpots(bspan.width) += bspan.label
			}
      vpots(1) ++= ctree.tokens.map(_.pos)
		}

    val pruneMax = 50
    val sparserLabels = new ArrayBuffer[String]()
    for (i <- pruneMax until vpots.size) {
      sparserLabels ++= vpots(i)
    }

    for (j <- pruneMax until vpots.size) {
      vpots(j) ++= sparserLabels
    }

    for (i <- 2 until pruneMax) {
      for (j <- vpots.size-1 to i by -1) {
        vpots(i) ++= vpots(j)
      }
    }

//    for (i <- 0 until 60) {
//      println(i + ": (%d) ".format(vpots(i).size) + vpots(i).mkString(" "))
//    }

//    println((0 until 50).map(_.toString).toArray.mkString(", "))
//    println((0 until 50).map(vpots(_).size).toArray.mkString(", "))
//    println(tcount)
		new TreebankStatistics(labels, unaries, vpots)
	}	
}
