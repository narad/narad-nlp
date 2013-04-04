package narad.nlp.parser.constituent
import narad.io.tree.TreebankReader
import narad.nlp.trees.{ConstituentTree => Tree}
import narad.util.ArgParser
import scala.collection.mutable.HashSet
import java.io.FileWriter
import collection.mutable


class TreebankStatistics(labels: HashSet[String], unaries: HashSet[String], vpots: Array[HashSet[String]]) {
	def bins = new Array[Array[String]](1000)
  var binSize = 0

	def constituentLabels: Iterator[String] = {
		labels.iterator
	}
	
	def constituentLabelsOfSize(size: Int): Iterator[String] = {
		vpots(size).iterator
	}

  def binnedConstituentLabelsOfSize(size: Int): Iterator[String] = {
    vpots(size).iterator
  }

	def unaryLabels: Iterator[String] = {
		unaries.iterator
	}

  def createBins(binSize: Int, numBins: Int) {
    this.binSize = binSize
    var binCount = 1
    while (binCount <= numBins) {
      val lset = new HashSet[String]()
      for (i <- ((binCount-1 * binSize)+1) to (binCount * binSize)) {
        lset ++= constituentLabelsOfSize(i)
      }
      bins(binCount) = lset.toArray
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
		val stats = construct(TreebankReader.read(treebankFile, options))
	}

	def construct(trees: Iterator[Tree]): TreebankStatistics = {
    val maxSpan = 1000
		val vpots = Array.fill[HashSet[String]](maxSpan+1)(new HashSet[String])
		val labels  = new HashSet[String]
		val unaries = new HashSet[String]
		for (ctree <- trees) {
			val btree = ctree.binarize.removeUnaryChains
			ctree.annotateWithIndices()
			btree.annotateWithIndices()
      ctree.spans.filter(!_.isUnary).foreach(labels += _.label)
      ctree.spans.filter(_.isUnary).foreach(unaries += _.label)

			for (bspan <- btree.spans) {
				labels += bspan.label
				vpots(bspan.width) += bspan.label
			}
		}
		new TreebankStatistics(labels, unaries, vpots)
	}	
}
