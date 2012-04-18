package narad.projects.cparser
import collection.mutable.ArrayBuffer
import narad.nlp.parse.{Tree, TreeReader}
import narad.util.ChunkReader
import java.io._

object Evaluate {
	val spanPattern1 = """unaryLabel([^\(]+)\(([0-9]+),([0-9]+)\)\t\+.*""".r
	val spanPattern2 = """[0-9]+\t([0-9]+)\t([0-9]+)\t(.*)""".r
	
	case class Span(left: Int, right: Int, label: String) {}

	//		val chunks2 = new ArrayBuffer[String]
	//		for (chunk <- ChunkReader.read(options.getString("-goldFile"))) { chunks1 += chunk }
	//		println("# of gold chunks = %d".format(chunks1.size))

	//		val chunks1 = ChunkReader.slurp(options.getString("-goldFile"))
	//		ChunkReader.reset
	//		val chunks2 = ChunkReader.slurp(options.getString("-testFile"))




	def main(args: Array[String]) {
		val options = new narad.util.ArgParser(args)
		println("Reading files...")
		val chunks1 = new ArrayBuffer[String]
		val chunks2 = new ArrayBuffer[String] 
		for (chunk <- ChunkReader.read(options.getString("-testFile"))) { chunks1 += chunk }
		println("# of test chunks = %d".format(chunks1.size))		
		ChunkReader.reset
		for (chunk <- ChunkReader.read(options.getString("-goldFile"))) { chunks2 += chunk }
		println("# of gold chunks = %d".format(chunks2.size))		

		var rdenom = 0
		var pdenom = 0
		var correct = 0
		val fw = new FileWriter("unary.reout")
		for (i <- 0 until chunks1.size) {
			val spans1 = parseSpans(chunks1(i))
			val spans2 = parseSpans(chunks2(i))
			rdenom += spans1.size
			pdenom += spans2.size
			for (span <- spans1) {
				if (spans2.contains(span))
					correct += 1
			}
			println(i)
		}
		fw.close()
		
		val prec = correct * 1.0 / pdenom
		val rec  = correct * 1.0 / rdenom
		val fmeasure = 2 * ((prec * rec) / (prec + rec))
		println("Count = %d".format(correct))
		println("Precision: %f".format(prec))
		println("Recall: %f".format(rec))
		println("F-Measure: %f".format(fmeasure))
	}
	
	def parseSpans(chunk: String): Array[Span] = {
		val buffer = new ArrayBuffer[Span]
		for (line <- chunk.split("\n")){
			line match {
				case spanPattern1(label, left, right) => buffer += new Span(left.toInt, right.toInt, label)
				case spanPattern2(left, right, label) => buffer += new Span(left.toInt, right.toInt, label)
				case default => None
			}
		}
		buffer.toArray
	}	
		
/*		
		val length = goldTrees.size
		var correctUnaries = 0.0
		var testUnaries = 0
		var goldUnaries = 0
		var totalLeafUnaries = 0
		var totalTopUnaries  = 0
		for (i <- 0 until length) {
			val goldTree = goldTrees(i)
			goldTree.annotateWithIndices(0)
			val testTree = testTrees(i)
			testTree.annotateWithIndices(0)
			val goldSpans = goldTree.flatten.filter(_.isUnaryRewrite).map(_.toSpan)
			val testSpans = testTree.flatten.filter(_.isUnaryRewrite).map(_.toSpan)
			println(goldSpans.mkString("\n"))
			println("----------------------")
			println(testSpans.mkString("\n"))
			var local = 0
			for (span <- goldSpans) {
				if (testSpans.filter(s => s.start == span.start && s.end == span.end && s.label == span.label).size > 0) {
					correctUnaries += 1
					local += 1
				}
				if (span.label == "TOP") {
					totalTopUnaries += 1
				}
				testUnaries += testSpans.size
				goldUnaries += goldSpans.size
			}
			println(local)
			println
		}
		val urecall = correctUnaries / goldUnaries
		val uprecision = correctUnaries / testUnaries
		val ufmeasure = 2 * ((uprecision * urecall) / (uprecision + urecall))
		println("Unary Recall      =  %f".format(urecall))
		println("Unary Precision   =  %f".format(uprecision))
		println("Unary FMeasure    =  %f".format(ufmeasure))
	}
*/
	
	def countCorrect(gold: Tree, test: Tree): Int = {
		val goldSpans = gold.flatten
		val testSpans = test.flatten
		var count = 0
		for (gs <- goldSpans) {
			for (ts <- testSpans) {
				if (gs.start == ts.start && gs.end == gs.end && gs.label == ts.label) {
					count += 1
				}
			}
		}
		return count		
	}	
}