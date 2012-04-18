package narad.projects.relmarg
import java.io._
import scala.collection.mutable.ArrayBuffer
import narad.util.{ArgParser, ChunkReader}


object SRLReader {

	def iterator(options: ArgParser): Iterator[SRLDatum] = {
		val filename  = options.getString("--srl.file")
		val format    = options.getString("--format", "CoNLL09")
		assert(format == "CoNLL09" || format == "CoNLL08", "Invalid SRL format: " + format)
		for (chunk <- ChunkReader.read(filename)) yield {
			try {
		 		SRLDatum.constructFromCoNLL(chunk.split("\n"), format)
			}
			catch {
				case e: Exception => {
					System.err.println("Error trying to create datum from string:\n%s\n".format(chunk))
					System.err.println("\n" + e.getStackTrace + "\n")
					System.exit(1)
			 		null.asInstanceOf[SRLDatum]
				}
			}
		}
	}

	def read(options: ArgParser): Array[SRLDatum] = {
		val filename  = options.getString("--srl.file")
		val format    = options.getString("--format", "CoNLL09")
		val filter    = options.getBoolean("--filter.examples", false)
		val print 		= options.getBoolean("--print", false)
		assert(format == "CoNLL09" || format == "CoNLL08", "Invalid SRL format: " + format)
		val data = new ArrayBuffer[SRLDatum]
		var fcount = 0
		for (chunk <- ChunkReader.read(filename)) {
			val datum = SRLDatum.constructFromCoNLL(chunk.split("\n"), format)
			if (!filter || datum.containsPredicates) {
				data += datum
				if (print) {
					println(datum)
					println
				}
			}
			else {
				fcount += 1
			}
		}
		System.err.println("Filtered out %d of %d examples.".format(fcount, fcount + data.size))
		return data.toArray
	}
	
	def main(args: Array[String]) = {
		val datums = read(new ArgParser(args))
	}
}















/*
val args  = io.Source.fromFile(labelFile).getLines.toArray
val preds = io.Source.fromFile(tagFile).getLines.toArray

var count = 0
for (chunk <- ChunkReader.read(filename)) {
if (count % 10 == 0) System.err.println("Extracting features for sentence %d".format(count))
featurize(SRLDatum.constructFromCoNLL(chunk.split("\n"), format), options)
count += 1
}
}
}

def main(args: Array[String]) = {
val options = new narad.util.ArgParser(args)
val data = 
}
*/










/*
val oargPattern = """A([0-9]+)""".r
val nargPattern = """AM\\-([A-Z]+)""".r

val oargPattern2 = "(A[0-9]+)"
val nargPattern2 = "(AM\\-[A-Z]+)"

val puncPattern = "[]"
val numPattern = "[0-9]"




def findMaxChild(filename: String): Int = {
var max = 0
for (chunk <- ChunkReader.read(filename)) {
oargPattern findAllIn chunk foreach (_ match {
case oargPattern(pos) => {
val ipos = pos.toInt
if (ipos > max) max = ipos
}		
})
}
return max+1
}

*/

/*
val feats	    = options.getString("--feats", "srl")
val labelHidden = options.getBoolean("--label.hidden", false)
val prune = options.getBoolean("--prune", false)
val mode   = options.getString("--mode", "train")
val format = options.getString("--format", "CoNLL09")

*/

//				feats.contains("r"), feats.contains("s"), feats.contains("c"), 
//									labelHidden, prune, bigram, format, skipSyntax, mode=fmode)

/*
val labels = if (mode == "train")
findLabels(filename, format=format)
else
io.Source.fromFile(labelFile).getLines.toArray

val predtags = if (mode == "train")
findPredTags(filename)
else
io.Source.fromFile(tagFile).getLines.toArray

*/


/*
case class Role(idx: Int, predicate: String, oargs: Array[String], nargs: Array[String]) {}

case class SRLToken(word: String, tag: String, predicate: Int, var args: Array[Int] = Array[Int]()) {
def isPredicate: Boolean = predicate >= 0
}
*/

/*
// As in catalan: arg2-atr
def findLabels(filename: String, poffset: Int = 13, threshold: Int = 0): Array[String] = {
val labels = new ArrayBuffer[String]
for (line <- io.Source.fromFile(filename).getLines() if line.contains("\t")) {
val cells = line.split("\t")
for (i <- poffset+1 until cells.size) {
//			if (cells(i).matches(oargPattern2) || cells(i).matches(nargPattern2)) {
if (cells(i) != "_") labels += cells(i)
//			}
}
}
val ltokens = labels.toArray
val ltypes  = ltokens.distinct
System.err.println("Arg labels (%d): %s".format(ltypes.size, ltypes.mkString(", ")))
return ltypes

//	val nlabs = ltypes.filter(_.matches("A[0-9]+"))
//	println("nlabs = " + nlabs.mkString(", "))
//	val bigram = ltypes.filter(_.matches("A[0-9]+")).map(_.substring(1).toInt).max     // NEED TO CATCH THIS

//	println("bigram max = " + bigram)
//	val l = ((0 to bigram).map("A%d".format(_)) ++ ltypes.filter(!_.matches("A[0-9]+"))).toArray
//	println("l = " + l.mkString(", "))
//	return l
}
*/
//	if (threshold == 0) {
	//		return ltypes
	//	}
	//	else {
		//		return ltypes.filter(l => ltokens.filter(_ == l).size > threshold)		
		//	}


/*
def featurize(chunk: String, labels: Array[String]) = {
val lines = chunk.split("\n")
val slen = lines.size

//		val preds  = new ArrayBuffer[Int]
var pcount = 0
val args   = Array.ofDim[Int](slen, maxChildren)
val tokens = Array.ofDim[SRLToken](slen)
for (i <- 0 until slen) {
val cols = lines(i).split("\t")
val pidx = if (cols(10) != "_"){ pcount += 1; pcount-1 } else { -1 }
tokens(i) = SRLToken(cols(1), cols(3), pidx)
//			if (cols(10) != "_") 
for (cidx <- 11 until cols.size) {
cols(cidx) match {
case oargPattern(pos) => args(cidx-11)(pos.toInt) = i
case _ => null
}
}			
}

pcount = 0
for (j <- 0 until tokens.size) {
if (tokens(j).isPredicate) {
tokens(j).args = args(pcount)
pcount += 1
}
}

val ttokens = tokens.map(t => new Token(t.word, t.pos))
println("@slen\t%d".format(tokens.size))
println("@words\t%s".format(tokens.map(_.word).mkString(" ")))
for (i <- 0 until slen) {
val token = tokens(i)
val plabel = if (token.isPredicate) "+" else ""
println("pred(%d)\t%s%s".format(i, plabel, predicateFeatures(i, ttokens).mkString(" ")))			
for (j <- 0 until maxChildren) {
for (k <- 0 until slen) {
val alabel = if (token.isPredicate && token.args(j) == k) "+" else ""
//					println("arg(%d,%d,%d)\t%s%s".format(i, j, k, alabel, argumentFeatures(i, k, tokens).mkString(" ")))
}
}
}
println
}
*/
/*		
val roles = Array.ofDim[Int](lines.size, 5)
for (idx <- 0 until lines.size) {
val line = lines(idx)
val cols = line.split("\t")
//			if (cols(10) != "_") roles(idx)(0) = cols(10)		
for (cidx <- 11 until cols.size) {
cols(cidx) match {
//					case oargPattern(pos) => roles(cidx-11)(pos.toInt+1) = idx
case _ => null
}
}
}

for (idx <- 0 until lines.size) {
val line = lines(idx)
val cols = line.split("\t")
//				if (roles(idx)(0)
}

}
*/

/*	
def collectRoles(lines: Array[String]): Array[Role] = {
val rarray = Array.ofDim[String](lines.size, 5)
for (idx <- 0 until lines.size) {
val line = lines(idx)
val cols = line.split("\t")
if (cols(10) != "_") rarray(idx)(0) = cols(10)		
for (cidx <- 11 until cols.size) {
cols(cidx) match {
case oargPattern(pos) => rarray(cidx-11)(pos.toInt+1) = idx
case _ => null
}
}
}
return rarray.zipWithIndex.filter(_._1(0) != "_").map {
case(x,i) => new Role(i, x(0), x.slice(1, x.size))
}
}
*/
/*		
val preds = new ArrayBuffer[String]
val oargs = new HashMap[Int,  // new ArrayBuffer[Array[String]]
val nargs = new HashMap[Int, Hash]//new ArrayBuffer[String]
for (line <- chunk.split("\n")) {
val cols = line.split("\t")
if (cells(10) != "_") preds += cells(10)		
for (cidx <- 11 until cols.size) {
cols(cidx) match {
case oargPattern(pos) => oargs(cidx-11) += 
case nargPattern(type) => 
}
}
}		
}
*/
/*	
def featurize(chunk: String) = {
val preds = new ArrayBuffer[String]
val roles = Array.fill(15)(new HashMap[String, Int])
for (line <- chunk.split("\n")) {
val cells = line.split("\t")
val idx = cells(0).toInt
if (cells(10) != "_") preds += cells(10)
for (i <- 11 until cells.size) {
if (cells(i) != "_") roles(i-11).put(cells(i), idx) 
}
}
for (i <- 0 until preds.size) {
println("%s(%s)".format(preds(i), roles(i).mkString(", ")))
}
}
*/




/*

class Relation(id: String, name: String, subj: String, obj: String) {
override def toString: String = "Relation[%s]: %s(%s, %s)".format(id, name, subj, obj)
}


object FreebaseReader {
val comics = "/Users/narad/Documents/corpora/freebase/data/comic_books/comic_book_character.tsv"

def read(directory: File): Array[Relation] = {
val relations = new ArrayBuffer[Relation]
for (file <- directory.listFiles) {
if (file.isDirectory) {
relations ++= read(file)
}
else if (file.getName.contains(".tsv") && file.getName.contains("sports")) {
relations ++= readRelationFile(file)
}
else if (file.getName.endsWith(".xml")) {
for (sent <- readNYT(file)) {
println(toRelFormat(sent))
println
}
}
}
relations.foreach(println(_))
relations.toArray
}

def readRelationFile(file: File): Array[Relation] = {
println("...processing file %s...".format(file))
val relations = new ArrayBuffer[Relation]
val reader = new BufferedInputStream(new FileInputStream(file))
var lines = io.Source.fromInputStream(reader).getLines.toArray
val header = lines.head.split("\t")
for (line <- lines.tail) {
val args = specialSplit(line, "\t")
//			This assertion should probably be on, but the error is so rare and would otherwise require a look-ahead style reader.
//			assert(args.size == header.size, "Error in special split?  Sizes should be equivalent: \n%s\n\n%s.".format(header.mkString("\n"), args.mkString("\n")))
if (args.size == header.size) {
for (i <- 2 until header.size if args(i) != "") {
for (arg <- args(i).split(",")) {
relations += new Relation(args(1), header(i), args(0), arg)
}
}				
}
}
relations.toArray			
}

// The normal split does not return a null element between two delimeters,
// throwing off the alignment between argument slots and relation slots
def specialSplit(str: String, delim: String): Array[String] = {
val buffer = new ArrayBuffer[String]
var last = 0
for (i <- 0 to str.size) {
if (i == str.size || str(i) == '\t') {
if (last == i) {
buffer += ""
}
else {
buffer += str.slice(last, i).replaceAll("\t", "")
}
last = i
}
}
return buffer.toArray
}

def readNYT(file: File):Array[String] = {
println("...processing xml file %s...".format(file))
val snippets = new ArrayBuffer[String]
val xml = scala.xml.XML.load(scala.xml.Source.fromFile(file))
val testResults = xml \\ "_" foreach { node =>
if (node.attributes.exists(_.value.toString == "full_text")) {			
for (p <- node \\ "_" if p.label == "p") {
snippets += p.text
}
}
}
return snippets.toArray
}

def main(args: Array[String]) {
val rels = read(new File(args(0)))
println(rels.size)
}

def toRelFormat(str: String): String = {
val tokens = new ArrayBuffer[String]
str.split(" ").zipWithIndex.foreach { case(t,i) =>
tokens += "%d\t%s".format(i,t)
}
return tokens.mkString("\n")
}
}















//			x.attributex => println(x.label) } // { x => x.label == "full_text" }
//		for (ts <- testResults) {
//			println(ts.toString.replaceAll("\n", ""))
//		}

//import org.apache.tools.bzip2.CBZip2InputStream
//import org.apache.tools.tar.TarEntry



//import com.gc.iotools.fmt._
//import com.gc.iotools.fmt.decoders._

//class Relation(name: Sting, id: String, )


//		for (file <- new File(directory).listFiles; if (file.isFile)) {
//			readFile(file)
//		}
//		readFile("/Users/narad/Documents/corpora/freebase/freebase-datadump-tsv.tar.bz2")


/*	
def readFile(file: String) = {
println("Reading file %s...".format(file))
val reader = new CBZip2InputStream(new BufferedInputStream(new FileInputStream(file)))
//		val reader = GuessInputStream.getInstance(new FileInputStream(file))
var lines = io.Source.fromInputStream(reader).getLines
for (line <- lines) {
println(line)
}
*/		


//		while({line=reader.readline()};line!=null) {
//			println(line)
//		}

/*	
public InputStream decode(final InputStream istream) throws IOException {
final InputStream decoded = new CBZip2InputStream(istream);
return decoded;
}
*/	

//		val files = recursiveListFiles(File.open(directory))
//		files.foreach(println(_))
/*
def recursiveListFiles(f: File): Array[File] = {
val these = f.listFiles
these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
}	

object FreebaseReader {

}
*/


*/






/*
val slen = tokens.size
val pidx = if ((idx - window) < 0) Math.abs(idx-window) else 0
val aidx = if ((idx + window) >= slen) Math.abs((idx + window) - slen + 1) else 0
val oidx = idx + pidx
val mtokens = pad(tokens, pidx, aidx)
val feats = new ArrayBuffer[String]
val token = mtokens(oidx)

feats += "pred"
//		feats += "word-%s".format(mtokens(oidx).word)
feats += "tag-%s".format(mtokens(oidx).pos)
feats += "word-tag-%s-%s".format(mtokens(oidx).word, mtokens(idx).pos)
feats += "caps-%s".format(mtokens(oidx))
for (i <- idx-window to idx+window if i != idx) {
val offset = i - idx
val ii = i + pidx
//			feats += "NW(%d,%d)-%s".format(offset, window, mtokens(ii).word)
//			feats += "NT(%d,%d)-%s".format(offset, window, mtokens(ii).pos)
//			feats += "NC(%d,%d)-%s".format(offset, window, mtokens(ii).word + mtokens(ii).pos)
}
feats.toArray
}
*/
