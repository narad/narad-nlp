package narad.projects.relmarg
import java.io.FileWriter
import narad.nlp.relations._
import narad.util.{ArgParser, ChunkReader}
import scala.collection.mutable.HashSet

object REFeaturizer {
	val startToken = RelationToken("START", "START_POS", "SNA", "SNA", "SNA")
	val endToken   = RelationToken("END", "END_POS", "ENA", "ENA", "ENA")

	def extractLabels(filename: String): Array[String] = {
		val labels = new HashSet[String]
		for (datum <- REReader.iterator(filename)) {
			val entities = datum.entities
			for (a1 <- entities; a2 <- entities if (a1.index < a2.index)) {
				labels += datum.getLabel(a1.index, a2.index)
			}
		}
		labels.toArray.filter(_ != "")
	}

	def main(args: Array[String]) = {
		val options = new ArgParser(args)
		val constituencySyntax = false
		val dependencySyntax = true

		val labelHidden = true

		val filename = options.getString("--relation.file")
		//		val mode = options.getString("--mode", "baseline")
		//		assert(mode == "baseline" || mode == "observed" || mode == "hidden", "Mode not set properly: %s".format(mode))

		val feats  = options.getString("--mode", "r1s1c1")
		val skipSyntax  = options.getBoolean("--skip.syntax", false)

		val labelFile = options.getString("--label.file")
		val labels = if (labelFile != null) {
			io.Source.fromFile(labelFile).getLines.toArray
		}
		else {
			val ls = extractLabels(filename)
			val out = new FileWriter("re.labels")
			for (l <- ls) out.write(l + "\n")
			out.close
			ls
		}

		val lfeats = feats.toLowerCase
		val rel     = feats.contains("r") || feats.contains("R")
		val csyntax  = feats.contains("s") || feats.contains("S")
		val dsyntax  = feats.contains("d") || feats.contains("D")		
		val labelRel = feats.contains("R")
		val labelCSyntax = feats.contains("S")
		val labelDSyntax = feats.contains("D")
		val relmode     = if (rel)     lfeats.substring(lfeats.indexOf("r")+1, lfeats.indexOf("r")+2).toInt else 1
		//		val syntaxmode  = if (syntax)  lfeats.substring(lfeats.indexOf("s")+1, lfeats.indexOf("s")+2).toInt else 1
		//		val connectmode = if (connect) lfeats.substring(lfeats.indexOf("c")+1, lfeats.indexOf("c")+2).toInt else 1

		var ex = 0
		for (datum <- REReader.iterator(filename)) {
			if (datum != null) {
				ex += 1
				if (ex % 10 == 0) System.err.println("..ex %d".format(ex))
				val slen = datum.size
				val tokens = Array(startToken) ++ datum.tokens ++ Array(endToken)
				println("@slen\t%d".format(slen))
				println("@words\t%s".format(datum.toString))
				println("@entities\t%s".format(datum.entities.map(_.index).mkString(" ")))
				println("@labels\t%s".format(labels.mkString(" ")))
				val entities = datum.entities
				for (a1 <- entities; a2 <- entities if (a1.index < a2.index)) {
					val i = a1.index
					val j = a2.index
					val feats = REFeatures.relationFeatures(tokens, i, j)
					val realLabel = datum.getLabel(i, j)
					val hasRel = if (realLabel != "" && labelRel) "+" else ""
					println("rel(%d,%d)\t%s%s".format(i, j, hasRel, feats.mkString(" ")))
					for (label <- labels) {
						val hasLabel = if (label == realLabel && labelRel) "+" else ""
						println("label(%d,%d,%d)\t%s%s".format(i, j, labels.indexOf(label), hasLabel, feats.map("%s-%s".format(label, _)).mkString(" ")))
					}				
				}

				if (dsyntax) {
					val heads = datum.parse
					for (i <- 0 to slen; j <- 1 to slen if i != j) {
						val hasArc = if (heads(j) == i && labelDSyntax) "+" else ""
						val feats = if (!skipSyntax) REFeatures.dependencyFeatures(tokens, i, j) else Array("X_DUMMY")
						println("un(%d,%d)\t%s%s".format(i, j, hasArc, feats.mkString(" ")))
					}
					for (a1 <- entities; a2 <- entities if (a1.index < a2.index)) {
						val i = a1.index
						val j = a2.index
						for (head <- 0 to slen) {
							if (head != i && head != j) {
								val hasLink = if (heads(i) == head && 
								heads(j) == head && 
								datum.hasRelation(i,j) && 
								labelDSyntax) "+" else ""
								val feats = REFeatures.dconnect(tokens, head, i, j)
								println("sslink(%d,%d,%d)\t%s%s".format(head, i, j, hasLink, feats.mkString(" ")))							
							}
						}
					}				
				}

				if (csyntax) {
					val tree = datum.tree
					tree.annotateWithIndices(0)
					for ( width <- 2 to slen; start <- 0 to (slen - width)) {
						val end = start + width
						val feats = REFeatures.constituencyFeatures(tokens, start, end).mkString(" ")
						val sfeats = REFeatures.cconnect(tokens, start, end).mkString
						val hasSpan = tree.containsSpan(start, end)
						if (hasSpan && labelCSyntax) {
							println("brack(%d,%d)\t+%s".format(start, end, feats))
							val hasLink = if (datum.hasRelation(start, end)) "+" else ""
							println("sslink(%d,%d)\t%s%s".format(start, end, hasLink, sfeats))														
						}
						else {
							println("brack(%d,%d)\t%s".format(start, end, feats))
							println("sslink(%d,%d)\t%s".format(start, end, sfeats))														
						}
					}
				}
			}
			println
		}
	}
}




















/*

println("@reltypes\t%s".format(reltypes.mkString(" ")))			
for (i <- entities; j <- entities if i != j) {
val label = if (datum.hasRelation(i, j)) "+" else ""
val feats = relationFeatures(datum, options)	
println("rel(%d,%d)\t%s%s".format(i, j, label, feats.mkString(" ")))
var found = false
var rcount = 0
for (reltype <- reltypes) {
val rlabel = if (datum.hasRelation(i, j, reltype)) "+" else ""
println("rel(%d,%d,%d)\t%s%s".format(i, j, rcount, rlabel, feats.map("R-%d_%s".format(rcount,_)).mkString(" ")))				
if (rlabel == "+") found = true
rcount += 1
}
val dummy = if (!found) "+" else ""
println("rel(%d,%d,%d)\t%sDUMMY".format(i, j, reltypes.size, dummy))								
}

// Dependency parsing features
if (dependencySyntax) {
val heads = datum.parse
for (i <- 0 to slen; j <- 1 to slen if i != j) {
val label = if (heads(j) == i && labelHidden) "+" else ""
val feats = Array("SFEAT") // SRLFeatures.morphDependency(tokens, i, j).mkString(" ")
println("un(%d,%d)\t%s%s".format(i, j, label, feats.mkString(" ")))					
}
}						
ex += 1
println
}
}
}
*/


















//				println("Parsing: " + sentence)
//				val parse = parser.dependencyParse(sentence)
//			parse match {
/*
case Some(heads) => {
println("Parse:")
//println(
heads.zipWithIndex.foreach {case(e,i) => println("[%d]=%d; %s".format(i,e,w(i)))}
//.mkString("\n"))
val length = slen
*/












/*
// Dependency parsing features
if (dependencySyntax) {
val parse = parses(i)
println(parse)
val heads = Array(-1) ++ parse.split("\n").map(_.split("\t")(6).toInt)	
println(heads.mkString(" "))			
//				val heads = Array(-1) ++ datum.heads
for (i <- 0 to slen; j <- 1 to slen if i != j) {
val label = if (heads(j) == i && labelHidden) "+" else ""
val feats = SRLFeatures.morphDependency(tokens, i, j).mkString(" ")
println("un(%d,%d)\t%s%s".format(i, j, label, feats.mkString(" ")))					
}
}


// Constituency parsing features
if (constituencySyntax) {
val sentence = datum.toString
println("Parsing: " + sentence)
val parse = parser.parse(sentence)
parse match {
case Some(tree) => {
tree.annotateWithIndices(0)
println(tree)
val length = slen
for ( width <- 2 to length; start <- 0 to (length - width)) {
val end = start + width
val features = Array("blah1", "blah2")
//			val features = FeatureFactory.syntaxFeatures(tree, start, end)
val labelSet = tree.labels(start, end)
println("brack(%d,%d)\t%s%s".format(start, end, if (labelSet.size > 0) "+" else "", features.mkString(" ")))	
}					
}
case _=> System.err.println("Error parsing: " + sentence)
}				
}

*/