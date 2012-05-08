package narad.nlp.re
import narad.nlp.trees.{Tree, TreebankReader}
import scala.collection.mutable.ArrayBuffer

case class RelationToken(word: String, pos: String, elabel: String, esublabel: String, mlabel: String)

case class DatumRelation(label: String, sublabel: String, arg1: Int, arg2: Int) {}

case class DatumEntity(text: String, index: Int, label: String, sublabel: String, mlabel: String) {}

class REDatum(slen: Int) {
	var words 	= Array.fill[String](slen)("POS")
	var tags 	= Array.fill[String](slen)("POS")
	var relations = new Array[DatumRelation](slen)
	var entities = new Array[DatumEntity](slen)
	var parse = new Array[Int](slen+1)
	var tree = null.asInstanceOf[Tree]

	def entityIndices = entities.map(_.index)
	
	def getEntity(i: Int): DatumEntity = {
		for (e <- entities) if (e.index == i) return e
		return null.asInstanceOf[DatumEntity]
	}
	
	def getLabel(i: Int, j: Int): String = {
		for (rel <- relations) {
			if (rel.arg1 == i && rel.arg2 == j) return rel.label
		}
		return ""
	}

	def hasEntity(i: Int): Boolean = {
		for (entity <- entities) if (entity.index == i) return true
		return false
	}

	def hasRelation(i: Int, j: Int): Boolean = {
		for (rel <- relations) {
			if (rel.arg1 == i && rel.arg2 == j) return true
		}
		return false
	}

	def size = words.size

	def tokens: Array[RelationToken] = {
		println(entities.size)
		println(words.size)
		(0 until words.size).toArray.map { i =>
			if (!hasEntity(i)) {
				new RelationToken(words(i), tags(i), "NA", "NA", "NA")
			}
			else {
				val entity = getEntity(i)
				new RelationToken(words(i), tags(i), entity.label, entity.sublabel, entity.mlabel)				
			}
		}
	}

//	def tokens: Array[SRLToken] = (0 until words.size).toArray.map(i => new SRLToken(words(i), words(i), tags(i), tags(i)))

	override def toString = words.mkString(" ")
}

// col index 2 - tagger pos
// col index 3 - parser pos
object REDatum {

	def construct(lines: Array[String]): REDatum = {
		val nrComments = lines.filter(_.startsWith("@")).size
		val slen = lines.size - nrComments
		val words = new Array[String](slen)
		val tags = new Array[String](slen)
		val heads = new Array[Int](slen+1)
		val entities = new ArrayBuffer[DatumEntity]
		val relations = new ArrayBuffer[DatumRelation]
// 		13      two_parties     NNS     NNS     12      Y       NA      NA      NA      11      GEN-AFF Org-Location    ARG-1
		println(slen)
		val datum = new REDatum(slen)
		for (line <- lines) {
			if (line.startsWith("@parse")) {
				datum.tree = TreebankReader.parseExpression(line.substring(7))
			}
			else if (line.startsWith("@deps")) {
				// do nothing
			}
			else {
				val cols = line.split("\t")
				val id = cols(0).toInt
				val word = cols(1)
				words(id-1) = word
				val tag = cols(3)
				tags(id-1) = tag
				println("converting " + cols(4))
				heads(id) = cols(4).toInt	

				val isEntity = cols(5) == "Y"
				if (isEntity) {
					entities += new DatumEntity(word, id, cols(6), cols(7), cols(8))
				}
				if (cols(10) != "_") {
					val rel = cols(9).toInt
					relations += new DatumRelation(cols(10), cols(11), id, rel)
				}				
			}
		}
		datum.words = words
		datum.tags = tags
		datum.entities = entities.toArray
		datum.relations = relations.toArray
		datum.parse = heads
		return datum
	}
}







//				relations(id)(rel) = Tuple(cols(7), cols(8))
//				relations(rel)(id) = Tuple(cols(7), cols(8))


//Array.ofDim[(String, String)](slen, slen)

/*
def hasRelation(a1: Int, a2: Int, label: String = "_") = label match {
case "_" => rchart(a1)(a2) != null
case _ =>   rchart(a1)(a2) != null && rchart(a1)(a2)._1 == label
}



*/
//	var rchart = new Array[Array[(String, String)]](slen, slen)

/*
def relations: Array[Relation] = {
val rbuff = new ArrayBuffer[Relation]
for (i <- 0 until rchart.size; j <- 0 until rchart(i).size) {
if (rchart(i)(j) != null) rbuff += rchart(i)(j)
}
return rbuff.toArray
}


def entityIndices: Array[Int] = {
return entities.zipWithIndex.filter{case(e,i) => e != null}.map{case(e,i) => i+1}
}

*/


//		datum.rchart = relations
/*
val dep = if (cols(3) != "_") cols(3).toInt-1 else -1
if (cols(5) == "Y" && dep >= 0) {
val anchor = id-1
val arg1 = new EntityMention(cols(1)) //cols(1).split("-"))
entities(id-1) = arg1
if (cols(6) == "ARG-1") {
val dep = cols(3).toInt-1
val arg2 = new EntityMention(lines(dep)) //lines(dep).split("\t")(1).split("-"))
val label = cols(5)
relations(anchor)(dep) = new Relation(label, arg1=arg1, arg2=arg2)
}
}
*/
