package narad.io.re

import scala.collection.JavaConversions._
import java.io.{File, FileInputStream, InputStream}
import cc.refectorie.proj.relation.protobuf.DocumentProtos
import collection.mutable.ArrayBuffer
import narad.nlp.ner.NamedEntity
import scala.collection.mutable.HashMap
import narad.nlp.trees.{DependencyTree, LabeledDependencyTree}
import narad.io.util.DirectoryReader

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 4/11/14
 * Time: 11:50 AM
 */
class ProtoRelationReader(filename: String) extends Iterable[ProtoDocument] {

  def getDocuments(f: String): Iterator[ProtoDocument] = {
    val in: InputStream = new FileInputStream(f)
    val docs = new ArrayBuffer[DocumentProtos.Document]()
    var msg : DocumentProtos.Document = DocumentProtos.Document.parseFrom(in)
    while (msg != null) {
      docs += msg
      try {
        msg = DocumentProtos.Document.parseFrom(in)
      }
      catch {
        case _ : Throwable => msg = null.asInstanceOf[DocumentProtos.Document]
      }
    }
    in.close()
    docs.map(parseDocument(_)).iterator
  }

  def parseDocument(doc: DocumentProtos.Document): ProtoDocument = {
    new ProtoDocument(doc.getSentencesList.map{ s =>
      val tokens = s.getTokensList.map(t => new ProtoToken(t.getWord, t.getTag, t.getNer)).toArray
      val tree = new LabeledDependencyTree(s.getDepTree.getHeadList.map(_.toInt).toArray, s.getDepTree.getRelTypeList.map(_.toString).toArray)
//      println("# mentions = " + s.get)
      val mentions = s.getMentionsList.map{m =>
        println(m)
        new ProtoEntityMention(m.getFrom, m.getTo, m.getEntityGuid, m.getLabel)}.toArray
      new ProtoSentence(tokens, mentions, tree)}.toArray)
  }

  def getRelations(f: String): Iterator[ProtoRelation] = {
    val in: InputStream = new FileInputStream(f)
    val rels = new ArrayBuffer[DocumentProtos.Relation]()
    var msg : DocumentProtos.Relation = DocumentProtos.Relation.parseDelimitedFrom(in)
    while (msg != null) {
      rels += msg
      try {
        msg = DocumentProtos.Relation.parseDelimitedFrom(in)
      }
      catch {
        case _ : Throwable => msg = null.asInstanceOf[DocumentProtos.Relation]
      }
    }
    in.close()
    rels.map(parseRelation(_)).iterator
  }

  def parseRelation(r: DocumentProtos.Relation): ProtoRelation = {
    new ProtoRelation(
      r.getSourceGuid,
      r.getDestGuid,
      r.getRelType,
      r.getMentionList.map{m =>
        new ProtoRelationMention(m.getFilename, m.getSourceId, m.getDestId, m.getSentence, m.getFeatureList.map(_.toString).toArray)
      }.toArray)
  }


  def iterator: Iterator[ProtoDocument] = {
    if (new File(filename).isDirectory) {
      val dr = new DirectoryReader
      dr.listAllFiles(new File(filename)).map(f => getDocuments(f.toString).toArray).flatten.iterator
    }
    else {
      getDocuments(filename)
    }
  }
}

object ProtoRelationReader {

  def main(args: Array[String]) {
    val reader = new ProtoRelationReader(args(0))
//    reader.getDocuments(args(0)).foreach { d =>
//      println(d)
//    }
    reader.getRelations(args(0)).foreach{ r =>
      println(r)
    }
  }
}


case class ProtoDocument(sentences: Array[ProtoSentence]) {
  override def toString = sentences.map(_.toString).mkString("\n\n")
}

case class ProtoToken(word: String, tag: String, ner: String) {}

case class ProtoSentence(tokens: Array[ProtoToken], mentions: Array[ProtoEntityMention], tree: LabeledDependencyTree) {
  override def toString = {
    val sb = new StringBuilder
    var count = 1
    tokens.zip(tree.heads).foreach { case(t, h) =>
      sb.append("%d\t%s\t%s\t%s\t%d\n".format(count, t.word, t.tag, t.ner, h))
      count += 1
    }
    sb.append("Mentions: " + mentions.mkString("\n"))
    sb.toString()
  }
}

class ProtoRelation(sourceID: String, destID: String, label: String, mentions: Array[ProtoRelationMention]) {

  override def toString = {
    "Source: %s\nDest:%s\nType:%s\nMentions:%s\n".format(sourceID, destID, label, mentions.map(_.toString).mkString("\n"))
  }
}

case class ProtoRelationMention(filename: String, from: Int, to: Int, sentence: String, feats: Array[String]) {
  override def toString = {
    "  File:%s\n  From <%d> to <%d>\n  Sentence: %s\n  Feats: %s".format(filename, from, to, sentence, feats.mkString(" "))
  }
}

case class ProtoEntityMention(from: Int, to: Int, guid: String, label: String) {}

case class ProtoEntity(guid: String, name: String, etype: String, pred: String) {
  override def toString = {
    "Guid:%s\nName:%s\nType:%s\nPred:%s\n".format(guid, name, etype, pred)
  }
}

















// y(label: String, sentID: Int, start: Int, end: Int, tokens: Array[String] = Array())
/*
  def getEntities(): HashMap[String, ProtoNamedEntity] = {
    val in: InputStream = new FileInputStream(filename)
    val emap = new HashMap[String, ProtoNamedEntity]()
    var msg : DocumentProtos.Entity = DocumentProtos.Entity.parseDelimitedFrom(in)
    while (msg != null) {
      emap(msg.getGuid) = new ProtoNamedEntity(msg.getGuid, msg.getName, msg.getType, msg.getPred)
      msg = DocumentProtos.Entity.parseDelimitedFrom(in)
    }
    in.close()
    emap
  }

  def getRelations(): Iterator[ProtoRelation] = {
    val entities = getEntities()
    val in: InputStream = new FileInputStream(filename)
    var msg : DocumentProtos.Relation = DocumentProtos.Relation.parseDelimitedFrom(in)
    while (msg != null) {
      val srcid = msg.getSourceGuid
      val destid = msg.getDestGuid
      val relType = msg.getRelType
      val mentions = msg.getMentionList
      for (m <- mentions) {
        println("MENTION")
        println("sentence: " + m.getSentence)
        println("feats: " + m.getFeatureList.mkString(" "))
      }
      println(srcid + "\t" + destid + "\tREL$" + relType  )      //to distinguish relation labels from other features
      println(srcid + "\t" + destid) //todo: get entity name
      println()


      for (index <- 0 until msg.getMentionCount) {
        val mentionMsg = msg.getMention(index)
        //        for(index <- 0 until mentionMsg.getFeatureCount) print("\t" + mentionMsg.getFeature(index))
        val sentence = mentionMsg.getSentence
        println(sentence)
      }
      msg = DocumentProtos.Relation.parseDelimitedFrom(in)
    }
    //    if (msg == null) return null
    println
    in.close()
    null.asInstanceOf[Iterator[ProtoRelation]]
  }
*/
