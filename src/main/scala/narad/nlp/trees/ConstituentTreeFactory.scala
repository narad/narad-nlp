package narad.nlp.trees

import collection.mutable.ArrayBuffer

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 11/10/13
 * Time: 4:51 PM
 */
object ConstituentTreeFactory {

  def buildTree(label: String = "SPAN", word: String = "", children: List[ConstituentTree] = List()): ConstituentTree = {
    if (word == "") {
      new ConstituentTree(new NonterminalNode(label), children)
    }
    else {
      new ConstituentTree(new PreterminalNode(label, word), children)
    }
  }

  def constructFromSpans(spans: Array[Span], slen: Int, words: Array[String] = Array(), tags: Array[String] = Array()): ConstituentTree = {
    return ConstituentTreeFactory.buildTree(label="TOP", children=findChildren(spans.sortBy(sp => (sp.width * 1000) + sp.height).reverse, 0, slen, words, tags))
  }

  def findChildren(spans: Array[Span], start: Int, end: Int, words: Array[String] = Array(), tags: Array[String] = Array()): List[ConstituentTree] = {
    val children = new ArrayBuffer[ConstituentTree]
    val max = findMaxSpan(spans, start, end)
    max match {
      case None => {
        if (words.isEmpty || tags.isEmpty) {
          List.fill(end-start)(ConstituentTreeFactory.buildTree(label="TAG", word="TMP", children=List()))
        }
        else {
          (start until end).map { i => ConstituentTreeFactory.buildTree(label=tags(i), word=words(i), children=List()) }.toList
        }
      }
      case Some(maxspan) => {
        if (maxspan.start > start) {
          val leftChildren = findChildren(spans, start, maxspan.start, words, tags)
          if (leftChildren.size > 0) children ++= leftChildren
        }
        val cchildren = findChildren(spans.filter(_ != maxspan), maxspan.start, maxspan.end, words, tags)
        children += ConstituentTreeFactory.buildTree(label=maxspan.label, children=cchildren)
        if (maxspan.end < end) {
          val rightChildren = findChildren(spans, maxspan.end, end, words, tags)
          if (rightChildren.size > 0) children ++= rightChildren
        }
        children.toList
      }
    }
  }

  def findMaxSpan(spans: Array[Span], start: Int, end: Int): Option[Span] = {
    for (span <- spans) {
      if (span.start >= start && span.end <= end) return Some(span)
    }
    return None
  }
}