////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.om

import java.util

import net.sf.saxon.ma.trie.ImmutableHashTrieMap
import net.sf.saxon.tree.util.FastStringBuffer
import LargeAttributeMap._




object LargeAttributeMap {

  private class AttributeInfoLink {

    var payload: AttributeInfo = _

    var prior: NodeName = _

    var next: NodeName = _

  }

}

class LargeAttributeMap extends AttributeMap {

  private var attributes: ImmutableHashTrieMap[NodeName, AttributeInfoLink] = _

  private var firstNode: NodeName = _

  private var lastNode: NodeName = _

   var sizeInt: Int = _

  def this(atts: List[AttributeInfo]) = {
    this()
    assert(!atts.isEmpty)
    this.attributes = ImmutableHashTrieMap.empty[NodeName, AttributeInfoLink]()
    this.sizeInt = atts.size
    var current: AttributeInfoLink = null
    for (att:AttributeInfo <- atts) {
      if (attributes.get(att.getNodeName) != null) {
        throw new IllegalArgumentException("Attribute map contains duplicates")
      }
      val link: AttributeInfoLink = new AttributeInfoLink()
      link.payload = att
      if (current == null) {
        firstNode = att.getNodeName
      } else {
        current.next = att.getNodeName
        link.prior = current.payload.getNodeName
      }
      current = link
      attributes = attributes.put(att.getNodeName, link)
    }
    lastNode = current.payload.getNodeName
  }

  def this(
      attributes: ImmutableHashTrieMap[NodeName, AttributeInfoLink],
      size: Int,
      first: NodeName,
      last: NodeName) = {
    this()
    this.attributes = attributes
    this.sizeInt = size
    this.firstNode = first
    this.lastNode = last
  }

  override def get(name: NodeName): AttributeInfo = {
    val link: AttributeInfoLink = attributes.get(name)
    if (link == null) null else link.payload
  }

  override def get(uri: String, local: String): AttributeInfo = {
    val name: NodeName = new FingerprintedQName("", uri, local)
    get(name)
  }

  override def getByFingerprint(fingerprint: Int, namePool: NamePool): AttributeInfo = {
    val name: NodeName = new FingerprintedQName(
      namePool.getStructuredQName(fingerprint),
      fingerprint)
    get(name)
  }

  override def put(att: AttributeInfo): AttributeMap = {
    val existing: AttributeInfoLink = attributes.get(att.getNodeName)
    val link: AttributeInfoLink = new AttributeInfoLink()
    var last2: NodeName = lastNode
    link.payload = att
    if (existing == null) {
      link.prior = lastNode
      last2 = att.getNodeName
      val oldLast: AttributeInfoLink = attributes.get(lastNode)
      val penult: AttributeInfoLink = new AttributeInfoLink()
      penult.payload = oldLast.payload
      penult.next = att.getNodeName
      penult.prior = oldLast.prior
      attributes = attributes.put(lastNode, penult)
    } else {
      link.prior = existing.prior
      link.next = existing.next
    }
    val att2: ImmutableHashTrieMap[NodeName, AttributeInfoLink] =
      attributes.put(att.getNodeName, link)
    val size2: Int = if (existing == null) size + 1 else size
    new LargeAttributeMap(att2, size2, firstNode, last2)
  }

  override def remove(name: NodeName): AttributeMap = // Not actually used (or tested)
    if (attributes.get(name) == null) {
      this
    } else {
      var first2: NodeName = firstNode
      var last2: NodeName = lastNode
      val att2: ImmutableHashTrieMap[NodeName, AttributeInfoLink] =
        attributes.remove(name)
      val existing: AttributeInfoLink = attributes.get(name)
      if (existing.prior != null) {
        val priorLink: AttributeInfoLink = attributes.get(existing.prior)
        val priorLink2: AttributeInfoLink = new AttributeInfoLink()
        priorLink2.payload = priorLink.payload
        priorLink2.prior = priorLink.prior
        priorLink2.next = existing.next
        att2.put(existing.prior, priorLink2)
      } else {
        first2 = existing.next
      }
      if (existing.next != null) {
        val nextLink: AttributeInfoLink = attributes.get(existing.next)
        val nextLink2: AttributeInfoLink = new AttributeInfoLink()
        nextLink2.payload = nextLink.payload
        nextLink2.next = nextLink.next
        nextLink2.prior = existing.prior
        att2.put(existing.next, nextLink2)
      } else {
        last2 = existing.prior
      }
      new LargeAttributeMap(att2, size - 1, first2, last2)
    }

  override def iterator(): Iterator[AttributeInfo] =
    new Iterator[AttributeInfo]() {
      var current: NodeName = firstNode

      override def hasNext(): Boolean = current != null

      override def next(): AttributeInfo = {
        val link: AttributeInfoLink = attributes.get(current)
        current = link.next
        link.payload
      }
    }
import scala.jdk.CollectionConverters._
  override def toList(): List[AttributeInfo] = synchronized {
    var result: List[AttributeInfo] = new util.ArrayList[AttributeInfo](size).asScala.toList
    var iteratorVar : Iterator[AttributeInfo] = iterator()
    while(iteratorVar.hasNext) {
      val element : AttributeInfo = iteratorVar.next
      result:+element
    }
    result
  }

  override def toString(): String = {
    val sb: FastStringBuffer = new FastStringBuffer(256)
    for (att <- this) {
      sb.cat(att.getNodeName.getDisplayName)
        .cat("=\"")
        .cat(att.getValue)
        .cat("\" ")
    }
    sb.toString.trim()
  }

}
