////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.om

import java.{util => ju}

import org.orbeon.saxon.om.LargeAttributeMap._
import org.orbeon.saxon.tree.util.FastStringBuffer

import scala.jdk.CollectionConverters._

object LargeAttributeMap {

  private class AttributeInfoLink {
    var payload: AttributeInfo = _
    var prior: NodeName = _
    var next: NodeName = _
  }
}

class LargeAttributeMap extends AttributeMap {

  private var attributes: Map[NodeName, AttributeInfoLink] = _
  private var firstNode: NodeName = _
  private var lastNode: NodeName = _
  private var _size: Int = _

  override def size: Int = _size

  def this(atts: ju.List[AttributeInfo]) = {
    this()
    assert(!atts.isEmpty)
    this.attributes = Map.empty[NodeName, AttributeInfoLink]
    this._size = atts.size
    var current: AttributeInfoLink = null
    for (att: AttributeInfo <- atts.asScala) {
      if (attributes.contains(att.nodeName))
        throw new IllegalArgumentException("Attribute map contains duplicates")
      val link: AttributeInfoLink = new AttributeInfoLink()
      link.payload = att
      if (current == null) {
        firstNode = att.nodeName
      } else {
        current.next = att.nodeName
        link.prior = current.payload.nodeName
      }
      current = link
      attributes = attributes.updated(att.nodeName, link)
    }
    lastNode = current.payload.nodeName
  }

  private def this(
      attributes: Map[NodeName, AttributeInfoLink],
      size: Int,
      first: NodeName,
      last: NodeName) = {
    this()
    this.attributes = attributes
    this._size = size
    this.firstNode = first
    this.lastNode = last
  }

  override def get(name: NodeName): AttributeInfo =
    attributes.get(name).map(_.payload).orNull

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
    val existing = attributes.get(att.nodeName).orNull
    val link = new AttributeInfoLink
    var last2 = lastNode
    var att2 = attributes
    link.payload = att
    if (existing == null) {
      link.prior = lastNode
      last2 = att.nodeName
      val oldLast = attributes(lastNode)
      val penult = new AttributeInfoLink
      penult.payload = oldLast.payload
      penult.next = att.nodeName
      penult.prior = oldLast.prior
      att2 = att2.updated(lastNode, penult)
    } else {
      link.prior = existing.prior
      link.next = existing.next
    }
    att2 = att2.updated(att.nodeName, link)
    val size2 = if (existing == null) size + 1 else size
    new LargeAttributeMap(att2, size2, firstNode, last2)
  }

  override def remove(name: NodeName): AttributeMap = // Not actually used (or tested)
    attributes.get(name) match {
      case None =>
        this
      case Some(existing) =>
        var first2 = firstNode
        var last2 = lastNode
        var att2 = attributes.removed(name)
        if (existing.prior != null) {
          val priorLink = attributes(existing.prior)
          val priorLink2 = new AttributeInfoLink()
          priorLink2.payload = priorLink.payload
          priorLink2.prior = priorLink.prior
          priorLink2.next = existing.next
          att2 = att2.updated(existing.prior, priorLink2)
        } else {
          first2 = existing.next
        }
        if (existing.next != null) {
          val nextLink = attributes(existing.next)
          val nextLink2 = new AttributeInfoLink()
          nextLink2.payload = nextLink.payload
          nextLink2.next = nextLink.next
          nextLink2.prior = existing.prior
          att2 = att2.updated(existing.next, nextLink2)
        } else {
          last2 = existing.prior
        }
        new LargeAttributeMap(att2, size - 1, first2, last2)
    }

  override def iterator: Iterator[AttributeInfo] =
    new Iterator[AttributeInfo] {

      var current: NodeName = firstNode

      def hasNext: Boolean = current != null

      def next(): AttributeInfo = {
        val link = attributes(current)
        current = link.next
        link.payload
      }
    }

  override def asList: ju.List[AttributeInfo] = synchronized {
    val result = new ju.ArrayList[AttributeInfo](size)
    iterator foreach result.add
    result
  }

  override def toString: String = {
    val sb = new FastStringBuffer(256)
    for (att <- this) {
      sb.cat(att.nodeName.getDisplayName)
        .cat("=\"")
        .cat(att.value)
        .cat("\" ")
    }
    sb.toString.trim()
  }

}
