package net.sf.saxon.om

import java.util.ArrayList
import java.util.Iterator
import java.util.List

import scala.jdk.CollectionConverters._

object SmallAttributeMap {

  val LIMIT: Int = 8

}

class SmallAttributeMap(attrib: List[AttributeInfo]) extends AttributeMap {

  private var attributes: List[AttributeInfo] = new ArrayList(attrib)

  override def size(): Int = attributes.size

  override def get(name: NodeName): AttributeInfo =
    attributes.asScala.find(_.getNodeName == name).getOrElse(null)

  override def get(uri: String, local: String): AttributeInfo = {
    for (info <- attributes.asScala) {
      val name: NodeName = info.getNodeName
      if (name.getLocalPart == local && name.hasURI(uri)) {
        info
      }
    }
    null
  }

  override def getByFingerprint(fingerprint: Int, namePool: NamePool): AttributeInfo = {
    for (info <- attributes.asScala) {
      val name: NodeName = info.getNodeName
      if (name.obtainFingerprint(namePool) == fingerprint) {
        info
      }
    }
    null
  }

  override def iterator(): scala.Iterator[AttributeInfo] = attributes.iterator().asScala

  override def asList(): List[AttributeInfo] = new ArrayList(attributes)

  override def itemAt(index: Int): AttributeInfo = attributes.get(index)

}
