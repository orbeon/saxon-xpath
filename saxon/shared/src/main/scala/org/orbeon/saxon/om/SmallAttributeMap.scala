package org.orbeon.saxon.om

import java.{util => ju}
//import scala.collection.compat._
import scala.jdk.CollectionConverters._

object SmallAttributeMap {
  val LIMIT: Int = 8
}

class SmallAttributeMap(attrib: ju.List[AttributeInfo]) extends AttributeMap {

  private val attributes: ju.List[AttributeInfo] = new ju.ArrayList(attrib)

  def size: Int = attributes.size

  override def get(name: NodeName): AttributeInfo =
    attributes.asScala.find(_.getNodeName == name).orNull

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

  def iterator: ju.Iterator[AttributeInfo] = attributes.iterator

  override def asList: ju.List[AttributeInfo] = new ju.ArrayList(attributes)

  override def itemAt(index: Int): AttributeInfo = attributes.get(index)
}
