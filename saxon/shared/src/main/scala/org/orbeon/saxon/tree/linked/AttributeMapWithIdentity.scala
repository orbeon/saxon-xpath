package org.orbeon.saxon.tree.linked

import java.{util => ju}

import org.orbeon.saxon.om._
import org.orbeon.saxon.tree.iter.{AxisIterator, ListIterator}

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

class AttributeMapWithIdentity(private var attributes: ju.List[AttributeInfo])
  extends AttributeMap {

  override def size: Int = {
    var count: Int = 0
    for (att <- attributes.asScala if ! att.isInstanceOf[AttributeInfo.Deleted])
      count += 1
    count
  }

  def iterateAttributes(owner: ElementImpl): AxisIterator = {
    val list = new ju.ArrayList[NodeInfo](attributes.size)
    for (i <- 0 until attributes.size) {
      val att = attributes.get(i)
      if (!att.isInstanceOf[AttributeInfo.Deleted])
        list.add(new AttributeImpl(owner, i))
    }
    new ListIterator.OfNodes(list)
  }

  override def get(name: NodeName): AttributeInfo =
    attributes.asScala.find(info => info.getNodeName == name && !info.isInstanceOf[AttributeInfo.Deleted]).orNull

  override def get(uri: String, local: String): AttributeInfo = {
    for (info <- attributes.asScala) {
      val name = info.getNodeName
      if (name.getLocalPart == local && name.hasURI(uri) && ! info.isInstanceOf[AttributeInfo.Deleted])
        return info
    }
    null
  }

  def getIndex(uri: String, local: String): Int = {
    for (i <- 0 until attributes.size) {
      val info = attributes.get(i)
      val name: NodeName = info.getNodeName
      if (name.getLocalPart == local && name.hasURI(uri))
        return i
    }
    -1
  }

  def set(index: Int, info: AttributeInfo): AttributeMapWithIdentity = {
    val newList = new ju.ArrayList[AttributeInfo](attributes)
    if (index >= 0 && index < attributes.size)
      newList.set(index, info)
    else if (index == attributes.size)
      newList.add(info)
    new AttributeMapWithIdentity(newList)
  }

  def add(info: AttributeInfo): AttributeMapWithIdentity = {
    val newList = new ju.ArrayList[AttributeInfo](attributes)
    newList.add(info)
    new AttributeMapWithIdentity(newList)
  }

  def remove(index: Int): AttributeMapWithIdentity = {
    val newList = new ju.ArrayList[AttributeInfo](attributes)
    if (index >= 0 && index < attributes.size) {
      val del = new AttributeInfo.Deleted(attributes.get(index))
      newList.set(index, del)
    }
    new AttributeMapWithIdentity(newList)
  }

  override def getByFingerprint(fingerprint: Int, namePool: NamePool): AttributeInfo = {
    for (info <- attributes.asScala) {
      val name = info.getNodeName
      if (name.obtainFingerprint(namePool) == fingerprint)
        return info
    }
    null
  }

  def iterator: ju.Iterator[AttributeInfo] =
    (attributes.iterator.asScala filterNot (_.isInstanceOf[AttributeInfo.Deleted])).asJava

  override def asList: ju.List[AttributeInfo] =
    (attributes.asScala filterNot (_.isInstanceOf[AttributeInfo.Deleted])).asJava

  override def itemAt(index: Int): AttributeInfo = attributes.get(index)
}