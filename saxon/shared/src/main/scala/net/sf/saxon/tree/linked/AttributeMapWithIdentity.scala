package net.sf.saxon.tree.linked

import java.util.{ArrayList, List}
import java.util.stream.Collectors

import net.sf.saxon.om._
import net.sf.saxon.tree.iter.{AxisIterator, ListIterator}

import scala.jdk.CollectionConverters._

class AttributeMapWithIdentity(private var attributes: List[AttributeInfo])
  extends AttributeMap {

  override def size: Int = {
    var count: Int = 0
    for (att <- attributes.asScala if ! att.isInstanceOf[AttributeInfo.Deleted])
      count += 1
    count
  }

  def iterateAttributes(owner: ElementImpl): AxisIterator = {
    val list: List[NodeInfo] = new ArrayList[NodeInfo](attributes.size)
    for (i <- 0 until attributes.size) {
      val att: AttributeInfo = attributes.get(i)
      if (!att.isInstanceOf[AttributeInfo.Deleted]) {
        list.add(new AttributeImpl(owner, i))
      }
    }
    new ListIterator.OfNodes(list)
  }

  override def get(name: NodeName): AttributeInfo =
    attributes.asScala.find(info => info.getNodeName == name && !info.isInstanceOf[AttributeInfo.Deleted]).orNull

  override def get(uri: String, local: String): AttributeInfo = {
    for (info <- attributes.asScala) {
      val name: NodeName = info.getNodeName
      if (name.getLocalPart == local && name.hasURI(uri) && ! info.isInstanceOf[AttributeInfo.Deleted])
        return info
    }
    null
  }

  def getIndex(uri: String, local: String): Int = {
    for (i <- 0 until attributes.size) {
      val info: AttributeInfo = attributes.get(i)
      val name: NodeName = info.getNodeName
      if (name.getLocalPart == local && name.hasURI(uri))
        return i
    }
    -1
  }

  def set(index: Int, info: AttributeInfo): AttributeMapWithIdentity = {
    val newList: List[AttributeInfo] = new ArrayList[AttributeInfo](attributes)
    if (index >= 0 && index < attributes.size)
      newList.set(index, info)
    else if (index == attributes.size)
      newList.add(info)
    new AttributeMapWithIdentity(newList)
  }

  def add(info: AttributeInfo): AttributeMapWithIdentity = {
    val newList: List[AttributeInfo] = new ArrayList[AttributeInfo](attributes)
    newList.add(info)
    new AttributeMapWithIdentity(newList)
  }

  def remove(index: Int): AttributeMapWithIdentity = {
    val newList: List[AttributeInfo] = new ArrayList[AttributeInfo](attributes)
    if (index >= 0 && index < attributes.size) {
      val del: AttributeInfo.Deleted =
        new AttributeInfo.Deleted(attributes.get(index))
      newList.set(index, del)
    }
    new AttributeMapWithIdentity(newList)
  }

  override def getByFingerprint(fingerprint: Int, namePool: NamePool): AttributeInfo = {
    for (info <- attributes.asScala) {
      val name: NodeName = info.getNodeName
      if (name.obtainFingerprint(namePool) == fingerprint)
        return info
    }
    null
  }

  override def iterator: collection.Iterator[AttributeInfo] =
    attributes
      .stream()
      .filter((info) => !info.isInstanceOf[AttributeInfo.Deleted])
      .iterator.asScala

  override def asList(): List[AttributeInfo] =
    attributes
      .stream()
      .filter((info) => !info.isInstanceOf[AttributeInfo.Deleted])
      .collect(Collectors.toList())

  override def itemAt(index: Int): AttributeInfo = attributes.get(index)
}