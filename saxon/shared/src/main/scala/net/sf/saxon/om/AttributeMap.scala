////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.om

import scala.jdk.CollectionConverters._

import java.util.ArrayList

import java.util.List



object AttributeMap {

  def fromList(list: List[AttributeInfo]): AttributeMap = {
    val n: Int = list.size
    if (n == 0) {
      EmptyAttributeMap.getInstance
    } else if (n == 1) {
      SingletonAttributeMap.of(list.get(0))
    } else if (n <= SmallAttributeMap.LIMIT) {
      new SmallAttributeMap(list)
    } else {
      new LargeAttributeMap(list.asScala.toList)
    }
  }

}

trait AttributeMap extends Iterable[AttributeInfo] {

  def size(): Int

  def get(name: NodeName): AttributeInfo =
    this.find(_.getNodeName == name).getOrElse(null)

  def get(uri: String, local: String): AttributeInfo = {
    for (att <- this) {
      val attName: NodeName = att.getNodeName
      if (attName.getLocalPart == local && attName.hasURI(uri)) {
        att
      }
    }
    null
  }

  def getByFingerprint(fingerprint: Int, namePool: NamePool): AttributeInfo = {
    for (att <- this) {
      val attName: NodeName = att.getNodeName
      if (attName.obtainFingerprint(namePool) == fingerprint) {
        att
      }
    }
    null
  }

  def getValue(uri: String, local: String): String = {
    val att: AttributeInfo = get(uri, local)
    if (att == null) null else att.getValue
  }

  def put(att: AttributeInfo): AttributeMap = {
    val list: List[AttributeInfo] = new ArrayList[AttributeInfo](size + 1)
    for (a <- this if a.getNodeName != att.getNodeName) {
      list.add(a)
    }
    list.add(att)
    AttributeMap.fromList(list)
  }

  def remove(name: NodeName): AttributeMap = {
    val list: List[AttributeInfo] = new ArrayList[AttributeInfo](size)
    for (a <- this if a.getNodeName != name) {
      list.add(a)
    }
    AttributeMap.fromList(list)
  }

  def verify(): Unit = {}

  def apply(mapper: java.util.function.Function[AttributeInfo, AttributeInfo])
    : AttributeMap = {
    val list: List[AttributeInfo] = new ArrayList[AttributeInfo](size)
    for (a <- this) {
      list.add(mapper.apply(a))
    }
    AttributeMap.fromList(list)
  }

  def asList(): List[AttributeInfo] = {
    val list: List[AttributeInfo] = new ArrayList[AttributeInfo](size)
    for (a <- this) {
      list.add(a)
    }
    list
  }

  def itemAt(index: Int): AttributeInfo = asList().get(index)

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * AttributeMap represents an immutable collection of attributes available on a particular element
  * node. An AttributeMap is an ordered collection of AttributeInfo objects. The order of the object
  * represents document order.
  */
