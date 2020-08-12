////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.om

import net.sf.saxon.model.SimpleType
import net.sf.saxon.s9api.Location
import net.sf.saxon.tree.jiter.MonoIterator
import java.util.ArrayList
import java.util.Iterator
import java.util.List

import SingletonAttributeMap._

import scala.collection


object SingletonAttributeMap {

  def of(att: AttributeInfo): SingletonAttributeMap =
    if (att.isInstanceOf[SingletonAttributeMap]) {
      att.asInstanceOf[SingletonAttributeMap]
    } else {
      new SingletonAttributeMap(att.getNodeName,
        att.getType,
        att.getValue,
        att.getLocation,
        att.getProperties)
    }

}

class SingletonAttributeMap private(nodeName: NodeName,
                                    `type`: SimpleType,
                                    value: String,
                                    location: Location,
                                    properties: Int)
  extends AttributeInfo(nodeName, `type`, value, location, properties)
    with AttributeMap {

  override def size(): Int = 1

  override def get(name: NodeName): AttributeInfo =
    if (name == getNodeName) this else null

  override def get(uri: String, local: String): AttributeInfo =
    if (getNodeName.getLocalPart == local && getNodeName.hasURI(uri)) this
    else null

  override def getByFingerprint(fingerprint: Int, namePool: NamePool): AttributeInfo =
    if (getNodeName.obtainFingerprint(namePool) == fingerprint) this else null

  override def put(att: AttributeInfo): AttributeMap =
    if (getNodeName == att.getNodeName) {
      SingletonAttributeMap.of(att)
    } else {
      val list: List[AttributeInfo] = new ArrayList[AttributeInfo](2)
      list.add(this)
      list.add(att)
      new SmallAttributeMap(list)
    }

  override def remove(name: NodeName): AttributeMap =
    if (name == getNodeName) EmptyAttributeMap.getInstance else this

  import scala.jdk.CollectionConverters._

  override def iterator(): collection.Iterator[AttributeInfo] = new MonoIterator(this).asScala

  override def apply(mapper: java.util.function.Function[AttributeInfo, AttributeInfo])
  : AttributeMap = SingletonAttributeMap.of(mapper.apply(this))

  override def asList(): List[AttributeInfo] = {
    val list: List[AttributeInfo] = new ArrayList[AttributeInfo](1)
    list.add(this)
    list
  }

  override def itemAt(index: Int): AttributeInfo =
    if (index == 0) {
      this
    } else {
      throw new IndexOutOfBoundsException(index + " of 1")
    }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * An implementation of AttributeMap for use when there is exactly one attribute
 */
