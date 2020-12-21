////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.om

import java.{util => ju}

import org.orbeon.saxon.model.SimpleType
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.tree.jiter.MonoIterator


/**
 * An implementation of AttributeMap for use when there is exactly one attribute
 */
object SingletonAttributeMap {

  def of(att: AttributeInfo): SingletonAttributeMap =
    att match {
      case map: SingletonAttributeMap =>
        map
      case _ =>
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

  override def size: Int = 1

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
      val list: ju.List[AttributeInfo] = new ju.ArrayList[AttributeInfo](2)
      list.add(this)
      list.add(att)
      new SmallAttributeMap(list)
    }

  override def remove(name: NodeName): AttributeMap =
    if (name == getNodeName) EmptyAttributeMap.getInstance else this

  def iterator: ju.Iterator[AttributeInfo] = new MonoIterator(this)

  override def apply(mapper: AttributeInfo => AttributeInfo): AttributeMap =
    SingletonAttributeMap.of(mapper.apply(this))

  override def asList: ju.List[AttributeInfo] = {
    val list: ju.List[AttributeInfo] = new ju.ArrayList[AttributeInfo](1)
    list.add(this)
    list
  }

  override def itemAt(index: Int): AttributeInfo =
    if (index == 0) {
      this
    } else {
      throw new IndexOutOfBoundsException(index.toString + " of 1")
    }

}

