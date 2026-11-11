////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.om

/**
  * An implementation of AttributeMap representing an empty AttributeMap
  */
object EmptyAttributeMap {
  val getInstance: EmptyAttributeMap = new EmptyAttributeMap
}

class EmptyAttributeMap private () extends AttributeMap {
  override def size: Int = 0
  override def get(name: NodeName): AttributeInfo = null
  override def get(uri: String, local: String): AttributeInfo = null
  override def getByFingerprint(fingerprint: Int, namePool: NamePool): AttributeInfo = null
  override def put(att: AttributeInfo): AttributeMap = SingletonAttributeMap.of(att)
  override def remove(name: NodeName): AttributeMap = this
  def iterator: Iterator[AttributeInfo] = Iterator.empty
  override def apply(mapper: AttributeInfo => AttributeInfo): AttributeMap = this
}

