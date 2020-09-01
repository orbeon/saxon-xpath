////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.om

import org.jetbrains.annotations.NotNull
import java.util.Collections
import java.util.Iterator

import scala.collection
import scala.jdk.CollectionConverters._




object EmptyAttributeMap {

  private var THE_INSTANCE: EmptyAttributeMap = new EmptyAttributeMap()

  def getInstance: EmptyAttributeMap = THE_INSTANCE

}

class EmptyAttributeMap private () extends AttributeMap {

  override def size(): Int = 0

  override def get(name: NodeName): AttributeInfo = null

  override def get(uri: String, local: String): AttributeInfo = null

  override def getByFingerprint(fingerprint: Int, namePool: NamePool): AttributeInfo = null

  override def put(att: AttributeInfo): AttributeMap =
    SingletonAttributeMap.of(att)

  override def remove(name: NodeName): AttributeMap = this

  @NotNull
  override def iterator(): collection.Iterator[AttributeInfo] =
    Collections.emptyIterator().asScala

  override def apply(mapper: java.util.function.Function[AttributeInfo, AttributeInfo])
    : AttributeMap = this

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * An implementation of AttributeMap representing an empty AttributeMap
  */
