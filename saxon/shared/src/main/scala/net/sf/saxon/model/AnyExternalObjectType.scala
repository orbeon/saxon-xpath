////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

package net.sf.saxon.model

import net.sf.saxon.om.Genre.Genre
import net.sf.saxon.om.{Genre, Item}
import net.sf.saxon.value.ObjectValue


object AnyExternalObjectType {
  var THE_INSTANCE: AnyExternalObjectType = new AnyExternalObjectType()
}

/**
  * This class represents the type of an external object returned by
  * an extension function, or supplied as an external variable/parameter.
  */
class AnyExternalObjectType  () extends ItemType {

  override def isAtomicType(): Boolean = false

  override def matches(item: Item, th: TypeHierarchy): Boolean =
    item.isInstanceOf[ObjectValue[Any]]

  override def isPlainType(): Boolean = false

  override def getPrimitiveType(): Int = -1

  /**
    * Get an alphabetic code representing the type, or at any rate, the nearest built-in type
    * from which this type is derived. The codes are designed so that for any two built-in types
    * A and B, alphaCode(A) is a prefix of alphaCode(B) if and only if A is a supertype of B.
    *
    * @return the alphacode for the nearest containing built-in type
    */
  override def getBasicAlphaCode(): String = "X"

  override def getPrimitiveItemType(): ItemType = this

  override def getUType(): UType = UType.EXTENSION

  override def getAtomizedItemType(): AtomicType = BuiltInAtomicType.STRING

  override def isAtomizable(th: TypeHierarchy): Boolean = true

  override def getGenre(): Genre = Genre.EXTERNAL

  /**
    * Get the default priority when this ItemType is used as an XSLT pattern
    *
    * @return the default priority
    */
  override def getDefaultPriority(): Double = -1

}

