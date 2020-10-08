////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

package org.orbeon.saxon.model

import org.orbeon.saxon.om.Genre.Genre
import org.orbeon.saxon.om.{Genre, Item}
import org.orbeon.saxon.value.ObjectValue


object AnyExternalObjectType {
  val THE_INSTANCE: AnyExternalObjectType = new AnyExternalObjectType
}

/**
  * This class represents the type of an external object returned by
  * an extension function, or supplied as an external variable/parameter.
  */
class AnyExternalObjectType extends ItemType {
  def isAtomicType: Boolean = false
  override def matches(item: Item, th: TypeHierarchy): Boolean = item.isInstanceOf[ObjectValue[_]]
  def isPlainType: Boolean = false
  def getPrimitiveType: Int = -1
  def getBasicAlphaCode: String = "X"
  def getPrimitiveItemType: ItemType = this
  def getUType: UType = UType.EXTENSION
  def getAtomizedItemType: AtomicType = BuiltInAtomicType.STRING
  def isAtomizable(th: TypeHierarchy): Boolean = true
  def getGenre: Genre = Genre.EXTERNAL
  def getDefaultPriority: Double = -1
}
