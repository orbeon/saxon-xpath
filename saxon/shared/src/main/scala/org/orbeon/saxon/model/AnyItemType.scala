////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.model

import org.orbeon.saxon.expr.StaticProperty
import org.orbeon.saxon.om.{Genre, Item}
import org.orbeon.saxon.value.SequenceType


/**
  * An implementation of ItemType that matches any item (node or atomic value)
  */
object AnyItemType extends ItemType.WithSequenceTypeCache {

  private var _one       : SequenceType = _
  private var _oneOrMore : SequenceType = _
  private var _zeroOrOne : SequenceType = _
  private var _zeroOrMore: SequenceType = _

  /**
    * Determine the Genre (top-level classification) of this type
    *
    * @return the Genre to which this type belongs, specifically `Genre`
    */
  def getGenre: Genre.Genre = Genre.ANY

  /**
    * Get the corresponding `org.orbeon.saxon.model.UType`. A UType is a union of primitive item
    * types.
    *
    * @return the smallest UType that subsumes this item type
    */
  def getUType: UType = UType.ANY

  /**
    * Determine whether this item type is an atomic type
    *
    * @return true if this is ANY_ATOMIC_TYPE or a subtype thereof
    */
  def isAtomicType: Boolean = false

  /**
    * Get an alphabetic code representing the type, or at any rate, the nearest built-in type
    * from which this type is derived. The codes are designed so that for any two built-in types
    * A and B, alphaCode(A) is a prefix of alphaCode(B) if and only if A is a supertype of B.
    *
    * @return the alphacode for the nearest containing built-in type
    */
  def getBasicAlphaCode: String = ""

  def isPlainType: Boolean = false

  /**
    * Test whether a given item conforms to this type
    *
    *
    *
    * @param item    The item to be tested
    * @param th
    * @return true if the item is an instance of this type; false otherwise
    */
  def matches(item: Item, th: TypeHierarchy): Boolean = true

  /*@NotNull*/

  def getPrimitiveItemType: ItemType = this

  def getPrimitiveType: Int = Type.ITEM

  /*@NotNull*/

  def getAtomizedItemType: AtomicType = BuiltInAtomicType.ANY_ATOMIC

  def isAtomizable(th: TypeHierarchy): Boolean = true

  def getDefaultPriority: Double = -2

  /*@NotNull*/

  override def toString: String = "item()"

  override def hashCode: Int = "AnyItemType".hashCode

  def one: SequenceType = {
    if (_one == null)
      _one = new SequenceType(this, StaticProperty.EXACTLY_ONE)
    _one
  }

  def zeroOrOne: SequenceType = {
    if (_zeroOrOne == null)
      _zeroOrOne = new SequenceType(this, StaticProperty.ALLOWS_ZERO_OR_ONE)
    _zeroOrOne
  }

  def oneOrMore: SequenceType = {
    if (_oneOrMore == null)
      _oneOrMore = new SequenceType(this, StaticProperty.ALLOWS_ONE_OR_MORE)
    _oneOrMore
  }

  def zeroOrMore: SequenceType = {
    if (_zeroOrMore == null)
      _zeroOrMore = new SequenceType(this, StaticProperty.ALLOWS_ZERO_OR_MORE)
    _zeroOrMore
  }
}
