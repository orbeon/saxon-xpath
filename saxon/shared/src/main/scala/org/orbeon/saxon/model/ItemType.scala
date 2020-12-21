////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.model

import org.orbeon.saxon.om.Genre.Genre
import org.orbeon.saxon.om.Item
import org.orbeon.saxon.value.SequenceType


/**
  * ItemType is an interface that allows testing of whether an Item conforms to an
  * expected type. ItemType represents the types in the type hierarchy in the XPath model,
  * as distinct from the schema model: an item type is either `item()` (matches everything),
  * a node type (matches nodes), an atomic type (matches atomic values), or empty()
  * (matches nothing). Atomic types, represented by the class `AtomicType`, are also
  * instances of SimpleType in the schema type hierarchy. Node Types, represented by
  * the class `NodeTest`, are also Patterns as used in XSLT.
 *
  * Saxon assumes that apart from `AnyItemType` (which corresponds to `item()`
  * and matches anything), every `ItemType` will be either an `AtomicType`, a `org.orbeon.saxon.pattern.NodeTest`,
  * or a `FunctionItemType`. User-defined implementations of ItemType must therefore extend one of those
  * three classes/interfaces.
  *
  * @see AtomicType
  * @see org.orbeon.saxon.pattern.NodeTest
  * @see FunctionItemType
  */
object ItemType {
  trait WithSequenceTypeCache extends ItemType {
    def one: SequenceType
    def zeroOrOne: SequenceType
    def oneOrMore: SequenceType
    def zeroOrMore: SequenceType
  }
}

trait ItemType {

  def getGenre: Genre
  def isAtomicType: Boolean
  def isPlainType: Boolean
  def isTrueItemType: Boolean = true
  def matches(item: Item, th: TypeHierarchy): Boolean

  /*@NotNull*/
  def getPrimitiveItemType: ItemType
  def getPrimitiveType: Int
  def getUType: UType
  def getDefaultPriority: Double
  def getNormalizedDefaultPriority: Double = (getDefaultPriority + 1) / 2
  def getAtomizedItemType: PlainType
  def isAtomizable(th: TypeHierarchy): Boolean
  def getBasicAlphaCode: String
  def getFullAlphaCode: String = getBasicAlphaCode

  /**
    * Return a string representation of this ItemType suitable for use in stylesheet
    * export files. This differs from the result of toString() in that it will not contain
    * any references to anonymous types. Note that it may also use the Saxon extended syntax
    * for union types and tuple types. The default implementation returns the result of
    * calling `toString()`.
    *
    * @return the string representation as an instance of the XPath SequenceType construct
    */
  def toExportString: String = toString
  def toString: String
  def explainMismatch(item: Item, th: TypeHierarchy): Option[String] = None
}
