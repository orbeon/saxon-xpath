////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

package org.orbeon.saxon.model

import org.orbeon.saxon.lib.ConversionRules
import org.orbeon.saxon.om.Genre.Genre
import org.orbeon.saxon.om.{Genre, Item, StructuredQName}
import org.orbeon.saxon.value.AtomicValue


/**
  * Interface for atomic types (these are either built-in atomic types
  * or user-defined atomic types). An AtomicType is both an ItemType (a possible type
  * for items in a sequence) and a SchemaType (a possible type for validating and
  * annotating nodes).
  */
trait AtomicType extends SimpleType with PlainType with CastingTarget {

  /**
    * Determine the Genre (top-level classification) of this type
    *
    * @return the Genre to which this type belongs, specifically `Genre#ATOMIC`
    */
  override def getGenre: Genre = Genre.ATOMIC

  def validate(primValue: AtomicValue,
               lexicalValue: CharSequence,
               rules: ConversionRules): ValidationFailure

  def isOrdered(optimistic: Boolean): Boolean
  def isAbstract: Boolean
  def isPrimitiveType: Boolean
  def isIdType: Boolean
  def isIdRefType: Boolean
  def isBuiltInType: Boolean
  def getTypeName: StructuredQName
  def getStringConverter(rules: ConversionRules): StringConverter

  /**
    * Get extra diagnostic information about why a supplied item does not conform to this
    * item type, if available. If extra information is returned, it should be in the form of a complete
    * sentence, minus the closing full stop. No information should be returned for obvious cases.
    *
    * @param item the item that doesn't match this type
    * @param th   the type hierarchy cache
    * @return optionally, a message explaining why the item does not match the type
    */
  override def explainMismatch(item: Item, th: TypeHierarchy): Option[String] =
    item match {
      case value: AtomicValue =>
        Some("The supplied value is of type " + value.getItemType)
      case _ =>
        Some("The supplied value is " + item.getGenre.getDescription)
    }

  /**
    * Get the default priority when this ItemType is used as an XSLT pattern.
    * @return the default priority. For an atomic type this is 1 minus 0.5^N, where
    * N is the depth of the type in the type hierarchy. The result is 0 for
    * xs:anyAtomicType, 0.5 for a primitive type such as xs:date, and between 0.5 and
    * 1.0 for derived atomic types.
    */
  override def getDefaultPriority: Double = {
    if (this == BuiltInAtomicType.ANY_ATOMIC) {
      return 0
    }
    var factor: Double = 1
    var at: SchemaType = this
    do {
      factor *= 0.5
      at = at.getBaseType
    } while (at != BuiltInAtomicType.ANY_ATOMIC);
    1 - factor
  }
}
