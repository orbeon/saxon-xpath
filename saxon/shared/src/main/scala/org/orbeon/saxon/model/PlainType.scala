////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.model

import org.orbeon.saxon.om.{Item, StructuredQName}


/**
  * A "plain type" is either an atomic type, or a union type that (a) imposes no restrictions other
  * than those imposed by its member types, and (b) has exclusively plain types as its member types
  */
trait PlainType extends ItemType {

  def getTypeName: StructuredQName
  def isNamespaceSensitive: Boolean
  def getPlainMemberTypes: Iterable[_ <: PlainType]
  def matches(item: Item, th: TypeHierarchy): Boolean

  /**
    * Redeclare getPrimitiveItemType to return a more specific result type
    * Get the primitive item type corresponding to this item type.
    * For anyAtomicValue and union types it is Type.ATOMIC_VALUE. For numeric it is Type.NUMBER.
    * For other atomic types it is the primitive type as defined in XML Schema,
    * except that integer, xs:dayTimeDuration, and xs:yearMonthDuration
    * are considered to be primitive types.
    *
    * @return the corresponding primitive type (this is an instance of BuiltInAtomicType in all cases
    * except where this type is xs:error. The class ErrorType does not inherit from BuiltInAtomicType
    * because of multiple inheritance problems).
    */
  def getPrimitiveItemType: AtomicType
}

