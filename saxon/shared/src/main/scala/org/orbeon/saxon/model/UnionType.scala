////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.model

import org.orbeon.saxon.lib.ConversionRules

import org.orbeon.saxon.om.AtomicSequence

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.NamespaceResolver

import org.orbeon.saxon.om.StructuredQName

import org.orbeon.saxon.tree.util.FastStringBuffer

import org.orbeon.saxon.value.AtomicValue

import org.orbeon.saxon.value.SequenceType

import java.util.Optional

import org.orbeon.saxon.om.Genre.ATOMIC




trait UnionType extends ItemType with CastingTarget {

  def getTypeName: StructuredQName

  def getStructuredQName: StructuredQName = getTypeName

  def containsListType(): Boolean

  def getPlainMemberTypes: Iterable[_ <: PlainType]

  def getResultTypeOfCast: SequenceType

  def getTypedValue(value: CharSequence,
                    resolver: NamespaceResolver,
                    rules: ConversionRules): AtomicSequence

  def checkAgainstFacets(value: AtomicValue,
                         rules: ConversionRules): ValidationFailure

  /**
    * Get extra diagnostic information about why a supplied item does not conform to this
    * item type, if available. If extra information is returned, it should be in the form of a complete
    * sentence, minus the closing full stop. No information should be returned for obvious cases.
    *
    * @param item the item that doesn't match this type
    * @param th   the type hierarchy cache
    * @return optionally, a message explaining why the item does not match the type
    */
  override def explainMismatch(item: Item,
                               th: TypeHierarchy): Optional[String] =
    if (item.getGenre == ATOMIC) {
      val message: FastStringBuffer = new FastStringBuffer(256)
      message.append("The required type is a union type allowing any of ")
      var punctuation: String = "("
      try for (member <- getPlainMemberTypes) {
        message.append(punctuation)
        punctuation = ", "
        message.append(member.getTypeName.getDisplayName)
      } catch {
        case e: MissingComponentException =>
          message.append("*member types unobtainable*")

      }
      message.append("), but the supplied type ")
      message.append(item.asInstanceOf[AtomicValue].getItemType.getDisplayName)
      message.append(" is not any of these")
      Optional.of(message.toString)
    } else {
      Optional.empty()
    }

  def getDescription: String =
    if (this.isInstanceOf[SimpleType]) {
      this.asInstanceOf[SimpleType].getDescription
    } else {
      toString
    }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Interface representing a union type. This may be either a built-in union type (of which there are
  * currently two, namely ErrorType and NumericType), or a user-defined union type.
  */
