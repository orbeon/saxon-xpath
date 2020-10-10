////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * The AtomicValue class corresponds to the concept of an atomic value in the
  * XPath 2.0 data model. Atomic values belong to one of the 19 primitive types
  * defined in XML Schema; or they are of type xs:untypedAtomic; or they are
  * "external objects", representing a Saxon extension to the XPath 2.0 type system.
  * <p>The AtomicValue class contains some methods that are suitable for applications
  * to use, and many others that are designed for internal use by Saxon itself.
  * These have not been fully classified. At present, therefore, none of the methods on this
  * class should be considered to be part of the public Saxon API.</p>
  *
  * @author Michael H. Kay
  */

package org.orbeon.saxon.value

import java.util

import org.orbeon.saxon.expr.{StaticContext, StaticProperty}
import org.orbeon.saxon.expr.sort.{AtomicMatchKey, CodepointCollator}
import org.orbeon.saxon.functions.AccessorFn.Component.Component
import org.orbeon.saxon.lib.StringCollator
import org.orbeon.saxon.model._
import org.orbeon.saxon.om._
import org.orbeon.saxon.trans.{Err, XPathException}
import org.orbeon.saxon.tree.iter.SingleAtomicIterator
import org.orbeon.saxon.tree.jiter.MonoIterator


abstract class AtomicValue
    extends Item
    with AtomicSequence
    with ConversionResult
    with IdentityComparable {

  var typeLabel: AtomicType = _

  def atomize(): AtomicSequence = this

  override def head: AtomicValue = this
  override def getLength: Int = 1

  def setTypeLabel(`type`: AtomicType): Unit = {
    typeLabel = `type`
  }

  def getSchemaComparable: Comparable[AnyRef]

  def getXPathComparable(ordered: Boolean,
                         collator: StringCollator,
                         implicitTimezone: Int): AtomicMatchKey

  def asMapKey(): AtomicMatchKey =
    getXPathComparable(ordered = false,
                       CodepointCollator.getInstance,
                       CalendarValue.NO_TIMEZONE)

  //override def equals(o: Any): Boolean

  def isIdentical(v: AtomicValue): Boolean = // default implementation
    getSchemaComparable == v.getSchemaComparable

  def isIdentical(other: IdentityComparable): Boolean =
    other.isInstanceOf[AtomicValue] && isIdentical(
      other.asInstanceOf[AtomicValue])

  /**
    * Get a hashCode that offers the guarantee that if A.isIdentical(B), then A.identityHashCode() == B.identityHashCode()
    *
    * @return a hashCode suitable for use when testing for identity.
    */
  def identityHashCode()
    : Int = // default implementation, which presumes that if two objects are identical then they are equal.
    hashCode

  def getStringValueCS: CharSequence = {
    val cs: CharSequence = getPrimitiveStringValue
    try typeLabel.postprocess(cs)
    catch {
      case err: XPathException => cs

    }
  }

  /**
    * Get the canonical lexical representation as defined in XML Schema. This is not always the same
    * as the result of casting to a string according to the XPath rules.
    *
    * @return the canonical lexical representation if defined in XML Schema; otherwise, the result
    *         of casting to string according to the XPath 2.0 rules
    */
  def getCanonicalLexicalRepresentation: CharSequence = getStringValueCS

  /*@Nullable*/

  override def itemAt(n: Int): AtomicValue = if (n == 0) head else null

  /*@NotNull*/

  def getItemType: AtomicType = typeLabel

  def getPrimitiveType: BuiltInAtomicType

  def getUType: UType = getItemType.getUType

  def getCardinality: Int = StaticProperty.EXACTLY_ONE

  def copyAsSubType(typeLabel: AtomicType): AtomicValue

  def isNaN: Boolean = false

  def getStringValue: String = getStringValueCS.toString

   def getPrimitiveStringValue: CharSequence

  override def effectiveBooleanValue: Boolean = {
    val err = new XPathException(
      "Effective boolean value is not defined for an atomic value of type " +
        Type.displayTypeName(this))
    err.setIsTypeError(true)
    err.setErrorCode("FORG0006")
    throw err
  }
// unless otherwise specified in a subclass
// unless otherwise specified in a subclass

  def getComponent(component: Component): AtomicValue =
    throw new UnsupportedOperationException(
      "Data type does not support component extraction")

  def checkPermittedContents(parentType: SchemaType,
                             env: StaticContext,
                             whole: Boolean): Unit = {
    if (whole) {
      var stype: SimpleType = null
      parentType match {
        case simpleType: SimpleType =>
          stype = simpleType
        case complexType: ComplexType if complexType.isSimpleContent =>
          stype = complexType.getSimpleContentType
        case _ =>
      }
      if (stype != null && !stype.isNamespaceSensitive) {
// Can't validate namespace-sensitive content statically
        val err: ValidationFailure = stype.validateContent(
          getStringValueCS,
          null,
          env.getConfiguration.getConversionRules)
        if (err != null) {
          throw err.makeException()
        }
        return
      }
    }
    parentType match {
      case complexType: ComplexType if !Whitespace.isWhite(getStringValueCS) && !complexType.isMixedContent && !complexType.isSimpleContent =>
        val err = new XPathException(
          "Complex type " + parentType.getDescription + " does not allow text content " +
            Err.wrap(getStringValueCS))
        err.setIsTypeError(true)
        throw err
      case _ =>
    }
  }

  def checkValidInJavascript(): Unit = ()
// default - no action
// default - no action

  /*@NotNull*/

  def asAtomic(): AtomicValue = this

  override def toString: String =
    typeLabel.toString + "(\"" + getStringValueCS.toString + "\")"

  /**
    * Get an iterator over all the items in the sequence
    *
    * @return an iterator over all the items
    */
  override def iterate(): SingleAtomicIterator[_ <: AtomicValue] =
    new SingleAtomicIterator(this)

  def iterator: util.Iterator[AtomicValue] = new MonoIterator(this)

  /**
    * Get the genre of this item
    *
    * @return the genre: specifically, {@link Genre#ATOMIC};
    */
  def getGenre:Genre.Genre = Genre.ATOMIC

}

