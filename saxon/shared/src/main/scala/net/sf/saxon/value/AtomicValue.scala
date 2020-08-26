////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.value

import java.util

import net.sf.saxon.expr.StaticContext
import net.sf.saxon.expr.StaticProperty
import net.sf.saxon.expr.sort.AtomicMatchKey
import net.sf.saxon.expr.sort.CodepointCollator
import net.sf.saxon.functions.AccessorFn
import net.sf.saxon.lib.{ConversionRules, StringCollator}
import net.sf.saxon.model._
import net.sf.saxon.om._
import net.sf.saxon.trans.Err
import net.sf.saxon.trans.NoDynamicContextException
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.iter.SingleAtomicIterator
import net.sf.saxon.tree.jiter.MonoIterator

import scala.jdk.CollectionConverters._
import net.sf.saxon.functions.AccessorFn.Component.Component




abstract class AtomicValue
    extends Item
    with AtomicSequence
    with ConversionResult
    with IdentityComparable {

   var typeLabel: AtomicType = _

  def atomize(): AtomicSequence = this

  override def head(): AtomicValue = this

  override def getLength(): Int = 1

  def setTypeLabel(`type`: AtomicType): Unit = {
    typeLabel = `type`
  }

  def getSchemaComparable(): Comparable[AnyRef]

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

  def getStringValueCS(): CharSequence = {
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
  def getCanonicalLexicalRepresentation(): CharSequence = getStringValueCS

  /*@Nullable*/

  override def itemAt(n: Int): AtomicValue = if (n == 0) head() else null

  /*@NotNull*/

  def getItemType(): AtomicType = typeLabel

  def getPrimitiveType(): BuiltInAtomicType

  def getUType(): UType = getItemType.getUType

  def getCardinality(): Int = StaticProperty.EXACTLY_ONE

  def copyAsSubType(typeLabel: AtomicType): AtomicValue

  def isNaN(): Boolean = false

  def getStringValue(): String = getStringValueCS.toString

   def getPrimitiveStringValue(): CharSequence

  override def effectiveBooleanValue(): Boolean = {
    val err: XPathException = new XPathException(
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
      if (parentType.isInstanceOf[SimpleType]) {
        stype = parentType.asInstanceOf[SimpleType]
      } else if (parentType.isInstanceOf[ComplexType] &&
                 parentType.asInstanceOf[ComplexType].isSimpleContent) {
        stype = parentType.asInstanceOf[ComplexType].getSimpleContentType
      }
      if (stype != null && !stype.isNamespaceSensitive) {
// Can't validate namespace-sensitive content statically
        val err: ValidationFailure = stype.validateContent(
          getStringValueCS,
          null,
          env.getConfiguration.getConversionRules.asInstanceOf[ConversionRules])
        if (err != null) {
          throw err.makeException()
        }
        return
      }
    }
    if (parentType.isInstanceOf[ComplexType] &&
        !parentType.asInstanceOf[ComplexType].isSimpleContent &&
        !parentType.asInstanceOf[ComplexType].isMixedContent &&
        !Whitespace.isWhite(getStringValueCS)) {
      val err: XPathException = new XPathException(
        "Complex type " + parentType.getDescription + " does not allow text content " +
          Err.wrap(getStringValueCS))
      err.setIsTypeError(true)
      throw err
    }
  }

  def checkValidInJavascript(): Unit = ()
// default - no action
// default - no action

  /*@NotNull*/

  def asAtomic(): AtomicValue = this

  override def toString(): String =
    typeLabel + "(\"" + getStringValueCS + "\")"

  /**
    * Get an iterator over all the items in the sequence
    *
    * @return an iterator over all the items
    */
  override def iterate(): SingleAtomicIterator[_ <: AtomicValue] =
    new SingleAtomicIterator(this)

  def iterator(): util.Iterator[AtomicValue] = new MonoIterator(this)

  /**
    * Get the genre of this item
    *
    * @return the genre: specifically, {@link Genre#ATOMIC};
    */
  override def getGenre():Genre.Genre = Genre.ATOMIC

}

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
