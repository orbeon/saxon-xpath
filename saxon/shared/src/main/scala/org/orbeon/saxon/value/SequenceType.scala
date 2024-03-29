////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.value

import org.orbeon.saxon.expr.StaticProperty
import org.orbeon.saxon.expr.parser.RoleDiagnostic
import org.orbeon.saxon.model._
import org.orbeon.saxon.om.{GroundedValue, Item, Sequence}
import org.orbeon.saxon.pattern.{AnyNodeTest, NodeKindTest}
import org.orbeon.saxon.trans.XPathException


/**
 * SequenceType: a sequence type consists of a primary type, which indicates the type of item,
 * and a cardinality, which indicates the number of occurrences permitted. Where the primary type
 * is element or attribute, there may also be a content type, indicating the required type
 * annotation on the element or attribute content.
 */
object SequenceType {
  /**
   * A type that allows any sequence of items
   */
  val ANY_SEQUENCE: SequenceType = AnyItemType.zeroOrMore
  /**
   * A type that allows exactly one item, of any kind
   */
  val SINGLE_ITEM: SequenceType = AnyItemType.one
  /**
   * A type that allows zero or one items, of any kind
   */
  val OPTIONAL_ITEM: SequenceType = AnyItemType.zeroOrOne
  /**
   * A type that allows exactly one atomic value
   */
  val SINGLE_ATOMIC: SequenceType = BuiltInAtomicType.ANY_ATOMIC.one
  /**
   * A type that allows zero or one atomic values
   */
  val OPTIONAL_ATOMIC: SequenceType = BuiltInAtomicType.ANY_ATOMIC.zeroOrOne
  val ATOMIC_SEQUENCE: SequenceType = BuiltInAtomicType.ANY_ATOMIC.zeroOrMore
  /**
   * A type that allows a single string
   */
  val SINGLE_STRING: SequenceType = BuiltInAtomicType.STRING.one
  /**
   * A type that allows a single untyped atomic
   */
  val SINGLE_UNTYPED_ATOMIC: SequenceType = BuiltInAtomicType.UNTYPED_ATOMIC.one
  /**
   * A type that allows a single optional string
   */
  val OPTIONAL_STRING: SequenceType = BuiltInAtomicType.STRING.zeroOrOne
  /**
   * A type that allows a single boolean
   */
  val SINGLE_BOOLEAN: SequenceType = BuiltInAtomicType.BOOLEAN.one
  /**
   * A type that allows a single optional boolean
   */
  val OPTIONAL_BOOLEAN: SequenceType = BuiltInAtomicType.BOOLEAN.zeroOrOne
  /**
   * A type that allows a single integer
   */
  val SINGLE_INTEGER: SequenceType = BuiltInAtomicType.INTEGER.one
  /**
   * A type that allows a single decimal
   */
  val SINGLE_DECIMAL: SequenceType = BuiltInAtomicType.DECIMAL.one
  /**
   * A type that allows a single optional integer
   */
  val OPTIONAL_INTEGER: SequenceType = BuiltInAtomicType.INTEGER.zeroOrOne
  /**
   * A type that allows a single short
   */
  val SINGLE_SHORT: SequenceType = BuiltInAtomicType.SHORT.one
  /**
   * A type that allows a single optional short
   */
  val OPTIONAL_SHORT: SequenceType = BuiltInAtomicType.SHORT.zeroOrOne
  val SINGLE_BYTE: SequenceType = BuiltInAtomicType.BYTE.one
  /**
   * A type that allows a single optional byte
   */
  val OPTIONAL_BYTE: SequenceType = BuiltInAtomicType.BYTE.zeroOrOne
  /**
   * A type that allows a single double
   */
  val SINGLE_DOUBLE: SequenceType = BuiltInAtomicType.DOUBLE.one
  /**
   * A type that allows a single optional double
   */
  val OPTIONAL_DOUBLE: SequenceType = BuiltInAtomicType.DOUBLE.zeroOrOne
  /**
   * A type that allows a single float
   */
  val SINGLE_FLOAT: SequenceType = BuiltInAtomicType.FLOAT.one
  /**
   * A type that allows a single optional float
   */
  val OPTIONAL_FLOAT: SequenceType = BuiltInAtomicType.FLOAT.zeroOrOne
  /**
   * A type that allows a single optional decimal
   */
  val OPTIONAL_DECIMAL: SequenceType = BuiltInAtomicType.DECIMAL.zeroOrOne
  /**
   * A type that allows a single optional anyURI
   */
  val OPTIONAL_ANY_URI: SequenceType = BuiltInAtomicType.ANY_URI.zeroOrOne
  /**
   * A type that allows a single optional date
   */
  val OPTIONAL_DATE: SequenceType = BuiltInAtomicType.DATE.zeroOrOne
  /**
   * A type that allows a single optional time
   */
  val OPTIONAL_TIME: SequenceType = BuiltInAtomicType.TIME.zeroOrOne
  /**
   * A type that allows a single optional gYear
   */
  val OPTIONAL_G_YEAR: SequenceType = BuiltInAtomicType.G_YEAR.zeroOrOne
  /**
   * A type that allows a single optional gYearMonth
   */
  val OPTIONAL_G_YEAR_MONTH: SequenceType = BuiltInAtomicType.G_YEAR_MONTH.zeroOrOne
  /**
   * A type that allows a single optional gMonth
   */
  val OPTIONAL_G_MONTH: SequenceType = BuiltInAtomicType.G_MONTH.zeroOrOne
  /**
   * A type that allows a single optional gMonthDay
   */
  val OPTIONAL_G_MONTH_DAY: SequenceType = BuiltInAtomicType.G_MONTH_DAY.zeroOrOne
  /**
   * A type that allows a single optional gDay
   */
  val OPTIONAL_G_DAY: SequenceType = BuiltInAtomicType.G_DAY.zeroOrOne
  /**
   * A type that allows a single optional dateTime
   */
  val OPTIONAL_DATE_TIME: SequenceType = BuiltInAtomicType.DATE_TIME.zeroOrOne
  /**
   * A type that allows a single optional duration
   */
  val OPTIONAL_DURATION: SequenceType = BuiltInAtomicType.DURATION.zeroOrOne
  /**
   * A type that allows a single optional yearMonthDuration
   */
  val OPTIONAL_YEAR_MONTH_DURATION: SequenceType = BuiltInAtomicType.YEAR_MONTH_DURATION.zeroOrOne
  /**
   * A type that allows a single optional dayTimeDuration
   */
  val OPTIONAL_DAY_TIME_DURATION: SequenceType = BuiltInAtomicType.DAY_TIME_DURATION.zeroOrOne
  /**
   * A type that allows a single xs:QName
   */
  val SINGLE_QNAME: SequenceType = BuiltInAtomicType.QNAME.one
  /**
   * A type that allows a single optional xs:QName
   */
  val OPTIONAL_QNAME: SequenceType = BuiltInAtomicType.QNAME.zeroOrOne
  /**
   * A type that allows a single optional xs:NOTATION
   */
  val OPTIONAL_NOTATION: SequenceType = BuiltInAtomicType.NOTATION.zeroOrOne
  /**
   * A type that allows a single optional xs:Base64Binary
   */
  val OPTIONAL_BASE64_BINARY: SequenceType = BuiltInAtomicType.BASE64_BINARY.zeroOrOne
  /**
   * A type that allows a single optional xs:hexBinary
   */
  val OPTIONAL_HEX_BINARY: SequenceType = BuiltInAtomicType.HEX_BINARY.zeroOrOne
  /**
   * A type that allows an optional numeric value
   */
  val OPTIONAL_NUMERIC: SequenceType = makeSequenceType(NumericType.getInstance, StaticProperty.ALLOWS_ZERO_OR_ONE)
  val SINGLE_NUMERIC: SequenceType = makeSequenceType(NumericType.getInstance, StaticProperty.EXACTLY_ONE)
  /**
   * A type that allows zero or one nodes
   */
  val OPTIONAL_NODE: SequenceType = AnyNodeTest.zeroOrOne
  /**
   * A type that allows a single node
   */
  val SINGLE_NODE: SequenceType = AnyNodeTest.one
  /**
   * A type that allows a single document node
   */
  val OPTIONAL_DOCUMENT_NODE: SequenceType = NodeKindTest.DOCUMENT.zeroOrOne
  /**
   * A type that allows a sequence of zero or more nodes
   */
  val NODE_SEQUENCE: SequenceType = AnyNodeTest.zeroOrMore
  /**
   * A type that allows a sequence of zero or more string values
   */
  val STRING_SEQUENCE: SequenceType = BuiltInAtomicType.STRING.zeroOrMore
  /**
   * A type that allows a single function item
   */
  val SINGLE_FUNCTION: SequenceType = makeSequenceType(AnyFunctionType, StaticProperty.EXACTLY_ONE)
  /**
   * A type that allows a sequence of zero or one function items
   */
  val OPTIONAL_FUNCTION_ITEM: SequenceType = makeSequenceType(AnyFunctionType, StaticProperty.ALLOWS_ZERO_OR_ONE)
  /**
   * A type that allows a sequence of zero or mode function items
   */
  val FUNCTION_ITEM_SEQUENCE: SequenceType = makeSequenceType(AnyFunctionType, StaticProperty.ALLOWS_ZERO_OR_MORE)
  /**
   * A type that only permits the empty sequence
   */
  val EMPTY_SEQUENCE = new SequenceType(ErrorType, StaticProperty.EMPTY)
  /**
   * A type that only permits a non-empty sequence
   */
  val NON_EMPTY_SEQUENCE: SequenceType = makeSequenceType(AnyItemType, StaticProperty.ALLOWS_ONE_OR_MORE)
  /**
   * A type that has no instances
   */
  val VOID: SequenceType = makeSequenceType(ErrorType, StaticProperty.ALLOWS_MANY)

  /**
   * Construct an instance of SequenceType. This is a factory method: it maintains a
   * pool of SequenceType objects to reduce the amount of object creation.
   *
   * @param primaryType The item type
   * @param cardinality The required cardinality. This must be one of the constants { @link StaticProperty#EXACTLY_ONE},
   *                    { @link StaticProperty#ALLOWS_ONE_OR_MORE}, etc
   * @return the SequenceType (either a newly created object, or an existing one from the cache)
   */
  def makeSequenceType(primaryType: ItemType, cardinality: Int): SequenceType = {
    primaryType match {
      case bat: ItemType.WithSequenceTypeCache =>
        cardinality match {
          case StaticProperty.EXACTLY_ONE =>
            return bat.one
          case StaticProperty.ALLOWS_ZERO_OR_ONE =>
            return bat.zeroOrOne
          case StaticProperty.ALLOWS_ZERO_OR_MORE =>
            return bat.zeroOrMore
          case StaticProperty.ALLOWS_ONE_OR_MORE =>
            return bat.oneOrMore
          case _ =>
          // fall through
        }
      case _ =>
    }
    if (cardinality == StaticProperty.ALLOWS_ZERO)
      SequenceType.EMPTY_SEQUENCE
    else
      new SequenceType(primaryType, cardinality)
  }
}


class SequenceType {

  def this(primaryType: ItemType, cardinalityVar: Int) = {
    this()
    this.primaryType = primaryType
    this.cardinality = cardinalityVar
    if ((primaryType eq ErrorType) && Cardinality.allowsZero(cardinalityVar))
      this.cardinality = StaticProperty.EMPTY
    else
      this.cardinality = cardinalityVar
  }

  var primaryType: ItemType = _ // the primary type of the item, e.g. "element", "comment", or "integer"
  private var cardinality = 0 // the required cardinality

  /**
   * Get the "primary" part of this required type. E.g. for type element(*, xs:date) the "primary type" is element()
   *
   * @return The item type code of the primary type
   */
  def getPrimaryType: ItemType = primaryType

  /**
   * Get the cardinality component of this SequenceType. This is one of the constants {@link StaticProperty#EXACTLY_ONE},
   * {@link StaticProperty#ALLOWS_ONE_OR_MORE}, etc
   *
   * @return the required cardinality
   * @see org.orbeon.saxon.value.Cardinality
   */
  def getCardinality: Int = cardinality

  /**
   * Determine whether a given value is a valid instance of this SequenceType
   *
   * @param value the value to be tested
   * @param th    the type hierarchy cache
   * @return true if the value is a valid instance of this type
   * @throws XPathException if a dynamic error occurs while evaluating the Sequence (this
   *                        won't happen if the sequence is grounded)
   */
  @throws[XPathException]
  def matches(value: Sequence, th: TypeHierarchy): Boolean = {
    var count = 0
    val iter = value.iterate()
    var item: Item = null
    while ({
      item = iter.next()
      item
    } != null) {
      count += 1
      if (! primaryType.matches(item, th))
        return false
    }
    !(count == 0 && ! Cardinality.allowsZero(cardinality) || count > 1 && ! Cardinality.allowsMany(cardinality))
  }

  /**
   * Get extra diagnostic information about why a supplied item does not conform to this
   * item type, if available. If extra information is returned, it should be in the form of a complete
   * sentence, minus the closing full stop. No information should be returned for obvious cases.
   *
   * @param value the value which has been found not to match this sequence type
   * @param th    the TypeHierarchy cache
   */
  def explainMismatch(value: GroundedValue, th: TypeHierarchy): Option[String] = try {
    var count = 0
    val iter = value.iterate()
    var item: Item = null
    while ({
      item = iter.next()
      item
    } != null) {
      count += 1
      if (!primaryType.matches(item, th)) {
        var s = "The " + RoleDiagnostic.ordinal(count) + " item is not an instance of the required type"
        val more = primaryType.explainMismatch(item, th)
        if (more.isDefined) s = if (count == 1) more.get
        else s + ". " + more.get
        else if (count == 1) return None // no new information, so don't say anything
        return Some(s)
      }
    }
    if (count == 0 && !Cardinality.allowsZero(cardinality))
      Some("The type does not allow an empty sequence")
    else if (count > 1 && !Cardinality.allowsMany(cardinality))
      Some("The type does not allow a sequence of more than one item")
    else
      None
  } catch {
    case _: XPathException =>
      None
  }

  /**
   * Return a string representation of this SequenceType
   *
   * @return the string representation as an instance of the XPath
   *         SequenceType construct
   */
  override def toString: String =
    if (cardinality == StaticProperty.ALLOWS_ZERO)
      "empty-sequence()"
    else
      primaryType.toString + Cardinality.getOccurrenceIndicator(cardinality)

  /**
   * Return a string representation of this SequenceType suitable for use in stylesheet
   * export files. This differs from the result of toString() in that it will not contain
   * any references to anonymous types. Note that it may also use the Saxon extended syntax
   * for union types and tuple types.
   *
   * @return the string representation as an instance of the XPath SequenceType construct
   */
  def toExportString: String =
    if (cardinality == StaticProperty.ALLOWS_ZERO)
      "empty-sequence()"
    else
      primaryType.toExportString + Cardinality.getOccurrenceIndicator(cardinality)

  def toAlphaCode: String = AlphaCode.fromSequenceType(this)

  /**
   * Returns a hash code value for the object.
   */
  override def hashCode: Int = primaryType.hashCode ^ cardinality

  /**
   * Indicates whether some other object is "equal to" this one.
   */
  override def equals(obj: Any): Boolean =
    obj match {
      case other: SequenceType =>
        this.primaryType == other.primaryType && this.cardinality == other.cardinality
      case _ =>
        false
    }

  def isSameType(other: SequenceType, th: TypeHierarchy): Boolean =
    cardinality == other.cardinality && (th.relationship(primaryType, other.primaryType) eq Affinity.SAME_TYPE)
}