////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.value

import net.sf.saxon.expr.sort.AtomicMatchKey

import net.sf.saxon.expr.sort.ComparisonException

import net.sf.saxon.lib.StringCollator

import net.sf.saxon.model.AtomicType

import net.sf.saxon.model.BuiltInAtomicType

import net.sf.saxon.model.ConversionResult

import net.sf.saxon.model.ValidationFailure

import net.sf.saxon.om.SequenceTool

import net.sf.saxon.trans.Err

import net.sf.saxon.trans.XPathException

import BooleanValue._

object BooleanValue {

  val TRUE: BooleanValue = new BooleanValue(true)

  val FALSE: BooleanValue = new BooleanValue(false)

  def get(value: Boolean): BooleanValue = if (value) TRUE else FALSE

  def fromString(s: CharSequence): ConversionResult = {
    var charSeq: CharSequence = s
    charSeq = Whitespace.trimWhitespace(s)
    val len: Int = charSeq.length
    if (len == 1) {
      val c: Char = charSeq.charAt(0)
      if (c == '1') {
        return TRUE
      } else if (c == '0') {
        return FALSE
      }
    } else if (len == 4) {
      if (charSeq.charAt(0) == 't' && charSeq.charAt(1) == 'r' && charSeq.charAt(2) == 'u' &&
        charSeq.charAt(3) == 'e') {
        return TRUE
      }
    } else if (len == 5) {
      if (charSeq.charAt(0) == 'f' && charSeq.charAt(1) == 'a' && charSeq.charAt(2) == 'l' &&
        charSeq.charAt(3) == 's' &&
        charSeq.charAt(4) == 'e') {
        return FALSE
      }
    }
    val err: ValidationFailure = new ValidationFailure(
      "The string " + Err.wrap(s, Err.VALUE) + " cannot be cast to a boolean")
    err.setErrorCode("FORG0001")
    err
  }

}

class BooleanValue private()
  extends AtomicValue
    with Comparable[BooleanValue]
    with AtomicMatchKey {

  typeLabel = BuiltInAtomicType.BOOLEAN
  var value: Boolean = _

  def this(value: Boolean) {
    this()
    this.value = value
  }

  def this(value: Boolean, typeLabel: AtomicType) = {
    this()
    this.value = value
    this.typeLabel = typeLabel
  }

  def copyAsSubType(typeLabel: AtomicType): AtomicValue = {
    val v: BooleanValue = new BooleanValue(value)
    v.typeLabel = typeLabel
    v
  }

  def getBooleanValue: Boolean = value

  /**
   * Get the effective boolean value of this expression
   *
   * @return the boolean value
   */
  override def effectiveBooleanValue(): Boolean = value

  def getPrimitiveType: BuiltInAtomicType = BuiltInAtomicType.BOOLEAN

  def getPrimitiveStringValue(): String = if (value) "true" else "false"

  def getSchemaComparable(): Comparable[AnyRef] = new BooleanComparable()

  private class BooleanComparable extends Comparable[AnyRef] {

    def asBoolean(): Boolean = BooleanValue.this.getBooleanValue

    def compareTo(o: AnyRef): Int =
      if (this == o.asInstanceOf[BooleanValue]) 0 else SequenceTool.INDETERMINATE_ORDERING

    override def equals(o: Any): Boolean = o match {
      case o: BooleanComparable => asBoolean() == o.asBoolean()
      case _ => false

    }

    override def hashCode(): Int = if (asBoolean()) 9999999 else 8888888

  }

  def getXPathComparable(ordered: Boolean,
                         collator: StringCollator,
                         implicitTimezone: Int): AtomicMatchKey = this

  def compareTo(other: BooleanValue): Int = {
    if (!(other.isInstanceOf[BooleanValue])) {
      val e: XPathException = new XPathException(
        "Boolean values are not comparable to " + other.getClass,
        "XPTY0004")
      throw new ComparisonException(e)
    }
    if (value == other.asInstanceOf[BooleanValue].value) {
      return 0
    }
    if (value) {
      return +1
    }
    -1
  }

  /**
   * Determine whether two boolean values are equal
   *
   * @param other the value to be compared to this value
   * @return true if the other value is a boolean value and is equal to this
   *         value
   * @throws ClassCastException if other value is not xs:boolean or derived therefrom
   */
  override def equals(other: Any): Boolean = other match {
    case other: BooleanValue => value == other.value
    case _ => false

  }

  /**
   * Get a hash code for comparing two BooleanValues
   *
   * @return the hash code
   */
  override def hashCode(): Int = if (value) 0 else 1

  /**
   * Diagnostic display of this value as a string
   *
   * @return a string representation of this value: "true()" or "false()"
   */
  override def toString: String = getStringValue + "()"

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * A boolean XPath value
 */
