////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.value

import org.orbeon.saxon.expr.sort.AtomicMatchKey

import org.orbeon.saxon.lib.StringCollator

import org.orbeon.saxon.model.ConversionResult

import org.orbeon.saxon.model.ValidationException

import org.orbeon.saxon.model.ValidationFailure

import org.orbeon.saxon.trans.XPathException

import java.math.BigDecimal

import NumericValue._




object NumericValue {

  /*@NotNull*/

  def parseNumber(in: String): NumericValue = {

    if (in.indexOf('e') >= 0 || in.indexOf('E') >= 0) {
      try new DoubleValue(java.lang.Double.parseDouble(in))
      catch {
        case e: NumberFormatException => DoubleValue.NaN

      }
    } else if (in.indexOf('.') >= 0) {
      val v: ConversionResult = BigDecimalValue.makeDecimalValue(in, validate = true)
      if (v.isInstanceOf[ValidationFailure]) {
        DoubleValue.NaN
      } else {
        v.asInstanceOf[NumericValue]
      }
    } else {
      val v: ConversionResult = IntegerValue.stringToInteger(in)
      if (v.isInstanceOf[ValidationFailure]) {
        DoubleValue.NaN
      } else {
        v.asInstanceOf[NumericValue]
      }
    }
  }

  def isInteger(value: AtomicValue): Boolean = value.isInstanceOf[IntegerValue]

}

abstract class NumericValue
    extends AtomicValue
    with Comparable[NumericValue]
    with AtomicMatchKey {

  /**
    * Get the numeric value as a double
    *
    * @return A double representing this numeric value; NaN if it cannot be
    *         converted
    */
  def getDoubleValue: Double

  def getFloatValue: Float

  def getDecimalValue: BigDecimal

  /**
    * Get the effective boolean value of the value. This override of this method throws no exceptions.
    *
    * @return true, unless the value is boolean false, numeric zero, or
    * zero-length string
    */
  def effectiveBooleanValue: Boolean

  /**
    * Return the numeric value as a Java long.
    *
    * @return the numeric value as a Java long. This performs truncation
    *         towards zero.
    * @throws org.orbeon.saxon.trans.XPathException
    *          if the value cannot be converted
    */
  def longValue(): Long

  def negate(): NumericValue

  /*@NotNull*/

  def floor(): NumericValue

  /*@NotNull*/

  def ceiling(): NumericValue

  def round(scale: Int): NumericValue

  def roundHalfToEven(scale: Int): NumericValue

  def signum(): Int

  def isNegativeZero: Boolean = false

  def isWholeNumber: Boolean

  def asSubscript(): Int

  def abs(): NumericValue

  /*@NotNull*/

  def getXPathComparable(ordered: Boolean,
                         collator: StringCollator,
                         implicitTimezone: Int): AtomicMatchKey = this

// when comparing with another number of the same type.
  def compareTo(other: NumericValue): Int = {
    val a: Double = getDoubleValue
    val b: Double = other.getDoubleValue
//noinspection UseCompareMethod
    if (a == b) {
     return 0
    }
    if (a < b) {
      return -1
    }
    +1
  }
// IntelliJ says this can be replaced with Double.compare(). But it can't. Double.compare()
// treats positive and negative zero as not equal; we want them treated as equal. XSLT3 test case
// boolean-014.  MHK 2020-02-17
// IntelliJ says this can be replaced with Double.compare(). But it can't. Double.compare()
// treats positive and negative zero as not equal; we want them treated as equal. XSLT3 test case
// boolean-014.  MHK 2020-02-17

  def compareTo(other: Long): Int

  override def equals(other: Any): Boolean = other match {
    case other: NumericValue => compareTo(other) == 0
    case _ => false

  }

  override def hashCode: Int

  override def toString: String = getStringValue

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * NumericValue is an abstract superclass for IntegerValue, DecimalValue,
  * FloatValue, and DoubleValue
  */
