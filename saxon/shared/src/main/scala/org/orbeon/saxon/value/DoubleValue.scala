////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A numeric (double precision floating point) value
  */

package org.orbeon.saxon.value

import java.math.{BigDecimal, RoundingMode}

import org.orbeon.saxon.expr.sort.{AtomicMatchKey, AtomicSortComparer, DoubleSortComparer}
import org.orbeon.saxon.model.{AtomicType, BuiltInAtomicType, ValidationException}
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.value.DoubleValue._


object DoubleValue {

  val ZERO          : DoubleValue = new DoubleValue(0.0)
  val NEGATIVE_ZERO : DoubleValue = new DoubleValue(-0.0)
  val ONE           : DoubleValue = new DoubleValue(1.0)
  val NaN           : DoubleValue = new DoubleValue(java.lang.Double.NaN)

  def makeDoubleValue(value: Double): DoubleValue = new DoubleValue(value)

  def doubleToString(value: Double): CharSequence =
    FloatingPointConverter.appendDouble(
      new FastStringBuffer(FastStringBuffer.C16),
      value,
      forceExponential = false)
}

class DoubleValue extends NumericValue {

  typeLabel = BuiltInAtomicType.DOUBLE
  var value: Double = _

  def this(value: Double, `type`: AtomicType) = {
    this()
    this.value = value
    typeLabel = `type`
  }

  def this(value: Double) = {
    this()
    this.value = value
  }

  /*@NotNull*/
  def copyAsSubType(typeLabel: AtomicType): AtomicValue = {
    val v = new DoubleValue(value)
    v.typeLabel = typeLabel
    v
  }

  def getPrimitiveType: BuiltInAtomicType = BuiltInAtomicType.DOUBLE

  def getDoubleValue: Double = value

  /**
    * Get the numeric value converted to a float
    *
    * @return a float representing this numeric value; NaN if it cannot be converted
    */
  def getFloatValue: Float = value.toFloat

  /**
    * Get the numeric value converted to a decimal
    *
    * @return a decimal representing this numeric value;
    * @throws ValidationException
    *          if the value cannot be converted, for example if it is NaN or infinite
    */
  def getDecimalValue: BigDecimal = new BigDecimal(value)

  /**
    * Return the numeric value as a Java long.
    *
    * @return the numeric value as a Java long. This performs truncation
    *         towards zero.
    * @throws XPathException
    *          if the value cannot be converted
    */
  def longValue(): Long = value.toLong

  override def hashCode: Int =
    if (value > java.lang.Integer.MIN_VALUE && value < java.lang.Integer.MAX_VALUE)
      value.toInt
    else
      java.lang.Double.valueOf(value).hashCode

  override def isNaN: Boolean = java.lang.Double.isNaN(value)

  /**
    * Get the effective boolean value
    *
    * @return the effective boolean value (true unless the value is zero or NaN)
    */
  override def effectiveBooleanValue: Boolean =
    value != 0.0 && !java.lang.Double.isNaN(value)

  /**
    * Convert the double to a string according to the XPath 2.0 rules
    *
    * @return the string value
    */
  def getPrimitiveStringValue: CharSequence = doubleToString(value)

  override def getCanonicalLexicalRepresentation: CharSequence = {
    val fsb = new FastStringBuffer(FastStringBuffer.C16)
    FloatingPointConverter.appendDouble(fsb, value, forceExponential = true)
  }

  def negate(): NumericValue = new DoubleValue(-value)

  def floor(): NumericValue = new DoubleValue(Math.floor(value))

  def ceiling(): NumericValue = new DoubleValue(Math.ceil(value))

  def round(scale: Int): NumericValue =
    if (java.lang.Double.isNaN(value))
      this
    else if (java.lang.Double.isInfinite(value))
      this
    else if (value == 0.0)
      // handles the negative zero case
      this
    else if (scale == 0 && value > java.lang.Long.MIN_VALUE && value < java.lang.Long.MAX_VALUE) {
      if (value >= -0.5 && value < 0.0)
        new DoubleValue(-0.0)
      else
        new DoubleValue(Math.round(value))
    } else {
      val factor = Math.pow(10, scale + 1)
      var d = Math.abs(value * factor)
      if (java.lang.Double.isInfinite(d)) {
        // double arithmetic has overflowed - do it in decimal
        var dec = new BigDecimal(value)
        dec = dec.setScale(scale, RoundingMode.HALF_UP)
        new DoubleValue(dec.doubleValue())
      } else {
        val rem = d % 10
        if (rem >= 5)
          d += 10 - rem
        else if (rem < 5)
          d -= rem
        d /= factor
        if (value < 0)
          d = -d
        new DoubleValue(d)
      }
    }

  // Convert to a scaled integer, by multiplying by 10^scale
  // Now apply any rounding needed, using the "round half to even" rule***CHANGE
  // Now convert back to the original magnitude

  def roundHalfToEven(scale: Int): NumericValue = {
    if (java.lang.Double.isNaN(value))
      return this
    if (java.lang.Double.isInfinite(value))
      return this
    // handles the negative zero case
    if (value == 0.0)
      return this
    val factor = Math.pow(10, scale + 1)
    var d = Math.abs(value * factor)
    if (java.lang.Double.isInfinite(d)) {
      // double arithmetic has overflowed - do it in decimal
      var dec = new BigDecimal(value)
      dec = dec.setScale(scale, RoundingMode.HALF_EVEN)
      new DoubleValue(dec.doubleValue())
    }
    val rem = d % 10
    if (rem > 5) {
      d += 10 - rem
    } else if (rem < 5) {
      d -= rem
    } else {
// round half to even - check the last bit
      if ((d % 20) == 15)
        d += 5
      else
        d -= 5
    }
    d /= factor
    if (value < 0)
      d = -d
    new DoubleValue(d)
  }

  // Convert to a scaled integer, by multiplying by 10^scale
  // Now apply any rounding needed, using the "round half to even" rule
  // Now convert back to the original magnitude

  def signum(): Int = {
    if (java.lang.Double.isNaN(value))
      0
    else
      if (value > 0) 1 else if (value == 0) 0 else -1
  }

  /**
    * Ask whether this value is negative zero
    *
    * @return true if this value is float or double negative zero
    */
  override def isNegativeZero: Boolean =
    value == 0.0 &&
      (java.lang.Double
        .doubleToLongBits(value) & FloatingPointConverter.DOUBLE_SIGN_MASK) !=
        0

  def isWholeNumber: Boolean =
    value == Math.floor(value) && !java.lang.Double.isInfinite(value)

  /**
    * Test whether a number is a possible subscript into a sequence, that is,
    * a whole number greater than zero and less than 2^31
    *
    * @return the number as an int if it is a possible subscript, or -1 otherwise
    */
  def asSubscript(): Int =
    if (isWholeNumber && value > 0 && value <= java.lang.Integer.MAX_VALUE)
      value.toInt
    else
      -1

  def abs(): NumericValue =
    if (value > 0.0)
      this
    else
      new DoubleValue(Math.abs(value))

  def compareTo(other: Long): Int = {
    val otherDouble = other.toDouble
    if (value == otherDouble)
      0
    else
      if (value < otherDouble) -1 else +1
  }

  def getSchemaComparable: Comparable[AnyRef] = // that ensures NaN != NaN.
    if (value == 0.0) 0.0.asInstanceOf else value.asInstanceOf

  override def asMapKey(): AtomicMatchKey =
    if (isNaN)
      AtomicSortComparer.COLLATION_KEY_NaN
    else if (java.lang.Double.isInfinite(value))
      this
    else
      new BigDecimalValue(value)

  override def isIdentical(v: AtomicValue): Boolean =
    v.isInstanceOf[DoubleValue] &&
      DoubleSortComparer.getInstance.comparesEqual(this, v)
}
