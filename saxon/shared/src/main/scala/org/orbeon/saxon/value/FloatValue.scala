////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.value

import java.math.BigDecimal

import org.orbeon.saxon.expr.sort.{AtomicMatchKey, AtomicSortComparer, DoubleSortComparer}
import org.orbeon.saxon.model.{AtomicType, BuiltInAtomicType, Converter, ValidationException}
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.value.FloatValue._


/**
 * A numeric (single precision floating point) value
 */
object FloatValue {

  val ZERO          : FloatValue = new FloatValue(0.0.toFloat)
  val NEGATIVE_ZERO : FloatValue = new FloatValue(-0.0.toFloat)
  val ONE           : FloatValue = new FloatValue(1.0.toFloat)
  val NaN           : FloatValue = new FloatValue(java.lang.Float.NaN)

  def makeFloatValue(value: Float): FloatValue = new FloatValue(value)

  def floatToString(value: Float): CharSequence =
    FloatingPointConverter.appendFloat(
      new FastStringBuffer(FastStringBuffer.C16),
      value,
      forceExponential = false)

}

class FloatValue extends NumericValue {

  var value: Float = _
  typeLabel = BuiltInAtomicType.FLOAT

  def this(value: Float, `type`: AtomicType) = {
    this()
    this.value = value
    typeLabel = `type`
  }

  def this(value: Float) = {
    this()
    this.value = value
  }

  def copyAsSubType(typeLabel: AtomicType): AtomicValue = {
    val v: FloatValue = new FloatValue(value)
    v.typeLabel = typeLabel
    v
  }

  def getPrimitiveType: BuiltInAtomicType = BuiltInAtomicType.FLOAT

  def getFloatValue: Float = value

  def getDoubleValue: Double = value.toDouble

  /**
   * Get the numeric value converted to a decimal
   *
   * @return a decimal representing this numeric value;
   * @throws ValidationException
   * if the value cannot be converted, for example if it is NaN or infinite
   */
  def getDecimalValue: BigDecimal = new BigDecimal(value.toDouble)

  /**
   * Return the numeric value as a Java long.
   *
   * @return the numeric value as a Java long. This performs truncation
   *         towards zero.
   * @throws XPathException
   * if the value cannot be converted
   */
  def longValue(): Long = value.toLong

  override def hashCode: Int =
    if (value > java.lang.Integer.MIN_VALUE && value < java.lang.Integer.MAX_VALUE) {
      value.toInt
    } else {
      java.lang.Double.valueOf(getDoubleValue).hashCode
    }

  override def isNaN: Boolean = java.lang.Float.isNaN(value)

  /**
   * Get the effective boolean value
   *
   * @return true unless the value is zero or NaN
   */
  override def effectiveBooleanValue: Boolean =
    value != 0.0 && !java.lang.Float.isNaN(value)

  /*@NotNull*/
  def getPrimitiveStringValue: CharSequence = floatToString(value)

  override def getCanonicalLexicalRepresentation: CharSequence = {
    val fsb = new FastStringBuffer(FastStringBuffer.C16)
    FloatingPointConverter.appendFloat(fsb, value, forceExponential = true)
  }

  def negate(): NumericValue = new FloatValue(-value)

  def floor(): NumericValue = new FloatValue(Math.floor(value).toFloat)

  def ceiling(): NumericValue = new FloatValue(Math.ceil(value).toFloat)

  def round(scale: Int): NumericValue =
    if (java.lang.Float.isNaN(value))
      this
    else if (java.lang.Float.isInfinite(value))
      this
    else if (value == 0.0)
      // handles the negative zero case
      this
    else if (scale == 0 && value > java.lang.Integer.MIN_VALUE && value < java.lang.Integer.MAX_VALUE) {
      if (value >= -0.5 && value < 0.0)
        new FloatValue(-0.0f)
      else
        new FloatValue(Math.round(value).toFloat)
    } else {
      var d = new DoubleValue(getDoubleValue)
      d = d.round(scale).asInstanceOf[DoubleValue]
      new FloatValue(d.getFloatValue)
    }

  def roundHalfToEven(scale: Int): NumericValue = {
    var d = new DoubleValue(getDoubleValue)
    d = d.roundHalfToEven(scale).asInstanceOf[DoubleValue]
    new FloatValue(d.getFloatValue)
  }

  def signum(): Int = {
    if (java.lang.Float.isNaN(value))
      return 0
    compareTo(0)
  }

  /**
   * Ask whether this value is negative zero
   *
   * @return true if this value is float or double negative zero
   */
  override def isNegativeZero: Boolean =
    value == 0.0 &&
      (java.lang.Float.floatToIntBits(value) & FloatingPointConverter.FLOAT_SIGN_MASK) != 0

  def isWholeNumber: Boolean =
    value == Math.floor(value) && !java.lang.Float.isInfinite(value)

  /**
   * Test whether a number is a possible subscript into a sequence, that is,
   * a whole number greater than zero and less than 2^31
   *
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
      new FloatValue(Math.abs(value))

  override def compareTo(other: NumericValue): Int =
    other match {
      case otherFloatValue: FloatValue =>
        val otherFloat = otherFloatValue.value
        // Do not rewrite as Float.compare() - see IntelliJ bug IDEA-196419
        if (value == otherFloat)
          0
        else if (value < otherFloat)
          -1
        else
          +1
      case otherDoubleValue: DoubleValue =>
        super.compareTo(otherDoubleValue)
      case _ =>
        compareTo(Converter.NumericToFloat.INSTANCE.convert(other))
    }

  def compareTo(other: Long): Int = {
    val otherFloat = other.toFloat
    if (value == otherFloat)
      return 0
    if (value < otherFloat) -1 else +1
  }

  def getSchemaComparable: Comparable[AnyRef] = // that ensures NaN != NaN.
    if (value == 0.0f) 0.0f.asInstanceOf else value.asInstanceOf

  override def asMapKey(): AtomicMatchKey =
    if (isNaN)
      AtomicSortComparer.COLLATION_KEY_NaN
    else if (java.lang.Double.isInfinite(value))
      new DoubleValue(value)
    else
      new BigDecimalValue(value)

  override def isIdentical(v: AtomicValue): Boolean =
    v.isInstanceOf[FloatValue] &&
      DoubleSortComparer.getInstance.comparesEqual(this,
        v.asInstanceOf[FloatValue])

  override def asAtomic(): FloatValue = this
}
