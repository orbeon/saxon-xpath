////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.value

import java.math.{BigDecimal, BigInteger}

import org.orbeon.saxon.functions.FormatNumber
import org.orbeon.saxon.model.{BuiltInAtomicType, ConversionResult, ValidationFailure}
import org.orbeon.saxon.om.StandardNames
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.trans.{Err, XPathException}


/**
 * This class represents the XPath built-in type xs:integer. It is used for all
 * subtypes of xs:integer, other than user-defined subtypes. There are two implementations
 *
 * of IntegerValue: `Int64Value`, which accommodates values up to `2^63`, and
 * `BigIntegerValue`, which accommodates unlimited-length integers.
 *
 * @since 9.8: changed in 9.8 to make this class a subclass of the new abstract
 *        class DecimalValue, to better reflect the XDM type hierarchy
 */
object IntegerValue {

  /**
   * Static data identifying the min and max values for each built-in subtype of xs:integer.
   * This is a sequence of triples, each holding the fingerprint of the type, the minimum
   * value, and the maximum value. The special value NO_LIMIT indicates that there is no
   * minimum (or no maximum) for this type. The special value MAX_UNSIGNED_LONG represents the
   * value `2^64-1`
   */
  private val NO_LIMIT: Long = -9999

  private val MAX_UNSIGNED_LONG: Long = -9998

  /*@NotNull*/
  private val ranges: Array[Long] = Array(
    StandardNames.XS_INTEGER,
    NO_LIMIT,
    NO_LIMIT,
    StandardNames.XS_LONG,
    java.lang.Long.MIN_VALUE,
    java.lang.Long.MAX_VALUE,
    StandardNames.XS_INT,
    java.lang.Integer.MIN_VALUE,
    java.lang.Integer.MAX_VALUE,
    StandardNames.XS_SHORT,
    java.lang.Short.MIN_VALUE,
    java.lang.Short.MAX_VALUE,
    StandardNames.XS_BYTE,
    java.lang.Byte.MIN_VALUE,
    java.lang.Byte.MAX_VALUE,
    StandardNames.XS_NON_NEGATIVE_INTEGER,
    0,
    NO_LIMIT,
    StandardNames.XS_POSITIVE_INTEGER,
    1,
    NO_LIMIT,
    StandardNames.XS_NON_POSITIVE_INTEGER,
    NO_LIMIT,
    0,
    StandardNames.XS_NEGATIVE_INTEGER,
    NO_LIMIT,
    -1,
    StandardNames.XS_UNSIGNED_LONG,
    0,
    MAX_UNSIGNED_LONG,
    StandardNames.XS_UNSIGNED_INT,
    0,
    4294967295L,
    StandardNames.XS_UNSIGNED_SHORT,
    0,
    65535,
    StandardNames.XS_UNSIGNED_BYTE,
    0,
    255
  )

  def makeIntegerValue(value: BigInteger): IntegerValue =
    if (value.compareTo(BigIntegerValue.MAX_LONG) > 0 || value.compareTo(
      BigIntegerValue.MIN_LONG) < 0) {
      new BigIntegerValue(value)
    } else {
      Int64Value.makeIntegerValue(value.longValue())
    }

  def makeIntegerValue(value: Double): ConversionResult =
    if (java.lang.Double.isNaN(value)) {
      val err = new ValidationFailure(
        "Cannot convert double NaN to an integer")
      err.setErrorCode("FOCA0002")
      err
    } else if (java.lang.Double.isInfinite(value)) {
      val err = new ValidationFailure(
        "Cannot convert double INF to an integer")
      err.setErrorCode("FOCA0002")
      err
    } else if (value > java.lang.Long.MAX_VALUE || value < java.lang.Long.MIN_VALUE) {
      if (value == Math.floor(value))
        new BigIntegerValue(FormatNumber.adjustToDecimal(value, 2).toBigInteger)
      else
        new BigIntegerValue(new BigDecimal(value).toBigInteger)
    } else
      Int64Value.makeIntegerValue(value.toLong)

  def makeIntegerValue(doubleValue: DoubleValue): ConversionResult = {
    val value = doubleValue.getDoubleValue
    makeIntegerValue(value)
  }

  def checkRange(value: Long, `type`: BuiltInAtomicType): Boolean = {
    val fp = `type`.getFingerprint
    var i = 0
    while (i < ranges.length) {
      if (ranges(i) == fp) {
        val min = ranges(i + 1)
        if (min != NO_LIMIT && value < min)
          return false
        val max = ranges(i + 2)
        return (max == NO_LIMIT || max == MAX_UNSIGNED_LONG || value <= max)
      }
      i += 3
    }
    throw new IllegalArgumentException("No range information found for integer subtype " + `type`.getDescription)
  }

  def getMinInclusive(`type`: BuiltInAtomicType): IntegerValue = {
    val fp = `type`.getFingerprint
    var i = 0
    while (i < ranges.length) {
      if (ranges(i) == fp) {
        val min = ranges(i + 1)
        if (min == NO_LIMIT)
          return null
        else
          return Int64Value.makeIntegerValue(min)
      }
      i += 3
    }
    null
  }

  def getMaxInclusive(`type`: BuiltInAtomicType): IntegerValue = {
    val fp = `type`.getFingerprint
    var i = 0
    while (i < ranges.length) {
      if (ranges(i) == fp) {
        val max = ranges(i + 2)
        if (max == NO_LIMIT)
          return null
        else if (max == MAX_UNSIGNED_LONG)
          return IntegerValue.makeIntegerValue(BigIntegerValue.MAX_UNSIGNED_LONG)
        else
          return Int64Value.makeIntegerValue(max)
      }
      i += 3
    }
    null
  }

  def checkBigRange(big: BigInteger, `type`: BuiltInAtomicType): Boolean = {
    var i = 0
    while (i < ranges.length) {
      if (ranges(i) == `type`.getFingerprint) {
        val min = ranges(i + 1)
        if (min != NO_LIMIT && BigInteger.valueOf(min).compareTo(big) > 0)
          return false
        val max = ranges(i + 2)
        if (max == NO_LIMIT)
          return true
        else if (max == MAX_UNSIGNED_LONG)
          return BigIntegerValue.MAX_UNSIGNED_LONG.compareTo(big) >= 0
        else
          return BigInteger.valueOf(max).compareTo(big) >= 0
      }
      i += 3
    }
    throw new IllegalArgumentException("No range information found for integer subtype " + `type`.getDescription)
  }

  def stringToInteger(s: CharSequence): ConversionResult = {
    val len = s.length
    var start: Int = 0
    var last: Int = len - 1
    while (start < len && s.charAt(start) <= 0x20)
      start += 1
    while (last > start && s.charAt(last) <= 0x20)
      last -= 1
    if (start > last)
      return new ValidationFailure("Cannot convert zero-length string to an integer")
    if (last - start < 16) {
      var negative: Boolean = false
      var value: Long = 0
      var i: Int = start
      if (s.charAt(i) == '+') {
        i += 1
      } else if (s.charAt(i) == '-') {
        negative = true
        i += 1
      }
      if (i > last)
        return new ValidationFailure("Cannot convert string " + Err.wrap(s, Err.VALUE) + " to integer: no digits after the sign")
      while (i <= last) {
        val d = s.charAt({i += 1; i - 1})
        if (d >= '0' && d <= '9')
          value = 10 * value + (d - '0')
        else
          return new ValidationFailure("Cannot convert string " + Err.wrap(s, Err.VALUE) + " to an integer")
      }
      Int64Value.makeIntegerValue(if (negative) -value else value)
    } else {
      try {
        var t = Whitespace.trimWhitespace(s)
        if (t.charAt(0) == '+')
          t = t.subSequence(1, t.length)
        if (t.length < 16)
          new Int64Value(java.lang.Long.parseLong(t.toString))
        else
          new BigIntegerValue(new BigInteger(t.toString))
      } catch {
        case _ =>
          new ValidationFailure("Cannot convert string " + Err.wrap(s, Err.VALUE) + " to an integer")
      }
    }
  }

  /*@Nullable*/
  def castableAsInteger(input: CharSequence): ValidationFailure = {
    val s = Whitespace.trimWhitespace(input)
    val last = s.length - 1
    if (last < 0)
      return new ValidationFailure("Cannot convert empty string to an integer")
    var i = 0
    if (s.charAt(i) == '+' || s.charAt(i) == '-')
      i += 1
    if (i > last)
      return new ValidationFailure("Cannot convert string " + Err.wrap(s, Err.VALUE) + " to integer: no digits after the sign")
    while (i <= last) {
      val d = s.charAt({i += 1; i - 1})
      if (d >= '0' && d <= '9') {} else {
        return new ValidationFailure(
          "Cannot convert string " + Err.wrap(s, Err.VALUE) +
            " to an integer: contains a character that is not a digit")
      }
    }
    null
  }

  def signum(i: Int): Int = (i >> 31) | (-i >>> 31)
}

abstract class IntegerValue extends DecimalValue {

  /*@Nullable*/
  def convertToSubType(`type`: BuiltInAtomicType, validate: Boolean): ValidationFailure
  /*@Nullable*/
  def validateAgainstSubType(`type`: BuiltInAtomicType): ValidationFailure
  /*@NotNull*/
  def getPrimitiveType: BuiltInAtomicType = BuiltInAtomicType.INTEGER
  def getDecimalValue: BigDecimal
  def isWholeNumber: Boolean = true
  def plus(other: IntegerValue): IntegerValue
  def minus(other: IntegerValue): IntegerValue
  def times(other: IntegerValue): IntegerValue
  def div(other: IntegerValue): NumericValue

  def div(other: IntegerValue, locator: Location): NumericValue =
    try div(other)
    catch {
      case err: XPathException =>
        err.maybeSetLocation(locator)
        throw err
    }

  def mod(other: IntegerValue): IntegerValue

  def mod(other: IntegerValue, locator: Location): IntegerValue =
    try mod(other)
    catch {
      case err: XPathException =>
        err.maybeSetLocation(locator)
        throw err
    }

  def idiv(other: IntegerValue): IntegerValue

  def idiv(other: IntegerValue, locator: Location): IntegerValue =
    try idiv(other)
    catch {
      case err: XPathException =>
        err.maybeSetLocation(locator)
        throw err
    }

  def asBigInteger(): BigInteger

  override def isIdentical(v: AtomicValue): Boolean =
    v.isInstanceOf[IntegerValue] && equals(v)
}


