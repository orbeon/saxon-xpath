////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.value

import net.sf.saxon.expr.sort.AtomicMatchKey

import net.sf.saxon.lib.StringCollator

import net.sf.saxon.model.AtomicType

import net.sf.saxon.model.BuiltInAtomicType

import net.sf.saxon.model.ConversionResult

import net.sf.saxon.model.ValidationFailure

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.util.FastStringBuffer

import java.math.BigDecimal

import java.math.RoundingMode

import YearMonthDurationValue._




object YearMonthDurationValue {

  def makeYearMonthDurationValue(s: CharSequence): ConversionResult = {
    val d: ConversionResult = DurationValue.makeDuration(s, true, false)
    if (d.isInstanceOf[ValidationFailure]) {
      return d
    }
    val dv: DurationValue = d.asInstanceOf[DurationValue]
    YearMonthDurationValue.fromMonths(
      (dv.getYears * 12 + dv.getMonths) * dv.signum())
  }

  def fromMonths(months: Int): YearMonthDurationValue = {
    val mdv: YearMonthDurationValue = new YearMonthDurationValue()
    mdv.negative = months < 0
    mdv.months = if (months < 0) -months else months
    mdv.seconds = 0
    mdv.nanoseconds = 0
    mdv
  }

}

class YearMonthDurationValue private ()
    extends DurationValue
    with Comparable[YearMonthDurationValue] {

  typeLabel = BuiltInAtomicType.YEAR_MONTH_DURATION

  /*@NotNull*/

  override def copyAsSubType(typeLabel: AtomicType): AtomicValue = {
    val v: YearMonthDurationValue =
      YearMonthDurationValue.fromMonths(getLengthInMonths)
    v.typeLabel = typeLabel
    v
  }

  override def getPrimitiveType(): BuiltInAtomicType =
    BuiltInAtomicType.YEAR_MONTH_DURATION

  override def getPrimitiveStringValue(): CharSequence = {
    val y: Int = getYears
    val m: Int = getMonths
    val sb: FastStringBuffer = new FastStringBuffer(32)
    if (negative) {
      sb.cat('-')
    }
    sb.cat('P')
    if (y != 0) {
      sb.append(y.toString + "Y")
    }
    if (m != 0 || y == 0) {
      sb.append(m.toString + "M")
    }
    sb
  }
// The canonical representation has months in the range 0-11
// The canonical representation has months in the range 0-11

  def getLengthInMonths(): Int = months * (if (negative) -1 else +1)

  override def multiply(factor: Long): YearMonthDurationValue = // Fast path for simple cases
    if (Math.abs(factor) < 30000 && Math.abs(months) < 30000) {
      YearMonthDurationValue.fromMonths(factor.toInt * getLengthInMonths)
    } else {
      multiply(factor.toDouble)
    }

  override def multiply(n: Double): YearMonthDurationValue = {
    if (java.lang.Double.isNaN(n)) {
      val err: XPathException = new XPathException(
        "Cannot multiply a duration by NaN")
      err.setErrorCode("FOCA0005")
      throw err
    }
    val m: Double = getLengthInMonths.toDouble
    val product: Double = n * m
    if (java.lang.Double.isInfinite(product) || product > java.lang.Integer.MAX_VALUE ||
        product < java.lang.Integer.MIN_VALUE) {
      val err: XPathException = new XPathException(
        "Overflow when multiplying a duration by a number")
      err.setErrorCode("FODT0002")
      throw err
    }
    fromMonths(Math.round(product).toInt)
  }

  override def divide(n: Double): DurationValue = {
    if (java.lang.Double.isNaN(n)) {
      val err: XPathException = new XPathException(
        "Cannot divide a duration by NaN")
      err.setErrorCode("FOCA0005")
      throw err
    }
    val m: Double = getLengthInMonths.toDouble
    val product: Double = m / n
    if (java.lang.Double.isInfinite(product) || product > java.lang.Integer.MAX_VALUE ||
        product < java.lang.Integer.MIN_VALUE) {
      val err: XPathException = new XPathException(
        "Overflow when dividing a duration by a number")
      err.setErrorCode("FODT0002")
      throw err
    }
    fromMonths(Math.round(product).toInt)
  }

  override def divide(other: DurationValue): BigDecimalValue =
    if (other.isInstanceOf[YearMonthDurationValue]) {
      val v1: BigDecimal = BigDecimal.valueOf(getLengthInMonths)
      val v2: BigDecimal = BigDecimal.valueOf(
        other.asInstanceOf[YearMonthDurationValue].getLengthInMonths)
      if (v2.signum() == 0) {
        val err: XPathException = new XPathException(
          "Divide by zero (durations)")
        err.setErrorCode("FOAR0001")
        throw err
      }
      new BigDecimalValue(v1.divide(v2, 20, RoundingMode.HALF_EVEN))
    } else {
      val err: XPathException = new XPathException(
        "Cannot divide two durations of different type")
      err.setErrorCode("XPTY0004")
      throw err
    }

  override def add(other: DurationValue): DurationValue =
    if (other.isInstanceOf[YearMonthDurationValue]) {
      fromMonths(
        getLengthInMonths +
          other.asInstanceOf[YearMonthDurationValue].getLengthInMonths)
    } else {
      val err: XPathException = new XPathException(
        "Cannot add two durations of different type")
      err.setErrorCode("XPTY0004")
      throw err
    }

  override def subtract(other: DurationValue): DurationValue =
    if (other.isInstanceOf[YearMonthDurationValue]) {
      fromMonths(
        getLengthInMonths -
          other.asInstanceOf[YearMonthDurationValue].getLengthInMonths)
    } else {
      val err: XPathException = new XPathException(
        "Cannot subtract two durations of different type")
      err.setErrorCode("XPTY0004")
      throw err
    }

  override def negate(): DurationValue = fromMonths(-getLengthInMonths)

  def compareTo(other: YearMonthDurationValue): Int =
    java.lang.Integer.compare(getLengthInMonths, other.getLengthInMonths)

  override def getXPathComparable(ordered: Boolean,
                         collator: StringCollator,
                         implicitTimezone: Int): AtomicMatchKey = this

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A value of type xs:yearMonthDuration.
  * <p>The state retained by this class is essentially a signed 32-bit integer representing the number
  * of months: that is, {@code year*12 + month}; plus a type label allowing subtypes of {@code xs:yearMonthDuration}
  * to be represented.</p>
  */
