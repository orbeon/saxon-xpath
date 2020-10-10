////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.value

import org.orbeon.saxon.expr.sort.AtomicMatchKey

import org.orbeon.saxon.lib.StringCollator

import org.orbeon.saxon.model._

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.util.FastStringBuffer

import java.math.BigDecimal

import java.math.BigInteger

import java.math.RoundingMode

import java.util.Objects

import DayTimeDurationValue._


object DayTimeDurationValue {

  def makeDayTimeDurationValue(s: CharSequence): ConversionResult = {
    val d: ConversionResult = DurationValue.makeDuration(s, allowYM = false, allowDT = true)
    if (d.isInstanceOf[ValidationFailure]) {
      return d
    }
    val dv: DurationValue = d.asInstanceOf[DurationValue]
    Converter.DurationToDayTimeDuration.INSTANCE.convert(dv)
  }

  def fromSeconds(seconds: BigDecimal): DayTimeDurationValue = {
    var bigSeconds: BigDecimal = seconds
    val sdv: DayTimeDurationValue = new DayTimeDurationValue()
    sdv.negative = bigSeconds.signum() < 0
    if (sdv.negative) {
      bigSeconds = bigSeconds.negate()
    }
    val wholeSeconds: BigInteger = bigSeconds.toBigInteger
    // ArithmeticException if out of range

    // ORBEON: `BigInteger`
//    sdv.seconds = wholeSeconds.longValueExact
    sdv.seconds = wholeSeconds.longValue
    val fractionalPart: BigDecimal = bigSeconds.remainder(BigDecimal.ONE)
    val nanoseconds: BigDecimal =
      fractionalPart.multiply(BigDecimalValue.BIG_DECIMAL_ONE_BILLION)
    sdv.nanoseconds = nanoseconds.intValue()
    if (sdv.seconds == 0 && sdv.nanoseconds == 0) {
      // can happen with underflow (division by a very large number)
      sdv.negative = false
    }
    sdv
  }

  def fromMilliseconds(milliseconds: Long): DayTimeDurationValue = {
    var milliSeconds: Long = milliseconds
    val sign: Int = java.lang.Long.signum(milliSeconds)
    if (sign < 0) {
      milliSeconds = -milliSeconds
    }
    new DayTimeDurationValue(sign,
      0,
      0,
      0,
      milliSeconds / 1000,
      (milliSeconds % 1000).toInt * 1000)
  }

  def fromMicroseconds(microseconds: Long): DayTimeDurationValue = {
    var microSeconds: Long = microseconds
    val sign: Int = java.lang.Long.signum(microSeconds)
    if (sign < 0) {
      microSeconds = -microSeconds
    }
    new DayTimeDurationValue(sign,
      0,
      0,
      0,
      microSeconds / 1000000,
      (microSeconds % 1000000).toInt)
  }

  def fromNanoseconds(nanoseconds: Long): DayTimeDurationValue =
    new DayTimeDurationValue(0,
      0,
      0,
      nanoseconds / 1000000000L,
      (nanoseconds % 1000000000L).toInt)

  def fromJavaDuration(duration: java.time.Duration): DayTimeDurationValue = {
    val seconds: Long = duration.getSeconds
    val nanoseconds: Int = duration.getNano
    val negative: Boolean = false
    if (seconds < 0) {
      new DayTimeDurationValue(0, 0, 0, seconds, -1000000000 + nanoseconds)
    } else {
      new DayTimeDurationValue(0, 0, 0, seconds, nanoseconds)
    }
  }

}

class DayTimeDurationValue
  extends DurationValue
    with Comparable[DayTimeDurationValue] {

  typeLabel = BuiltInAtomicType.DAY_TIME_DURATION

  def this(sign: Int,
           days: Int,
           hours: Int,
           minutes: Int,
           seconds: Long,
           microseconds: Int) = {
    this()
    var microSeconds: Int = microseconds
    if (days < 0 || hours < 0 || minutes < 0 || seconds < 0 ||
      microSeconds < 0) {
      throw new IllegalArgumentException("Negative component value")
    }
    if (days.toDouble * (24 * 60 * 60) + hours.toDouble * (60 * 60) +
      minutes.toDouble * 60 +
      seconds.toDouble >
      java.lang.Long.MAX_VALUE) {
      throw new IllegalArgumentException("Duration seconds limit exceeded")
    }
    negative = sign < 0
    months = 0
    val h: Long = days.toLong * 24L + hours.toLong
    val m: Long = h * 60L + minutes.toLong
    var s: Long = m * 60L + seconds
    if (microSeconds > 1000000) {
      s += microSeconds / 1000000
      microSeconds %= 1000000
    }
    this.seconds = s
    this.nanoseconds = microSeconds * 1000
    if (s == 0 && microSeconds == 0) {
      negative = false
    }
    typeLabel = BuiltInAtomicType.DAY_TIME_DURATION
  }

  def this(days: Int,
           hours: Int,
           minutes: Int,
           seconds: Long,
           nanoseconds: Int) = {
    this()
    var numOfDay: Int = days
    var numOfHours: Int = hours
    var numOfMinutes: Int = minutes
    var numOfSeconds: Long = seconds
    var nanoSeconds: Int = nanoseconds
    val somePositive: Boolean = numOfDay > 0 || numOfHours > 0 || numOfMinutes > 0 || numOfSeconds > 0 ||
      nanoSeconds > 0
    val someNegative: Boolean = numOfDay < 0 || numOfHours < 0 || numOfMinutes < 0 || numOfSeconds < 0 ||
      nanoseconds < 0
    if (somePositive && someNegative) {
      throw new IllegalArgumentException(
        "Some component values are positive and others are negative")
    }
    if (someNegative) {
      negative = true
      numOfDay = -numOfDay
      numOfHours = -numOfHours
      numOfMinutes = -numOfMinutes
      numOfSeconds = -numOfSeconds
      nanoSeconds = -nanoSeconds
    }
    if (numOfDay.toDouble * (24 * 60 * 60) + numOfHours.toDouble * (60 * 60) +
      numOfMinutes.toDouble * 60 +
      numOfSeconds.toDouble >
      java.lang.Long.MAX_VALUE) {
      throw new IllegalArgumentException("Duration seconds limit exceeded")
    }
    months = 0
    val h: Long = numOfDay.toLong * 24L + numOfHours.toLong
    val m: Long = h * 60L + numOfMinutes.toLong
    var s: Long = m * 60L + seconds
    if (nanoSeconds > 1000000000) {
      s += nanoSeconds / 1000000000
      nanoSeconds %= 1000000000
    }
    this.seconds = s
    this.nanoseconds = nanoSeconds
    typeLabel = BuiltInAtomicType.DAY_TIME_DURATION
  }

  /*@NotNull*/

  override def copyAsSubType(typeLabel: AtomicType): AtomicValue = {
    val v: DayTimeDurationValue =
      DayTimeDurationValue.fromSeconds(getTotalSeconds)
    v.typeLabel = typeLabel
    v
  }

  override def getPrimitiveType: BuiltInAtomicType =
    BuiltInAtomicType.DAY_TIME_DURATION

  override def getPrimitiveStringValue(): CharSequence = {
    val sb = new FastStringBuffer(32)
    if (negative) {
      sb.cat('-')
    }
    val days: Int = getDays
    val hours: Int = getHours
    val minutes: Int = getMinutes
    val seconds: Int = getSeconds
    sb.cat('P')
    if (days != 0) {
      sb.append(s"${days}D")
    }
    if (days == 0 || hours != 0 || minutes != 0 || seconds != 0 ||
      nanoseconds != 0) {
      sb.cat('T')
    }
    if (hours != 0) {
      sb.append(s"${hours}H")
    }
    if (minutes != 0) {
      sb.append(s"${minutes}M")
    }
    if (seconds != 0 || nanoseconds != 0 || (days == 0 && minutes == 0 && hours == 0)) {
      if (nanoseconds == 0) {
        sb.append(s"${seconds}S")
      } else {
        DurationValue.formatFractionalSeconds(sb,
          seconds,
          (seconds * 1000000000L) + nanoseconds)
      }
    }
    sb
  }

  override def getLengthInSeconds(): Double = {
    val a: Double = seconds + (nanoseconds.toDouble / 1000000000)
    // System.err.println("Duration length " + days + "/" + hours + "/" + minutes + "/" + seconds + " is " + a);
    if (negative) -a else a
  }

  def getLengthInMicroseconds: Long = {
    if (seconds > java.lang.Long.MAX_VALUE / 1000000L) {
      throw new ArithmeticException(
        "Value is too large to be expressed in microseconds")
    }
    val a: Long = seconds * 1000000L + (nanoseconds / 1000)
    if (negative) -a else a
  }

  def getLengthInNanoseconds: Long = {
    if (seconds > java.lang.Long.MAX_VALUE / 1000000000L) {
      throw new ArithmeticException(
        "Value is too large to be expressed in nanoseconds")
    }
    val a: Long = seconds * 1000000000L + nanoseconds
    if (negative) -a else a
  }

  def toJavaDuration: java.time.Duration =
    if (negative) {
      java.time.Duration.ofSeconds(-seconds, -nanoseconds)
    } else {
      java.time.Duration.ofSeconds(seconds, nanoseconds)
    }

  override def multiply(factor: Long): DurationValue = // Fast path for simple cases
    if (Math.abs(factor) < 0x7fffffff && Math.abs(seconds) < 0x7fffffff) {
      new DayTimeDurationValue(
        0,
        0,
        0,
        seconds * factor * (if (negative) -1 else 1),
        (nanoseconds * factor * (if (negative) -1 else 1)).toInt)
    } else {
      multiply(BigDecimal.valueOf(factor))
    }

  override def multiply(n: Double): DayTimeDurationValue = {
    if (java.lang.Double.isNaN(n)) {
      val err = new XPathException(
        "Cannot multiply a duration by NaN")
      err.setErrorCode("FOCA0005")
      throw err
    }
    if (java.lang.Double.isInfinite(n)) {
      val err = new XPathException(
        "Cannot multiply a duration by infinity")
      err.setErrorCode("FODT0002")
      throw err
    }
    val factor: BigDecimal = BigDecimal.valueOf(n)
    multiply(factor)
  }

  private def multiply(factor: BigDecimal): DayTimeDurationValue = {
    val secs: BigDecimal = getTotalSeconds
    val product: BigDecimal = secs.multiply(factor)
    try fromSeconds(product)
    catch {
      case err@(_: IllegalArgumentException | _: ArithmeticException) =>
        if (err.getCause.isInstanceOf[XPathException]) {
          throw err.getCause.asInstanceOf[XPathException]
        } else {
          val err2: XPathException = new XPathException(
            "Overflow when multiplying a duration by a number",
            err)
          err2.setErrorCode("FODT0002")
          throw err2
        }

    }
  }

  override def divide(n: Double): DurationValue = {
    if (java.lang.Double.isNaN(n)) {
      val err = new XPathException(
        "Cannot divide a duration by NaN")
      err.setErrorCode("FOCA0005")
      throw err
    }
    if (n == 0) {
      val err = new XPathException(
        "Cannot divide a duration by zero")
      err.setErrorCode("FODT0002")
      throw err
    }
    val secs: BigDecimal = getTotalSeconds
    val product: BigDecimal = secs.divide(BigDecimal.valueOf(n))
    try fromSeconds(product)
    catch {
      case err@(_: IllegalArgumentException | _: ArithmeticException) =>
        if (err.getCause.isInstanceOf[XPathException]) {
          throw err.getCause.asInstanceOf[XPathException]
        } else {
          val err2: XPathException = new XPathException(
            "Overflow when dividing a duration by a number",
            err)
          err2.setErrorCode("FODT0002")
          throw err2
        }

    }
  }

  /**
   * Find the ratio between two durations
   *
   * @param other the dividend
   * @return the ratio, as a decimal
   * @throws XPathException when dividing by zero, or when dividing two durations of different type
   */
  override def divide(other: DurationValue): BigDecimalValue =
    if (other.isInstanceOf[DayTimeDurationValue]) {
      val v1: BigDecimal = getTotalSeconds
      val v2: BigDecimal = other.getTotalSeconds
      if (v2.signum() == 0) {
        val err = new XPathException(
          "Divide by zero (durations)")
        err.setErrorCode("FOAR0001")
        throw err
      }
      new BigDecimalValue(v1.divide(v2, 20, RoundingMode.HALF_EVEN))
    } else {
      val err = new XPathException(
        "Cannot divide two durations of different type")
      err.setErrorCode("XPTY0004")
      throw err
    }

  override def add(other: DurationValue): DurationValue =
    if (other.isInstanceOf[DayTimeDurationValue]) {
      val d2: DayTimeDurationValue = other.asInstanceOf[DayTimeDurationValue]
      if (((seconds | d2.seconds) & 0xffffffff00000000L) != 0) {
        // risk of complications, use BigDecimal arithmetic
        try {
          val v1: BigDecimal = getTotalSeconds
          val v2: BigDecimal = other.getTotalSeconds
          fromSeconds(v1.add(v2))
        } catch {
          case e: IllegalArgumentException => {
            val err = new XPathException(
              "Overflow when adding two durations")
            err.setErrorCode("FODT0002")
            throw err
          }

        }
      } else {
        // fast path for common case: no risk of overflow
        DayTimeDurationValue.fromNanoseconds(
          getLengthInNanoseconds + d2.getLengthInNanoseconds)
      }
    } else {
      val err = new XPathException(
        "Cannot add two durations of different type")
      err.setErrorCode("XPTY0004")
      throw err
    }

  override def subtract(other: DurationValue): DurationValue =
    if (other.isInstanceOf[DayTimeDurationValue]) {
      val d2: DayTimeDurationValue = other.asInstanceOf[DayTimeDurationValue]
      if (((seconds | d2.seconds) & 0xffffffff00000000L) != 0) {
        // risk of complications, use BigDecimal arithmetic
        try {
          val v1: BigDecimal = getTotalSeconds
          val v2: BigDecimal = other.getTotalSeconds
          fromSeconds(v1.subtract(v2))
        } catch {
          case e: IllegalArgumentException => {
            val err = new XPathException(
              "Overflow when subtracting two durations")
            err.setErrorCode("FODT0002")
            throw err
          }

        }
      } else {
        // fast path for common case: no risk of overflow
        DayTimeDurationValue.fromNanoseconds(
          getLengthInNanoseconds - d2.getLengthInNanoseconds)
      }
    } else {
      val err = new XPathException(
        "Cannot subtract two durations of different type")
      err.setErrorCode("XPTY0004")
      throw err
    }

  override def negate(): DurationValue = {
    val d2: DayTimeDurationValue = new DayTimeDurationValue()
    d2.setTypeLabel(typeLabel)
    d2.seconds = seconds
    d2.nanoseconds = nanoseconds
    d2.negative = !negative
    d2
  }

  def compareTo(other: DayTimeDurationValue): Int = {
    Objects.requireNonNull(other)
    if (this.negative != other.negative) {
      if (this.negative) -1 else +1
    } else if (this.seconds != other.seconds) {
      java.lang.Long.compare(this.seconds, other.seconds) *
        (if (this.negative) -1 else +1)
    } else {
      java.lang.Integer.compare(this.nanoseconds, other.nanoseconds) *
        (if (this.negative) -1 else +1)
    }
  }

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
 * A value of type xs:dayTimeDuration.
 * <p>Internally this is held as an integer number of seconds held in a positive long, a positive integer
 * number of microseconds in the range 0 to 999,999,999, and a boolean sign. Some of the constructor
 * and accessor methods cannot handle the full range of values.</p>
 */
