////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.value

import org.orbeon.saxon.expr.sort.AtomicMatchKey
import org.orbeon.saxon.functions.AccessorFn
import org.orbeon.saxon.lib.StringCollator
import org.orbeon.saxon.model.AtomicType
import org.orbeon.saxon.model.BuiltInAtomicType
import org.orbeon.saxon.model.ConversionResult
import org.orbeon.saxon.model.ValidationFailure
import org.orbeon.saxon.om.SequenceTool
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.util.FastStringBuffer
import java.math.BigDecimal
import java.math.BigInteger
import java.util.StringTokenizer

import DurationValue._
import org.orbeon.saxon.functions.AccessorFn.Component.Component


object DurationValue {

  def formatFractionalSeconds(sb: FastStringBuffer,
                              seconds: Int,
                              nanosecs: Long): Unit = {
    var mss: String = nanosecs.toString
    if (seconds == 0) {
      mss = "0000000000" + mss
      mss = mss.substring(mss.length - 10)
    }
    sb.append(mss.substring(0, mss.length - 9))
    sb.cat('.')
    var lastSigDigit: Int = mss.length - 1
    while (mss.charAt(lastSigDigit) == '0') {
      lastSigDigit -= 1;
      lastSigDigit + 1
    }
    sb.append(mss.substring(mss.length - 9, lastSigDigit + 1))
    sb.cat('S')
  }

  /*@NotNull*/

  def makeDuration(s: CharSequence): ConversionResult =
    makeDuration(s, allowYM = true, allowDT = true)

  /*@NotNull*/

  def makeDuration(s: CharSequence,
                   allowYM: Boolean,
                   allowDT: Boolean): ConversionResult = {
    var years: Int = 0
    var months: Int = 0
    var days: Int = 0
    var hours: Int = 0
    var minutes: Int = 0
    var seconds: Int = 0
    var nanoseconds: Int = 0
    var negative: Boolean = false
    val tok: StringTokenizer = new StringTokenizer(
      Whitespace.trimWhitespace(s).toString,
      "-+.PYMDTHS",
      true)
    var components: Int = 0
    if (!tok.hasMoreElements()) {
     return badDuration("empty string", s)
    }
    var part: String = tok.nextElement().asInstanceOf[String]
    if ("+" == part) {
     return badDuration("+ sign not allowed in a duration", s)
    } else if ("-" == part) {
      negative = true
      part = tok.nextElement().asInstanceOf[String]
    }
    if ("P" != part) {
     return badDuration("missing 'P'", s)
    }
    var state: Int = 0
    while (tok.hasMoreElements()) {
      part = tok.nextElement().asInstanceOf[String]
      if ("T" == part) {
        state = 4
        if (!tok.hasMoreElements()) {
          return badDuration("T must be followed by time components", s)
        }
        part = tok.nextElement().asInstanceOf[String]
      }
      var value: Int = simpleInteger(part)
      if (value < 0) {
        if (value == -2) {
          badDuration("component of duration exceeds Saxon limits",
            s,
            "FODT0002")
        } else {
          badDuration("invalid or non-numeric component", s)
        }
      }
      if (!tok.hasMoreElements()) {
        return  badDuration("missing unit letter at end", s)
      }
      val delim: Char = tok.nextElement().asInstanceOf[String].charAt(0)
      delim match {
        case 'Y' =>
          if (state > 0) {
            return  badDuration("Y is out of sequence", s)
          }
          if (!allowYM) {
            return badDuration("Year component is not allowed in dayTimeDuration", s)
          }
          years = value
          state = 1
          components += 1
        case 'M' =>
          if (state == 4 || state == 5) {
            if (!allowDT) {
              return badDuration(
                "Minute component is not allowed in yearMonthDuration",
                s)
            }
            minutes = value
            state = 6
            components += 1
          } else if (state == 0 || state == 1) {
            if (!allowYM) {
              return badDuration("Month component is not allowed in dayTimeDuration",
                s)
            }
            months = value
            state = 2
            components += 1
          } else {
            badDuration("M is out of sequence", s)
          }
        case 'D' =>
          if (state > 2) {
            return badDuration("D is out of sequence", s)
          }
          if (!allowDT) {
            return badDuration("Day component is not allowed in yearMonthDuration", s)
          }
          days = value
          state = 3
          components += 1
        case 'H' =>
          if (state != 4) {
            return badDuration("H is out of sequence", s)
          }
          if (!allowDT) {
            return badDuration("Hour component is not allowed in yearMonthDuration",
              s)
          }
          hours = value
          state = 5
          components += 1
        case '.' =>
          if (state < 4 || state > 6) {
            return badDuration("misplaced decimal point", s)
          }
          seconds = value
          state = 7
        case 'S' =>
          if (state < 4 || state > 7) {
            return badDuration("S is out of sequence", s)
          }
          if (!allowDT) {
            return badDuration(
              "Seconds component is not allowed in yearMonthDuration",
              s)
          }
          if (state == 7) {
            val frac: StringBuilder = new StringBuilder(part)
            while (frac.length < 9) frac.append("0")
            part = frac.toString
            if (part.length > 9) {
              part = part.substring(0, 9)
            }
            value = simpleInteger(part)
            if (value < 0) {
              return badDuration("non-numeric fractional seconds", s)
            }
            nanoseconds = value
          } else {
            seconds = value
          }
          state = 8
          components += 1;
        case _ => badDuration("misplaced " + delim, s)

      }
    }
    if (components == 0) {
      return badDuration("Duration specifies no components", s)
    }
    if (negative) {
      years = -years
      months = -months
      days = -days
      hours = -hours
      minutes = -minutes
      seconds = -seconds
      nanoseconds = -nanoseconds
    }
    try new DurationValue(years,
      months,
      days,
      hours,
      minutes,
      seconds,
      nanoseconds,
      BuiltInAtomicType.DURATION)
    catch {
      case err: IllegalArgumentException =>
        new ValidationFailure(err.getMessage)

    }
  }

  def badDuration(msg: String, s: CharSequence): ValidationFailure = {
    val err: ValidationFailure = new ValidationFailure(
      "Invalid duration value '" + s + "' (" + msg + ')')
    err.setErrorCode("FORG0001")
    err
  }

  def badDuration(msg: String,
                  s: CharSequence,
                  errorCode: String): ValidationFailure = {
    val err: ValidationFailure = new ValidationFailure(
      "Invalid duration value '" + s + "' (" + msg + ')')
    err.setErrorCode(errorCode)
    err
  }

  def simpleInteger(s: String): Int = {
    var result: Long = 0
    val len = s.length
    if (len == 0) {
      return -1
    }
    for (i <- 0 until len) {
      val c: Char = s.charAt(i)
      if (c >= '0' && c <= '9') {
        result = result * 10 + (c - '0')
        if (result > java.lang.Integer.MAX_VALUE) {
          -2
        }
      } else {
        -1
      }
    }
    result.toInt
  }

  /*@NotNull*/

  def getSchemaComparable(value: DurationValue): Comparable[DurationComparable] = {
    var m: Int = value.months
    var s: Long = value.seconds
    var n: Int = value.nanoseconds
    if (value.negative) {
      s = -s
      m = -m
      n = -n
    }
    new DurationComparable(m, s, n)
  }

  class DurationComparable(private var months: Int,
                           private var seconds: Long,
                           private var nanoseconds: Int)
    extends Comparable[DurationComparable] {

    def compareTo(other: DurationComparable): Int = //            }
      if (months == other.months) {
        if (seconds == other.seconds) {
          java.lang.Integer.compare(nanoseconds, other.nanoseconds)
        } else {
          java.lang.Long.compare(seconds, other.seconds)
        }
      } else {
        // enough. We make the assumption, however, that the nanoseconds won't affect things.
        val oneDay: Double = 24e0 * 60e0 * 60e0
        val min0: Double = monthsToDaysMinimum(months) * oneDay + seconds
        val max0: Double = monthsToDaysMaximum(months) * oneDay + seconds
        val min1: Double = monthsToDaysMinimum(other.months) * oneDay + other.seconds
        val max1: Double = monthsToDaysMaximum(other.months) * oneDay + other.seconds
        if (max0 < min1) {
          -1
        } else if (min0 > max1) {
          +1
        } else {
          SequenceTool.INDETERMINATE_ORDERING
        }
      }

    // The months figure varies, but the seconds figure might tip things over if it's high
    // The months figure varies, but the seconds figure might tip things over if it's high

    override def equals(o: Any): Boolean = o match {
      case o: DurationComparable => compareTo(o) == 0
      case _ => false

    }

    override def hashCode: Int = months ^ seconds.toInt

    private def monthsToDaysMinimum(months: Int): Int = {
      if (months < 0) {
       return -monthsToDaysMaximum(-months)
      }
      if (months < 12) {
        val shortest: Array[Int] =
          Array(0, 28, 59, 89, 120, 150, 181, 212, 242, 273, 303, 334)
        shortest(months)
      } else {
        val years: Int = months / 12
        val remainingMonths: Int = months % 12
        // the -1 is to allow for the fact that we might miss a leap day if we time the start badly
        val yearDays: Int = years * 365 + (years % 4) - (years % 100) + (years % 400) -
          1
        yearDays + monthsToDaysMinimum(remainingMonths)
      }
    }

    private def monthsToDaysMaximum(months: Int): Int = {
      if (months < 0) {
       return -monthsToDaysMinimum(-months)
      }
      if (months < 12) {
        val longest: Array[Int] =
          Array(0, 31, 62, 92, 123, 153, 184, 215, 245, 276, 306, 337)
        longest(months)
      } else {
        val years: Int = months / 12
        val remainingMonths: Int = months % 12
        // the +1 is to allow for the fact that we might miss a leap day if we time the start badly
        val yearDays: Int = years * 365 + (years % 4) - (years % 100) + (years % 400) +
          1
        yearDays + monthsToDaysMaximum(remainingMonths)
      }
    }

  }

}

class DurationValue extends AtomicValue with AtomicMatchKey {

  var negative: Boolean = false

  var months: Int = 0

  var seconds: Long = 0

  var nanoseconds: Int = 0

  def this(positive: Boolean,
           years: Int,
           months: Int,
           days: Int,
           hours: Int,
           minutes: Int,
           seconds: Long,
           microseconds: Int,
           `type`: AtomicType) = {
    this()
    negative = !positive
    if (years < 0 || months < 0 || days < 0 || hours < 0 || minutes < 0 ||
      seconds < 0 ||
      microseconds < 0) {
      throw new IllegalArgumentException("Negative component value")
    }
    if (years.toDouble * 12 + months.toDouble > java.lang.Integer.MAX_VALUE) {
      throw new IllegalArgumentException("Duration months limit exceeded")
    }
    if (days.toDouble * (24 * 60 * 60) + hours.toDouble * (60 * 60) +
      minutes.toDouble * 60 +
      seconds.toDouble >
      java.lang.Long.MAX_VALUE) {
      throw new IllegalArgumentException("Duration seconds limit exceeded")
    }
    this.months = years * 12 + months
    val h: Long = days * 24L + hours
    val m: Long = h * 60L + minutes
    this.seconds = m * 60L + seconds
    this.nanoseconds = microseconds * 1000
    normalizeZeroDuration()
    typeLabel = `type`
  }

  def this(positive: Boolean,
           years: Int,
           months: Int,
           days: Int,
           hours: Int,
           minutes: Int,
           seconds: Long,
           microseconds: Int) =
    this(positive,
      years,
      months,
      days,
      hours,
      minutes,
      seconds,
      microseconds,
      BuiltInAtomicType.DURATION)

  def this(years: Int,
           months: Int,
           days: Int,
           hours: Int,
           minutes: Int,
           seconds: Long,
           nanoseconds: Int,
           atomicType: AtomicType) = {
    this()
    val somePositive: Boolean = years > 0 || months > 0 || days > 0 || hours > 0 || minutes > 0 ||
      seconds > 0 ||
      nanoseconds > 0
    val someNegative: Boolean = years < 0 || months < 0 || days < 0 || hours < 0 || minutes < 0 ||
      seconds < 0 ||
      nanoseconds < 0
    if (somePositive && someNegative) {
      throw new IllegalArgumentException(
        "Some component values are positive and some negative")
    }
    var getYears = years
    var getMonths = months
    var getDays = days
    var getHours = hours
    var getMinutes = minutes
    var getSeconds = seconds
    var getNanoseconds = nanoseconds
    if (someNegative) {
      getYears = -years
      getMonths = -months
      getDays = -days
      getHours = -hours
      getMinutes = -minutes
      getSeconds = -seconds
      getNanoseconds = -nanoseconds
    }
    if (getYears.toDouble * 12 + getMonths.toDouble > java.lang.Integer.MAX_VALUE) {
      throw new IllegalArgumentException("Duration months limit exceeded")
    }
    if (getDays.toDouble * (24 * 60 * 60) + hours.toDouble * (60 * 60) +
      getMinutes.toDouble * 60 +
      getSeconds.toDouble >
      java.lang.Long.MAX_VALUE) {
      throw new IllegalArgumentException("Duration seconds limit exceeded")
    }
    this.months = getYears * 12 + getMonths
    val h: Long = getDays * 24L + getHours
    val m: Long = h * 60L + getMinutes
    this.seconds = m * 60L + getSeconds
    this.nanoseconds = getNanoseconds
    negative = someNegative
    normalizeZeroDuration()
    typeLabel = atomicType
  }

  def normalizeZeroDuration(): Unit = {
    if (months == 0 && seconds == 0L && nanoseconds == 0) {
      negative = false
    }
  }

  def copyAsSubType(typeLabel: AtomicType): AtomicValue =
    if (negative) {
      new DurationValue(0, -months, 0, 0, 0, -seconds, -nanoseconds, typeLabel)
    } else {
      new DurationValue(0, months, 0, 0, 0, seconds, nanoseconds, typeLabel)
    }

  def getPrimitiveType: BuiltInAtomicType = BuiltInAtomicType.DURATION

  def signum(): Int = {
    if (negative) return -1
    if (months == 0 && seconds == 0L && nanoseconds == 0) return 0
    +1
  }

  def getYears: Int = months / 12

  def getMonths: Int = months % 12

  def getDays
  : Int = //        System.err.println("days (int) = " + (int)(seconds / (24L*60L*60L)));
    (seconds / (24L * 60L * 60L)).toInt

  def getHours: Int = (seconds % (24L * 60L * 60L) / (60L * 60L)).toInt

  def getMinutes: Int = (seconds % (60L * 60L) / 60L).toInt

  def getSeconds: Int = (seconds % 60L).toInt

  def getMicroseconds: Int = nanoseconds / 1000

  def getNanoseconds: Int = nanoseconds

  def getTotalMonths: Int = if (negative) -months else months

  def getTotalSeconds: BigDecimal = {
    var dec: BigDecimal = new BigDecimal(if (negative) -seconds else seconds)
    if (nanoseconds != 0) {
      dec = dec.add(
        new BigDecimal(
          BigInteger.valueOf(if (negative) -nanoseconds else nanoseconds),
          9))
    }
    dec
  }

  def getPrimitiveStringValue(): CharSequence = {
    val years: Int = getYears
    val months: Int = getMonths
    val days: Int = getDays
    val hours: Int = getHours
    val minutes: Int = getMinutes
    val seconds: Int = getSeconds

    if (months == 0 && seconds == 0L && nanoseconds == 0) return "PT0S"
    val sb = new FastStringBuffer(32)
    if (negative) sb.cat('-')
    sb.append("P")
    if (years != 0) sb.append(years.toString + "Y")
    if (months != 0) sb.append(months.toString + "M")
    if (days != 0) sb.append(days.toString + "D")
    if (hours != 0 || minutes != 0 || seconds != 0 || nanoseconds != 0) sb.append("T")
    if (hours != 0) sb.append(hours.toString + "H")
    if (minutes != 0) sb.append(minutes.toString + "M")
    if (seconds != 0 || nanoseconds != 0) {
      if (seconds != 0 && nanoseconds == 0) {
        sb.append(seconds.toString + "S")
      } else {
        formatFractionalSeconds(sb,
          seconds,
          (seconds * 1000000000L) + nanoseconds)
      }
    }
    sb
  }

  // Note, Schema does not define a canonical representation. We omit all zero components, unless
  // the duration is zero-length, in which case we output PT0S.
  // Note, Schema does not define a canonical representation. We omit all zero components, unless
  // the duration is zero-length, in which case we output PT0S.

  def getLengthInSeconds: Double = {
    val a: Double = months * (365.242199 / 12.0) * 24 * 60 * 60 + seconds +
      (nanoseconds.toDouble / 1000000000)
    if (negative) -a else a
  }

  import org.orbeon.saxon.functions.AccessorFn.Component._

  override def getComponent(component: Component): AtomicValue =
    component match {
      case YEAR =>
        Int64Value.makeIntegerValue(if (negative) -getYears else getYears)
      case MONTH =>
        Int64Value.makeIntegerValue(if (negative) -getMonths else getMonths)
      case DAY =>
        Int64Value.makeIntegerValue(if (negative) -getDays else getDays)
      case HOURS =>
        Int64Value.makeIntegerValue(if (negative) -getHours else getHours)
      case MINUTES =>
        Int64Value.makeIntegerValue(if (negative) -getMinutes else getMinutes)
      case SECONDS =>
        var sb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C16)
        var ms: String = "000000000" + nanoseconds
        ms = ms.substring(ms.length - 9)
        sb.append((if (negative) "-" else "") + getSeconds + '.' + ms)
        BigDecimalValue.parse(sb)
      case WHOLE_SECONDS =>
        Int64Value.makeIntegerValue(if (negative) -seconds else seconds)
      case MICROSECONDS =>
        new Int64Value((if (negative) -nanoseconds else nanoseconds) / 1000)
      case NANOSECONDS =>
        new Int64Value(if (negative) -nanoseconds else nanoseconds)
      case _ =>
        throw new IllegalArgumentException(
          "Unknown component for duration: " + component)

    }

  /*@Nullable*/

  def getXPathComparable(ordered: Boolean,
                         collator: StringCollator,
                         implicitTimezone: Int): AtomicMatchKey =
    if (ordered) null else this

  override def equals(other: Any): Boolean = other match {
    case other: DurationValue => {
      val d1: DurationValue = this
      val d2: DurationValue = other
      d1.negative == d2.negative && d1.months == d2.months &&
        d1.seconds == d2.seconds &&
        d1.nanoseconds == d2.nanoseconds
    }
    case _ => false

  }

  override def hashCode: Int =
    java.lang.Double.valueOf(getLengthInSeconds).hashCode

  def add(other: DurationValue): DurationValue = {
    val err = new XPathException(
      "Only subtypes of xs:duration can be added")
    err.setErrorCode("XPTY0004")
    err.setIsTypeError(true)
    throw err
  }

  def subtract(other: DurationValue): DurationValue = {
    val err = new XPathException(
      "Only subtypes of xs:duration can be subtracted")
    err.setErrorCode("XPTY0004")
    err.setIsTypeError(true)
    throw err
  }

  def negate(): DurationValue =
    if (negative) {
      new DurationValue(0, months, 0, 0, 0, seconds, nanoseconds, typeLabel)
    } else {
      new DurationValue(0, -months, 0, 0, 0, -seconds, -nanoseconds, typeLabel)
    }

  def multiply(factor: Long): DurationValue = multiply(factor.toDouble)

  def multiply(factor: Double): DurationValue = {
    val err = new XPathException(
      "Only subtypes of xs:duration can be multiplied by a number")
    err.setErrorCode("XPTY0004")
    err.setIsTypeError(true)
    throw err
  }

  def divide(factor: Double): DurationValue = {
    val err = new XPathException(
      "Only subtypes of xs:duration can be divided by a number")
    err.setErrorCode("XPTY0004")
    err.setIsTypeError(true)
    throw err
  }

  def divide(other: DurationValue): BigDecimalValue = {
    val err = new XPathException(
      "Only subtypes of xs:duration can be divided by another duration")
    err.setErrorCode("XPTY0004")
    err.setIsTypeError(true)
    throw err
  }

  /*@NotNull*/

  def getSchemaComparable(): Comparable[AnyRef] = DurationValue.getSchemaComparable(this).asInstanceOf

  //.asInstanceOf[Comparable[AnyRef]]

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * A value of type xs:duration
 */
