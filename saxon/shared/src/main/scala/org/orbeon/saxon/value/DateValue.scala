
package org.orbeon.saxon.value

import java.time.LocalDate
import java.time.format.DateTimeParseException
import java.time.temporal.WeekFields

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.lib.ConversionRules
import org.orbeon.saxon.model._
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.value.CalendarValue._
import org.orbeon.saxon.value.DateValue._
import org.orbeon.saxon.value.GDateValue._


object DateValue {

  def makeDateValue(in: CharSequence,
                    rules: ConversionRules): ConversionResult = {
    val d: DateValue = new DateValue()
    d.typeLabel = BuiltInAtomicType.DATE
    setLexicalValue(d, in, rules.isAllowYearZero)
  }

  def parse(s: CharSequence): DateValue = {
    val result = makeDateValue(s, ConversionRules.DEFAULT)
    result match {
      case failure: ValidationFailure =>
        throw new DateTimeParseException(
          failure.getMessage,
          s,
          0)
      case _ =>
        result.asInstanceOf[DateValue]
    }
  }

  def tomorrow(year: Int, month: Byte, day: Byte): DateValue =
    if (isValidDate(year, month, day + 1)) {
      new DateValue(year, month, (day + 1).toByte, true)
    } else if (month < 12) {
      new DateValue(year, (month + 1).toByte, 1.toByte, true)
    } else {
      new DateValue(year + 1, 1.toByte, 1.toByte, true)
    }

  def yesterday(year: Int, month: Byte, day: Byte): DateValue =
    if (day > 1) {
      new DateValue(year, month, (day - 1).toByte, true)
    } else if (month > 1) {
      if (month == 3 && isLeapYear(year)) {
        new DateValue(year, 2.toByte, 29.toByte, true)
      } else {
        new DateValue(year, (month - 1).toByte, daysPerMonth(month - 2), true)
      }
    } else {
      new DateValue(year - 1, 12.toByte, 31.toByte, true)
    }

  def getJulianDayNumber(year: Int, month: Int, day: Int): Int = {
    var z: Int = year - (if (month < 3) 1 else 0)
    val f: Short = monthData(month - 1)
    if (z >= 0) {
      day + f + 365 * z + z / 4 - z / 100 + z / 400 + 1721118
    } else {

      z += 12000
      val j: Int = day + f + 365 * z + z / 4 - z / 100 + z / 400 + 1721118

      j -
        (365 * 12000 + 12000 / 4 - 12000 / 100 + 12000 / 400)
    }
  }

  def dateFromJulianDayNumber(julianDayNumber: Int): DateValue =
    if (julianDayNumber >= 0) {

      var L: Int = julianDayNumber + 68569 + 1
      val n: Int = (4 * L) / 146097
      L = L - (146097 * n + 3) / 4
      val i: Int = (4000 * (L + 1)) / 1461001
      L = L - (1461 * i) / 4 + 31
      val j: Int = (80 * L) / 2447
      val d: Int = L - (2447 * j) / 80
      L = j / 11
      val m: Int = j + 2 - (12 * L)
      val y: Int = 100 * (n - 49) + i + L
      new DateValue(y, m.toByte, d.toByte, true)
    } else {

      val dt: DateValue = dateFromJulianDayNumber(
        julianDayNumber + 365 * 12000 + 12000 / 4 - 12000 / 100 +
          12000 / 400)
      dt.year -= 12000
      dt
    }

  def getDayWithinYear(year: Int, month: Int, day: Int): Int = {
    val j: Int = getJulianDayNumber(year, month, day)
    val k: Int = getJulianDayNumber(year, 1, 1)
    j - k + 1
  }

  def getDayOfWeek(year: Int, month: Int, day: Int): Int = {
    var d: Int = getJulianDayNumber(year, month, day)

    d -= 2378500
    while (d <= 0)
      d += 70000000
    (d - 1) % 7 + 1
  }

  def getWeekNumber(year: Int, month: Int, day: Int): Int = {
    val date: LocalDate = LocalDate.of(year, month, day)
    date.get(WeekFields.ISO.weekOfWeekBasedYear())
  }

  def getWeekNumberWithinMonth(year: Int, month: Int, day: Int): Int = {
    val firstDay: Int = getDayOfWeek(year, month, 1)
    if (firstDay > 4 && (firstDay + day) <= 8) {

      val lastDayPrevMonth: DateValue = yesterday(year, month.toByte, 1.toByte)
      getWeekNumberWithinMonth(lastDayPrevMonth.year,
        lastDayPrevMonth.month,
        lastDayPrevMonth.day)
    }

    val inc: Int = if (firstDay < 5) 1 else 0
    ((day + firstDay - 2) / 7) + inc
  }

}

class DateValue extends GDateValue with Comparable[AnyRef] {

  def this(year: Int, month: Byte, day: Byte) = {
    this()
    this.hasNoYearZero = true
    this.year = year
    this.month = month
    this.day = day
    typeLabel = BuiltInAtomicType.DATE
  }

  def this(year: Int, month: Byte, day: Byte, xsd10: Boolean) = {
    this()
    this.hasNoYearZero = xsd10
    this.year = year
    this.month = month
    this.day = day
    typeLabel = BuiltInAtomicType.DATE
  }

  def this(year: Int, month: Byte, day: Byte, tz: Int, xsd10: Boolean) = {
    this()

    this.hasNoYearZero = xsd10
    this.year = year
    this.month = month
    this.day = day
    this.setTimezoneInMinutes(tz)
    typeLabel = BuiltInAtomicType.DATE
  }

  def this(year: Int, month: Byte, day: Byte, tz: Int, `type`: AtomicType) = {
    this()
    this.year = year
    this.month = month
    this.day = day
    this.setTimezoneInMinutes(tz)
    typeLabel = `type`
  }

  def this(s: CharSequence, rules: ConversionRules) = {
    this()
    setLexicalValue(this, s, rules.isAllowYearZero).asAtomic
    typeLabel = BuiltInAtomicType.DATE
  }


  def this(s: CharSequence) {
    this(s, ConversionRules.DEFAULT)
  }

  def this(localDate: LocalDate) =
    this(localDate.getYear,
      localDate.getMonthValue.toByte,
      localDate.getDayOfMonth.toByte)

//  def this(calendar: GregorianCalendar, tz: Int) = {
//    this()
//
//    val era: Int = calendar.get(Calendar.ERA)
//    year = calendar.get(Calendar.YEAR)
//    if (era == GregorianCalendar.BC) {
//      year = 1 - year
//    }
//    month = (calendar.get(Calendar.MONTH) + 1).toByte
//    day = calendar.get(Calendar.DATE).toByte
//    this.setTimezoneInMinutes(tz)
//    typeLabel = BuiltInAtomicType.DATE
//  }

  def getPrimitiveType: BuiltInAtomicType = BuiltInAtomicType.DATE

  def getPrimitiveStringValue(): CharSequence = {
    val sb = new FastStringBuffer(FastStringBuffer.C16)
    var yr: Int = year
    if (year <= 0) {

      yr = -yr + (if (hasNoYearZero) 1 else 0)
      if (yr != 0) {
        sb.cat('-')
      }
    }
    appendString(sb, yr, if (yr > 9999) s"$yr".length else 4)
    sb.cat('-')
    appendTwoDigits(sb, month)
    sb.cat('-')
    appendTwoDigits(sb, day)
    if (hasTimezone) {
      appendTimezone(sb)
    }
    sb
  }

  override def getCanonicalLexicalRepresentation(): CharSequence = {
    var target: DateValue = this
    if (hasTimezone) {
      if (getTimezoneInMinutes > 12 * 60) {
        target = adjustTimezone(getTimezoneInMinutes - 24 * 60)
      } else if (getTimezoneInMinutes <= -12 * 60) {
        target = adjustTimezone(getTimezoneInMinutes + 24 * 60)
      }
    }
    target.getStringValueCS
  }


  def copyAsSubType(typeLabel: AtomicType): AtomicValue = {
    val v: DateValue =
      new DateValue(year, month, day, getTimezoneInMinutes, hasNoYearZero)
    v.typeLabel = typeLabel
    v
  }

  def adjustTimezone(timezone: Int): DateValue = {
    val dt: DateTimeValue = toDateTime.adjustTimezone(timezone)
    new DateValue(dt.getYear,
      dt.getMonth,
      dt.getDay,
      dt.getTimezoneInMinutes,
      hasNoYearZero)
  }

  def add(duration: DurationValue): DateValue =
    duration match {
      case value: DayTimeDurationValue =>
        var microseconds = value.getLengthInMicroseconds
        val negative: Boolean = microseconds < 0
        microseconds = Math.abs(microseconds)
        val days =
          Math.floor(microseconds.toDouble / (1000000L * 60L * 60L * 24L)).toInt
        val partDay = (microseconds % (1000000L * 60L * 60L * 24L)) > 0
        val julian = getJulianDayNumberImpl
        var d = dateFromJulianDayNumber(julian + (if (negative) -days else days))
        if (partDay)
          if (negative)
            d = yesterday(d.year, d.month, d.day)
        d.setTimezoneInMinutes(getTimezoneInMinutes)
        d.hasNoYearZero = this.hasNoYearZero
        d
      case value: YearMonthDurationValue =>
        val months = value.getLengthInMonths
        var m = (month - 1) + months
        var y = year + m / 12
        m = m % 12
        if (m < 0) {
          m += 12
          y -= 1
        }
        m += 1
        var d = day
        while (! isValidDate(y, m, d))
          d = (d - 1).toByte
        new DateValue(y, m.toByte, d, getTimezoneInMinutes, hasNoYearZero)
      case _ =>
        val err = new XPathException("Date arithmetic is not available for xs:duration, only for its subtypes")
        err.setIsTypeError(true)
        err.setErrorCode("XPTY0004")
        throw err
    }

  override def subtract(other: CalendarValue,
                        context: XPathContext): DayTimeDurationValue = {
    if (!(other.isInstanceOf[DateValue])) {
      val err = new XPathException(
        "First operand of '-' is a date, but the second is not")
      err.setIsTypeError(true)
      err.setErrorCode("XPTY0004")
      throw err
    }
    super.subtract(other, context)
  }

  def compareTo(v2: AnyRef): Int =
    compareTo(v2.asInstanceOf[DateValue], MISSING_TIMEZONE)

  def getJulianDayNumberImpl: Int = getJulianDayNumber(year, month, day)

  def toLocalDate: LocalDate = LocalDate.of(getYear, getMonth, getDay)
}
