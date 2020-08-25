package net.sf.saxon.value

import net.sf.saxon.functions.AccessorFn

import net.sf.saxon.model.ConversionResult

import net.sf.saxon.model.ValidationFailure

import net.sf.saxon.om.SequenceTool

import net.sf.saxon.trans.Err

import net.sf.saxon.functions.AccessorFn.Component._

import net.sf.saxon.trans.XPathException

import java.util._

import GDateValue._

object GDateValue {

   var daysPerMonth: Array[Byte] =
    Array(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

   val monthData: Array[Short] =
    Array(306, 337, 0, 31, 61, 92, 122, 153, 184, 214, 245, 275)

   def setLexicalValue(d: GDateValue,
                                s: CharSequence,
                                allowYearZero: Boolean): ConversionResult = {
    d.hasNoYearZero = !allowYearZero
    val tok: StringTokenizer =
      new StringTokenizer(Whitespace.trimWhitespace(s).toString, "-:+TZ", true)
    try {
      if (!tok.hasMoreElements()) {
        badDate("Too short", s)
      }
      var part: String = tok.nextElement().asInstanceOf[String]
      var era: Int = +1
      if ("+" == part) {
        badDate("Date must not start with '+' sign", s)
      } else if ("-" == part) {
        era = -1
        if (!tok.hasMoreElements()) {
          badDate("No year after '-'", s)
        }
        part = tok.nextElement().asInstanceOf[String]
      }
      if (part.length < 4) {
        badDate("Year is less than four digits", s)
      }
      if (part.length > 4 && part.charAt(0) == '0') {
        badDate("When year exceeds 4 digits, leading zeroes are not allowed",
          s)
      }
      var value: Int = DurationValue.simpleInteger(part)
      if (value < 0) {
        if (value == -1) {
          badDate("Non-numeric year component", s)
        } else {
          badDate("Year is outside the range that Saxon can handle",
            s,
            "FODT0001")
        }
      }
      d.year = value * era
      if (d.year == 0 && !allowYearZero) {
        badDate("Year zero is not allowed", s)
      }
      if (era < 0 && !allowYearZero) {
        {
          d.year += 1
        }
      }
      if (!tok.hasMoreElements()) {
        badDate("Too short", s)
      }
      if ("-" != tok.nextElement()) {
        badDate("Wrong delimiter after year", s)
      }
      if (!tok.hasMoreElements()) {
        badDate("Too short", s)
      }
      part = tok.nextElement().asInstanceOf[String]
      if (part.length != 2) {
        badDate("Month must be two digits", s)
      }
      value = DurationValue.simpleInteger(part)
      if (value < 0) {
        badDate("Non-numeric month component", s)
      }
      d.month = value.toByte
      if (d.month < 1 || d.month > 12) {
        badDate("Month is out of range", s)
      }
      if (!tok.hasMoreElements()) {
        badDate("Too short", s)
      }
      if ("-" != tok.nextElement()) {
        badDate("Wrong delimiter after month", s)
      }
      if (!tok.hasMoreElements()) {
        badDate("Too short", s)
      }
      part = tok.nextElement().asInstanceOf[String]
      if (part.length != 2) {
        badDate("Day must be two digits", s)
      }
      value = DurationValue.simpleInteger(part)
      if (value < 0) {
        badDate("Non-numeric day component", s)
      }
      d.day = value.toByte
      if (d.day < 1 || d.day > 31) {
        badDate("Day is out of range", s)
      }
      var tzOffset: Int = 0
      if (tok.hasMoreElements()) {
        val delim: String = tok.nextElement().asInstanceOf[String]
        if ("T" == delim) {
          badDate("Value includes time", s)
        } else if ("Z" == delim) {
          tzOffset = 0
          if (tok.hasMoreElements()) {
            badDate("Continues after 'Z'", s)
          }
          d.setTimezoneInMinutes(tzOffset)
        } else if (!("+" != delim && "-" != delim)) {
          if (!tok.hasMoreElements()) {
            badDate("Missing timezone", s)
          }
          part = tok.nextElement().asInstanceOf[String]
          value = DurationValue.simpleInteger(part)
          if (value < 0) {
            badDate("Non-numeric timezone hour component", s)
          }
          val tzhour: Int = value
          if (part.length != 2) {
            badDate("Timezone hour must be two digits", s)
          }
          if (tzhour > 14) {
            badDate("Timezone hour is out of range", s)
          }
          if (!tok.hasMoreElements()) {
            badDate("No minutes in timezone", s)
          }
          if (":" != tok.nextElement()) {
            badDate("Wrong delimiter after timezone hour", s)
          }
          if (!tok.hasMoreElements()) {
            badDate("No minutes in timezone", s)
          }
          part = tok.nextElement().asInstanceOf[String]
          value = DurationValue.simpleInteger(part)
          if (value < 0) {
            badDate("Non-numeric timezone minute component", s)
          }
          val tzminute: Int = value
          if (part.length != 2) {
            badDate("Timezone minute must be two digits", s)
          }
          if (tzminute > 59) {
            badDate("Timezone minute is out of range", s)
          }
          if (tok.hasMoreElements()) {
            badDate("Continues after timezone", s)
          }
          tzOffset = tzhour * 60 + tzminute
          if ("-" == delim) {
            tzOffset = -tzOffset
          }
          d.setTimezoneInMinutes(tzOffset)
        } else {
          badDate("Timezone format is incorrect", s)
        }
      }
      if (!isValidDate(d.year, d.month, d.day)) {
        badDate("Non-existent date", s)
      }
    } catch {
      case err: NumberFormatException => badDate("Non-numeric component", s)

    }
    d
  }

  private def badDate(msg: String, value: CharSequence): ValidationFailure = {
    val err: ValidationFailure = new ValidationFailure(
      "Invalid date " + Err.wrap(value, Err.VALUE) + " (" +
        msg +
        ")")
    err.setErrorCode("FORG0001")
    err
  }

  private def badDate(msg: String,
                      value: CharSequence,
                      errorCode: String): ValidationFailure = {
    val err: ValidationFailure = new ValidationFailure(
      "Invalid date " + Err.wrap(value, Err.VALUE) + " (" +
        msg +
        ")")
    err.setErrorCode(errorCode)
    err
  }

  def isValidDate(year: Int, month: Int, day: Int): Boolean =
    month > 0 && month <= 12 && day > 0 && day <= daysPerMonth(month - 1) ||
      month == 2 && day == 29 && isLeapYear(year)

  def isLeapYear(year: Int): Boolean =
    (year % 4 == 0) && !(year % 100 == 0 && !(year % 400 == 0))

}

abstract class GDateValue extends CalendarValue {

   var year: Int = _

   var month: Byte = _

   var day: Byte = _

  var hasNoYearZero: Boolean = _

  def getYear(): Int = year

  def getMonth(): Byte = month

  def getDay(): Byte = day

  def getCalendar(): GregorianCalendar = {
    val tz: Int = if (hasTimezone()) getTimezoneInMinutes * 60000 else 0
    val zone: TimeZone = new SimpleTimeZone(tz, "LLL")
    val calendar: GregorianCalendar = new GregorianCalendar(zone)
    calendar.setGregorianChange(new Date(java.lang.Long.MIN_VALUE))
    if (tz < calendar.getMinimum(Calendar.ZONE_OFFSET) || tz > calendar
      .getMaximum(Calendar.ZONE_OFFSET)) {
      adjustTimezone(0).getCalendar
    }
    calendar.clear()
    calendar.setLenient(false)
    var yr: Int = year
    if (year <= 0) {
      yr = if (hasNoYearZero) 1 - year else 0 - year
      calendar.set(Calendar.ERA, GregorianCalendar.BC)
    }
    calendar.set(yr, month - 1, day)
    calendar.set(Calendar.ZONE_OFFSET, tz)
    calendar.set(Calendar.DST_OFFSET, 0)
    calendar.getTime
    calendar
  }

  override def checkValidInJavascript(): Unit = {
    if (year <= 0 || year > 9999) {
      throw new XPathException("Year out of range for Saxon-JS", "FODT0001")
    }
  }

  override def equals(o: Any): Boolean = o match {
    case o: GDateValue => {
      val gdv: GDateValue = o
      getPrimitiveType == gdv.getPrimitiveType && toDateTime() == gdv
        .toDateTime()
    }
    case _ => false

  }

  override def hashCode(): Int =
    DateTimeValue.hashCode(year,
      month,
      day,
      12.toByte,
      0.toByte,
      0.toByte,
      0,
      getTimezoneInMinutes)

  def compareTo(other: CalendarValue, implicitTimezone: Int): Int = {
    if (getPrimitiveType != other.getPrimitiveType) {
      throw new ClassCastException("Cannot compare dates of different types")
    }
    val v2: GDateValue = other.asInstanceOf[GDateValue]
    if (getTimezoneInMinutes == other.getTimezoneInMinutes) {
      if (year != v2.year) {
        IntegerValue.signum(year - v2.year)
      }
      if (month != v2.month) {
        IntegerValue.signum(month - v2.month)
      }
      if (day != v2.day) {
        IntegerValue.signum(day - v2.day)
      }
      return 0
    }
    toDateTime().compareTo(other.toDateTime(), implicitTimezone)
  }

  def toDateTime(): DateTimeValue =
    new DateTimeValue(year,
      month,
      day,
      0.toByte,
      0.toByte,
      0.toByte,
      0,
      getTimezoneInMinutes,
      hasNoYearZero)

  def getSchemaComparable(): Comparable[AnyRef] = new GDateComparable()

  override def getComponent(component: AccessorFn.Component.Component): AtomicValue =
    component match {
      case YEAR_ALLOWING_ZERO => Int64Value.makeIntegerValue(year)
      case YEAR =>
        Int64Value.makeIntegerValue(
          if (year > 0 || !hasNoYearZero) year else year - 1)
      case MONTH => Int64Value.makeIntegerValue(month)
      case DAY => Int64Value.makeIntegerValue(day)
      case TIMEZONE =>
        if (hasTimezone()) {
          DayTimeDurationValue.fromMilliseconds(60000L * getTimezoneInMinutes)
        } else {
          null
        }
      case _ =>
        throw new IllegalArgumentException(
          "Unknown component for date: " + component)

    }

   class GDateComparable extends Comparable[AnyRef] {

    def asGDateValue(): GDateValue = GDateValue.this

    def compareTo(o: AnyRef): Int =
      if (o.isInstanceOf[GDateComparable]) {
        if (asGDateValue().getPrimitiveType !=
          o.asInstanceOf[GDateComparable]
            .asGDateValue()
            .getPrimitiveType) {
          SequenceTool.INDETERMINATE_ORDERING
        }
        val dt0: DateTimeValue = GDateValue.this.toDateTime()
        val dt1: DateTimeValue =
          o.asInstanceOf[GDateComparable].asGDateValue().toDateTime()
        dt0.getSchemaComparable.compareTo(dt1.getSchemaComparable.asInstanceOf)
      } else {
        SequenceTool.INDETERMINATE_ORDERING
      }

    override def equals(o: Any): Boolean = compareTo(o.asInstanceOf[AnyRef]) == 0

    override def hashCode(): Int =
      GDateValue.this.toDateTime().getSchemaComparable.hashCode

  }

}
