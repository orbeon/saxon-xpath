package org.orbeon.saxon.value

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.functions.AccessorFn.Component._
import org.orbeon.saxon.lib.ConversionRules
import org.orbeon.saxon.model.{AtomicType, BuiltInAtomicType, ConversionResult, ValidationFailure}
import org.orbeon.saxon.om.SequenceTool
import org.orbeon.saxon.trans.{Err, NoDynamicContextException, XPathException}
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.value.CalendarValue._
import org.orbeon.saxon.value.DateTimeValue._
import org.orbeon.saxon.value.GDateValue._

import java.math.{BigDecimal, RoundingMode}
import java.time._
import java.time.format.DateTimeParseException
import java.time.temporal.{ChronoField, TemporalAccessor, TemporalField, UnsupportedTemporalTypeException}
import java.util._
import scala.beans.BeanProperty


object DateTimeValue {

  def getCurrentDateTime(context: XPathContext): DateTimeValue = {
    val c = context.getController
    if (context == null || c == null)
      now()
    else
      c.getCurrentDateTime
  }

  def now(): DateTimeValue =
    DateTimeValue.fromZonedDateTime(ZonedDateTime.now())

  def fromJavaDate(suppliedDate: Date): DateTimeValue = {
    val millis = suppliedDate.getTime
    EPOCH.add(DayTimeDurationValue.fromMilliseconds(millis))
  }

  def fromJavaTime(time: Long): DateTimeValue =
    EPOCH.add(DayTimeDurationValue.fromMilliseconds(time))

  def fromJavaInstant(seconds: Long, nano: Int): DateTimeValue =
    EPOCH.add(
      DayTimeDurationValue
        .fromSeconds(new BigDecimal(seconds))
        .add(DayTimeDurationValue.fromNanoseconds(nano)))

  def fromJavaInstant(instant: Instant): DateTimeValue =
    fromJavaInstant(instant.getEpochSecond, instant.getNano)

  def fromZonedDateTime(zonedDateTime: ZonedDateTime): DateTimeValue =
    fromOffsetDateTime(zonedDateTime.toOffsetDateTime)

  def fromOffsetDateTime(offsetDateTime: OffsetDateTime): DateTimeValue = {
    val ldt = offsetDateTime.toLocalDateTime
    val zo  = offsetDateTime.getOffset
    val tz  = zo.getTotalSeconds / 60

    val dtv = new DateTimeValue(
      ldt.getYear,
      ldt.getMonthValue.toByte,
      ldt.getDayOfMonth.toByte,
      ldt.getHour.toByte,
      ldt.getMinute.toByte,
      ldt.getSecond.toByte,
      ldt.getNano,
      tz
    )
    dtv.typeLabel = BuiltInAtomicType.DATE_TIME_STAMP
    dtv.hasNoYearZero = false
    dtv
  }

  def fromLocalDateTime(localDateTime: LocalDateTime): DateTimeValue = {
    val dtv = new DateTimeValue(
      localDateTime.getYear,
      localDateTime.getMonthValue.toByte,
      localDateTime.getDayOfMonth.toByte,
      localDateTime.getHour.toByte,
      localDateTime.getMinute.toByte,
      localDateTime.getSecond.toByte,
      localDateTime.getNano,
      NO_TIMEZONE
    )
    dtv.hasNoYearZero = false
    dtv
  }

  val EPOCH: DateTimeValue = new DateTimeValue(1970,
    1.toByte,
    1.toByte,
    0.toByte,
    0.toByte,
    0.toByte,
    0,
    0,
    true)

  def makeDateTimeValue(date: DateValue, time: TimeValue): DateTimeValue = {
    if (date == null || time == null)
      return null
    val tz1 = date.getTimezoneInMinutes
    val tz2 = time.getTimezoneInMinutes
    if (tz1 != NO_TIMEZONE && tz2 != NO_TIMEZONE && tz1 != tz2) {
      val err = new XPathException("Supplied date and time are in different timezones")
      err.setErrorCode("FORG0008")
      throw err
    }
    val v = date.toDateTime
    v.hour = time.getHour
    v.minute = time.getMinute
    v.second = time.getSecond
    v.nanosecond = time.getNanosecond
    v.setTimezoneInMinutes(Math.max(tz1, tz2))
    v.typeLabel = BuiltInAtomicType.DATE_TIME
    v.hasNoYearZero = date.hasNoYearZero
    v
  }

  def makeDateTimeValue(s: CharSequence,
                        rules: ConversionRules): ConversionResult = {
    val dt = new DateTimeValue
    dt.hasNoYearZero = !rules.isAllowYearZero
    val tok = new StringTokenizer(
      Whitespace.trimWhitespace(s).toString,
      "-:.+TZ",
      true)
    if (!tok.hasMoreElements)
      return badDate("too short", s)
    var part = tok.nextElement().asInstanceOf[String]
    var era  = +1
    if ("+" == part) {
      return badDate("Date must not start with '+' sign", s)
    } else if ("-" == part) {
      era = -1
      if (!tok.hasMoreElements)
        return badDate("No year after '-'", s)
      part = tok.nextElement().asInstanceOf[String]
    }
    var value = DurationValue.simpleInteger(part)
    if (value < 0) {
      if (value == -1)
        return badDate("Non-numeric year component", s)
      else
        return badDate("Year is outside the range that Saxon can handle", s, "FODT0001")
    }
    dt.year = value * era
    if (part.length < 4)
      return badDate("Year is less than four digits", s)
    if (part.length > 4 && part.charAt(0) == '0')
      return badDate("When year exceeds 4 digits, leading zeroes are not allowed", s)
    if (dt.year == 0 && !rules.isAllowYearZero)
      return badDate("Year zero is not allowed", s)
    if (era < 0 && !rules.isAllowYearZero)
      dt.year += 1
    if (!tok.hasMoreElements)
      return badDate("Too short", s)
    if ("-" != tok.nextElement())
      return badDate("Wrong delimiter after year", s)
    if (!tok.hasMoreElements)
      return badDate("Too short", s)
    part = tok.nextElement().asInstanceOf[String]
    if (part.length != 2)
      return badDate("Month must be two digits", s)
    value = DurationValue.simpleInteger(part)
    if (value < 0)
      return badDate("Non-numeric month component", s)
    dt.month = value.toByte
    if (dt.month < 1 || dt.month > 12)
      return badDate("Month is out of range", s)
    if (!tok.hasMoreElements)
      return badDate("Too short", s)
    if ("-" != tok.nextElement())
      return badDate("Wrong delimiter after month", s)
    if (!tok.hasMoreElements)
      return badDate("Too short", s)
    part = tok.nextElement().asInstanceOf[String]
    if (part.length != 2)
      return badDate("Day must be two digits", s)
    value = DurationValue.simpleInteger(part)
    if (value < 0)
      return badDate("Non-numeric day component", s)
    dt.day = value.toByte
    if (dt.day < 1 || dt.day > 31)
      return badDate("Day is out of range", s)
    if (!tok.hasMoreElements)
      return badDate("Too short", s)
    if ("T" != tok.nextElement())
      return badDate("Wrong delimiter after day", s)
    if (!tok.hasMoreElements)
      return badDate("Too short", s)
    part = tok.nextElement().asInstanceOf[String]
    if (part.length != 2)
      return badDate("Hour must be two digits", s)
    value = DurationValue.simpleInteger(part)
    if (value < 0)
      return badDate("Non-numeric hour component", s)
    dt.hour = value.toByte
    if (dt.hour > 24)
      return badDate("Hour is out of range", s)
    if (!tok.hasMoreElements)
      return badDate("Too short", s)
    if (":" != tok.nextElement())
      return badDate("Wrong delimiter after hour", s)
    if (!tok.hasMoreElements)
      return badDate("Too short", s)
    part = tok.nextElement().asInstanceOf[String]
    if (part.length != 2)
      return badDate("Minute must be two digits", s)
    value = DurationValue.simpleInteger(part)
    if (value < 0)
      return badDate("Non-numeric minute component", s)
    dt.minute = value.toByte
    if (dt.minute > 59)
      return badDate("Minute is out of range", s)
    if (dt.hour == 24 && dt.minute != 0)
      return badDate("If hour is 24, minute must be 00", s)
    if (!tok.hasMoreElements)
      return badDate("Too short", s)
    if (":" != tok.nextElement())
      return badDate("Wrong delimiter after minute", s)
    if (!tok.hasMoreElements)
      return badDate("Too short", s)
    part = tok.nextElement().asInstanceOf[String]
    if (part.length != 2)
      return badDate("Second must be two digits", s)
    value = DurationValue.simpleInteger(part)
    if (value < 0)
      return badDate("Non-numeric second component", s)
    dt.second = value.toByte
    if (dt.second > 59)
      return badDate("Second is out of range", s)
    if (dt.hour == 24 && dt.second != 0)
      return badDate("If hour is 24, second must be 00", s)

    var tz         = 0
    var negativeTz = false
    var state      = 0

    while (tok.hasMoreElements) {
      if (state == 9)
        return badDate("Characters after the end", s)
      val delim = tok.nextElement().asInstanceOf[String]
      if ("." == delim) {
        if (state != 0)
          return badDate("Decimal separator occurs twice", s)
        if (!tok.hasMoreElements)
          return badDate("Decimal point must be followed by digits", s)
        part = tok.nextElement().asInstanceOf[String]
        if (part.length > 9 && part.matches("^[0-9]+$"))
          part = part.substring(0, 9)
        value = DurationValue.simpleInteger(part)
        if (value < 0)
          return badDate("Non-numeric fractional seconds component", s)
        val fractionalSeconds: Double =
          java.lang.Double.parseDouble("." + part)
        var nanoSeconds: Int = Math.round(fractionalSeconds * 1000000000).toInt
        if (nanoSeconds == 1000000000)
          nanoSeconds -= 1
        dt.nanosecond = nanoSeconds
        if (dt.hour == 24 && dt.nanosecond != 0) {
          return badDate("If hour is 24, fractional seconds must be 0", s)
        }
        state = 1
      } else if ("Z" == delim) {
        if (state > 1)
          return badDate("Z cannot occur here", s)
        tz = 0
        state = 9
        dt.setTimezoneInMinutes(0)
      } else if ("+" == delim || "-" == delim) {
        if (state > 1)
          return badDate(delim + " cannot occur here", s)
        state = 2
        if (!tok.hasMoreElements)
          return badDate("Missing timezone", s)
        part = tok.nextElement().asInstanceOf[String]
        if (part.length != 2)
          return badDate("Timezone hour must be two digits", s)
        value = DurationValue.simpleInteger(part)
        if (value < 0)
          return badDate("Non-numeric timezone hour component", s)
        tz = value
        if (tz > 14)
          return badDate("Timezone is out of range (-14:00 to +14:00)", s)
        tz *= 60
        if ("-" == delim)
          negativeTz = true
      } else if (":" == delim) {
        if (state != 2)
          return badDate("Misplaced ':'", s)
        state = 9
        part = tok.nextElement().asInstanceOf[String]
        value = DurationValue.simpleInteger(part)
        if (value < 0)
          return badDate("Non-numeric timezone minute component", s)
        val tzminute: Int = value
        if (part.length != 2)
          return badDate("Timezone minute must be two digits", s)
        if (tzminute > 59)
          return badDate("Timezone minute is out of range", s)
        if (Math.abs(tz) == 14 * 60 && tzminute != 0)
          return badDate("Timezone is out of range (-14:00 to +14:00)", s)
        tz += tzminute
        if (negativeTz)
          tz = -tz
        dt.setTimezoneInMinutes(tz)
      } else {
        return badDate("Timezone format is incorrect", s)
      }
    }
    if (state == 2 || state == 3)
      return badDate("Timezone incomplete", s)
    var midnight = false
    if (dt.hour == 24) {
      dt.hour = 0
      midnight = true
    }
    if (!isValidDate(dt.year, dt.month, dt.day))
      return badDate("Non-existent date", s)
    if (midnight) {
      val t = DateValue.tomorrow(dt.year, dt.month, dt.day)
      dt.year = t.getYear
      dt.month = t.getMonth
      dt.day = t.getDay
    }
    dt.typeLabel = BuiltInAtomicType.DATE_TIME
    dt
  }

  def parse(s: CharSequence): DateTimeValue = {
    val result: ConversionResult =
      makeDateTimeValue(s, ConversionRules.DEFAULT)
    result match {
      case failure: ValidationFailure =>
        throw new DateTimeParseException(
          failure.getMessage,
          s,
          0
        )
      case _                          =>
        result.asInstanceOf[DateTimeValue]
    }
  }

  private def badDate(msg: String, value: CharSequence): ValidationFailure = {
    val err = new ValidationFailure(
      "Invalid dateTime value " + Err.wrap(value, Err.VALUE) +
        " (" +
        msg +
        ")")
    err.setErrorCode("FORG0001")
    err
  }

  private def badDate(
    msg       : String,
    value     : CharSequence,
    errorCode : String
  ): ValidationFailure = {
    val err = new ValidationFailure(
      "Invalid dateTime value " + Err.wrap(value, Err.VALUE) +
        " (" +
        msg +
        ")")
    err.setErrorCode(errorCode)
    err
  }

  def fromJulianInstant(instant: BigDecimal): DateTimeValue = {
    val julianSecond = instant.toBigInteger
    val nanoseconds  = instant
      .subtract(new BigDecimal(julianSecond))
      .multiply(BigDecimalValue.BIG_DECIMAL_ONE_BILLION)
    var js = julianSecond.longValue
    val jd              = js / (24L * 60L * 60L)
    val date            = DateValue.dateFromJulianDayNumber(jd.toInt)
    js = js % (24L * 60L * 60L)
    val hour = (js / (60L * 60L)).toByte
    js = js % (60L * 60L)
    val minute = (js / 60L).toByte
    js = js % 60L
    new DateTimeValue(date.getYear,
      date.getMonth,
      date.getDay,
      hour,
      minute,
      js.toByte,
      nanoseconds.intValue(),
      0)
  }

  def hashCode(year: Int,
               month: Byte,
               day: Byte,
               hour: Byte,
               minute: Byte,
               second: Byte,
               nanosecond: Int,
               tzMinutes: Int): Int = {
    var lYear = year
    var lMonth = month
    var lDay = day

    val tz: Int = if (tzMinutes == CalendarValue.NO_TIMEZONE) 0 else -tzMinutes
    var h: Int = hour
    var mi: Int = minute
    mi += tz
    if (mi < 0 || mi > 59) {
      h = h + Math.floor(mi / 60.0).toInt
      mi = (mi + 60 * 24) % 60
    }
    while (h < 0) {
      h += 24
      val t: DateValue = DateValue.yesterday(lYear, lMonth, lDay)
      lYear = t.getYear
      lMonth = t.getMonth
      lDay = t.getDay
    }
    while (h > 23) {
      h -= 24
      val t: DateValue = DateValue.tomorrow(lYear, lMonth, lDay)
      lYear = t.getYear
      lMonth = t.getMonth
      lDay = t.getDay
    }
    (lYear << 4) ^ (lMonth << 28) ^ (lDay << 23) ^ (h << 18) ^
      (mi << 13) ^
      second ^
      nanosecond
  }

}

class DateTimeValue extends CalendarValue
  with Comparable[AnyRef]
  with TemporalAccessor {

  @BeanProperty
  var year: Int = _
  @BeanProperty
  var month: Byte = _
  @BeanProperty
  var day: Byte = _
  @BeanProperty
  var hour: Byte = _
  @BeanProperty
  var minute: Byte = _
  @BeanProperty
  var second: Byte = _
  @BeanProperty
  var nanosecond: Int = _
  private var hasNoYearZero: Boolean = _


//  def this(calendar: Calendar, tzSpecified: Boolean) = {
//    this()
//    val era: Int = calendar.get(Calendar.ERA)
//    year = calendar.get(Calendar.YEAR)
//    if (era == GregorianCalendar.BC) {
//      year = 1 - year
//    }
//    month = (calendar.get(Calendar.MONTH) + 1).toByte
//    day = calendar.get(Calendar.DATE).toByte
//    hour = calendar.get(Calendar.HOUR_OF_DAY).toByte
//    minute = calendar.get(Calendar.MINUTE).toByte
//    second = calendar.get(Calendar.SECOND).toByte
//    nanosecond = calendar.get(Calendar.MILLISECOND) * 1000000
//    if (tzSpecified) {
//      val tz: Int = (calendar.get(Calendar.ZONE_OFFSET) + calendar.get(
//        Calendar.DST_OFFSET)) /
//        60000
//      this.setTimezoneInMinutes(tz)
//    }
//    typeLabel = BuiltInAtomicType.DATE_TIME
//    hasNoYearZero = true
//  }

  def this(year: Int,
           month: Byte,
           day: Byte,
           hour: Byte,
           minute: Byte,
           second: Byte,
           nanosecond: Int,
           tz: Int) = {
    this()
    this.hasNoYearZero = false
    this.year = year
    this.month = month
    this.day = day
    this.hour = hour
    this.minute = minute
    this.second = second
    this.nanosecond = nanosecond
    this.setTimezoneInMinutes(tz)
    typeLabel = BuiltInAtomicType.DATE_TIME
  }

  def this(year: Int,
           month: Byte,
           day: Byte,
           hour: Byte,
           minute: Byte,
           second: Byte,
           microsecond: Int,
           tz: Int,
           hasNoYearZero: Boolean) = {
    this()
    this.hasNoYearZero = hasNoYearZero
    this.year = year
    this.month = month
    this.day = day
    this.hour = hour
    this.minute = minute
    this.second = second
    this.nanosecond = microsecond * 1000
    this.setTimezoneInMinutes(tz)
    typeLabel = BuiltInAtomicType.DATE_TIME
  }

  def getPrimitiveType : BuiltInAtomicType = BuiltInAtomicType.DATE_TIME
  def getMicrosecond   : Int               = nanosecond / 1000
  def toDateTime       : DateTimeValue     = this
  def isXsd10Rules     : Boolean           = hasNoYearZero

  override def checkValidInJavascript(): Unit = {
    if (year <= 0 || year > 9999) {
      throw new XPathException("Year out of range for Saxon-JS", "FODT0001")
    }
  }

  def adjustToUTC(implicitTimezone: Int): DateTimeValue =
    if (hasTimezone) {
      adjustTimezone(0)
    } else {
      if (implicitTimezone == CalendarValue.MISSING_TIMEZONE || implicitTimezone == CalendarValue.NO_TIMEZONE)
        throw new NoDynamicContextException("DateTime operation needs access to implicit timezone")
      val dt = copyAsSubType(null)
      dt.setTimezoneInMinutes(implicitTimezone)
      dt.adjustTimezone(0)
    }

  def toJulianInstant: BigDecimal = {
    val julianDay = DateValue.getJulianDayNumber(year, month, day)
    var julianSecond = julianDay * 24L * 60L * 60L
    julianSecond += ((hour * 60L + minute) * 60L) + second
    val j = BigDecimal.valueOf(julianSecond)
    if (nanosecond == 0) {
      j
    } else {
      j.add(
        BigDecimal
          .valueOf(nanosecond)
          .divide(BigDecimalValue.BIG_DECIMAL_ONE_BILLION,
            9,
            RoundingMode.HALF_EVEN))
    }
  }

//  def getCalendar: GregorianCalendar = {
//    val tz = if (hasTimezone) getTimezoneInMinutes * 60000 else 0
//    val zone = new SimpleTimeZone(tz, "LLL")
//
//    val calendar = new GregorianCalendar(zone)
//    if (tz < calendar.getMinimum(Calendar.ZONE_OFFSET) || tz > calendar.getMaximum(Calendar.ZONE_OFFSET))
//      return adjustTimezone(0).getCalendar
//
//    calendar.setGregorianChange(new Date(java.lang.Long.MIN_VALUE))
//    calendar.setLenient(false)
//    var yr: Int = year
//    if (year <= 0) {
//      yr = if (hasNoYearZero) 1 - year else 0 - year
//      calendar.set(Calendar.ERA, GregorianCalendar.BC)
//    }
//    calendar.set(yr, month - 1, day, hour, minute, second)
//    calendar.set(Calendar.MILLISECOND, nanosecond / 1000000)
//    calendar.set(Calendar.ZONE_OFFSET, tz)
//    calendar.set(Calendar.DST_OFFSET, 0)
//    calendar
//  }

  def toJavaInstant: Instant = Instant.from(this)

  def toZonedDateTime: ZonedDateTime =
    if (hasTimezone)
      ZonedDateTime.from(this)
    else
      ZonedDateTime.from(adjustToUTC(0))

  def toOffsetDateTime: OffsetDateTime =
    if (hasTimezone)
      OffsetDateTime.from(this)
    else
      OffsetDateTime.from(adjustToUTC(0))

  def toLocalDateTime: LocalDateTime = LocalDateTime.from(this)

  def getPrimitiveStringValue: CharSequence = {
    val sb = new FastStringBuffer(30)
    var yr = year
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
    sb.cat('T')
    appendTwoDigits(sb, hour)
    sb.cat(':')
    appendTwoDigits(sb, minute)
    sb.cat(':')
    appendTwoDigits(sb, second)
    if (nanosecond != 0) {
      sb.cat('.')
      var ms: Int = nanosecond
      var div: Int = 100000000
      while (ms > 0) {
        val d: Int = ms / div
        sb.cat((d + '0').toChar)
        ms = ms % div
        div /= 10
      }
    }
    if (hasTimezone)
      appendTimezone(sb)
    sb
  }

  def toDateValue: DateValue =
    new DateValue(year, month, day, getTimezoneInMinutes, hasNoYearZero)

  def toTimeValue: TimeValue =
    new TimeValue(hour, minute, second, nanosecond, getTimezoneInMinutes, "")

  override def getCanonicalLexicalRepresentation: CharSequence =
    if (hasTimezone && getTimezoneInMinutes != 0)
      adjustTimezone(0).getStringValueCS
    else
      getStringValueCS

  def copyAsSubType(typeLabel: AtomicType): DateTimeValue = {
    val v = new DateTimeValue(year,
      month,
      day,
      hour,
      minute,
      second,
      nanosecond,
      getTimezoneInMinutes)
    v.hasNoYearZero = hasNoYearZero
    v.typeLabel = typeLabel
    v
  }

  def adjustTimezone(timezone: Int): DateTimeValue = {
    if (!hasTimezone) {
      val in = copyAsSubType(typeLabel)
      in.setTimezoneInMinutes(timezone)
      return in
    }
    val oldtz = getTimezoneInMinutes
    if (oldtz == timezone)
      return this
    val tz = timezone - oldtz
    var h: Int       = hour
    var mi: Int      = minute
    mi += tz
    if (mi < 0 || mi > 59) {
      h = h + Math.floor(mi / 60.0).toInt
      mi = (mi + 60 * 24) % 60
    }
    if (h >= 0 && h < 24) {
      val d2 = new DateTimeValue(year,
        month,
        day,
        h.toByte,
        mi.toByte,
        second,
        nanosecond,
        timezone)
      d2.hasNoYearZero = hasNoYearZero
      return d2
    }
    var dt: DateTimeValue = this
    while (h < 0) {
      h += 24
      val t = DateValue.yesterday(dt.getYear, dt.getMonth, dt.getDay)
      dt = new DateTimeValue(t.getYear,
        t.getMonth,
        t.getDay,
        h.toByte,
        mi.toByte,
        second,
        nanosecond,
        timezone)
      dt.hasNoYearZero = hasNoYearZero
    }
    if (h > 23) {
      h -= 24
      val t = DateValue.tomorrow(year, month, day)
      dt = new DateTimeValue(t.getYear,
        t.getMonth,
        t.getDay,
        h.toByte,
        mi.toByte,
        second,
        nanosecond,
        timezone)
      dt.hasNoYearZero = hasNoYearZero
    }
    dt
  }

  def add(duration: DurationValue): DateTimeValue =
    duration match {
      case dayTimeDurationValue: DayTimeDurationValue     =>
        val seconds = dayTimeDurationValue.getTotalSeconds
        var julian = toJulianInstant
        julian = julian.add(seconds)
        val dt = fromJulianInstant(julian)
        dt.setTimezoneInMinutes(getTimezoneInMinutes)
        dt.hasNoYearZero = this.hasNoYearZero
        dt
      case yearMonthDurationValue: YearMonthDurationValue =>
        val months = yearMonthDurationValue.getLengthInMonths
        var m = (month - 1) + months
        var y = year + m / 12
        m = m % 12
        if (m < 0) {
          m += 12
          y -= 1
        }
        m += 1
        var d: Int = day
        while (!isValidDate(y, m, d))
          d -= 1
        val dtv = new DateTimeValue(
          y,
          m.toByte,
          d.toByte,
          hour,
          minute,
          second,
          nanosecond,
          getTimezoneInMinutes
        )
        dtv.hasNoYearZero = hasNoYearZero
        dtv
      case _                             =>
        val err = new XPathException(
          "DateTime arithmetic is not supported on xs:duration, only on its subtypes"
        )
        err.setErrorCode("XPTY0004")
        err.setIsTypeError(true)
        throw err
    }

  override def subtract(other: CalendarValue,
                        context: XPathContext): DayTimeDurationValue = {
    if (! other.isInstanceOf[DateTimeValue]) {
      val err = new XPathException("First operand of '-' is a dateTime, but the second is not")
      err.setErrorCode("XPTY0004")
      err.setIsTypeError(true)
      throw err
    }
    super.subtract(other, context)
  }

  def secondsSinceEpoch: BigDecimal = {
    val dtv = adjustToUTC(0)
    val d1 = dtv.toJulianInstant
    val d2 = EPOCH.toJulianInstant
    d1.subtract(d2)
  }

  override def getComponent(component: Component): AtomicValue =
    component match {
      case YEAR_ALLOWING_ZERO => Int64Value.makeIntegerValue(year)
      case YEAR               => Int64Value.makeIntegerValue(if (year > 0 || !hasNoYearZero) year else year - 1)
      case MONTH              => Int64Value.makeIntegerValue(month)
      case DAY                => Int64Value.makeIntegerValue(day)
      case HOURS              => Int64Value.makeIntegerValue(hour)
      case MINUTES            => Int64Value.makeIntegerValue(minute)
      case SECONDS =>
        var d: BigDecimal = BigDecimal.valueOf(nanosecond)
        d = d.divide(BigDecimalValue.BIG_DECIMAL_ONE_BILLION,
          6,
          BigDecimal.ROUND_HALF_UP)
        d = d.add(BigDecimal.valueOf(second))
        new BigDecimalValue(d)
      case WHOLE_SECONDS      => Int64Value.makeIntegerValue(second)
      case MICROSECONDS       => new Int64Value(nanosecond / 1000)
      case NANOSECONDS        => new Int64Value(nanosecond)
      case TIMEZONE =>
        if (hasTimezone)
          DayTimeDurationValue.fromMilliseconds(60000L * getTimezoneInMinutes)
        else
          null
      case _ =>
        throw new IllegalArgumentException("Unknown component for dateTime: " + component)
    }

  def isSupported(field: TemporalField): Boolean =
    if (field == ChronoField.OFFSET_SECONDS)
      getTimezoneInMinutes != NO_TIMEZONE
    else if (field.isInstanceOf[ChronoField])
      true
    else
      field.isSupportedBy(this)

  def getLong(field: TemporalField): Long =
    field match {
      case chronoField: ChronoField =>
        chronoField match {
          case ChronoField.NANO_OF_SECOND               => nanosecond
          case ChronoField.NANO_OF_DAY                  => (hour * 3600 + minute * 60 + second) * 1000000000L + nanosecond
          case ChronoField.MICRO_OF_SECOND              => nanosecond / 1000
          case ChronoField.MICRO_OF_DAY                 => (hour * 3600 + minute * 60 + second) * 1000000L + (nanosecond / 1000)
          case ChronoField.MILLI_OF_SECOND              => nanosecond / 1000000
          case ChronoField.MILLI_OF_DAY                 => (hour * 3600 + minute * 60 + second) * 1000L + nanosecond / 1000000
          case ChronoField.SECOND_OF_MINUTE             => second
          case ChronoField.SECOND_OF_DAY                => hour * 3600 + minute * 60 + second
          case ChronoField.MINUTE_OF_HOUR               => minute
          case ChronoField.MINUTE_OF_DAY                => hour * 60 + minute
          case ChronoField.HOUR_OF_AMPM                 => hour % 12
          case ChronoField.CLOCK_HOUR_OF_AMPM           => (hour + 11) % 12 + 1
          case ChronoField.HOUR_OF_DAY                  => hour
          case ChronoField.CLOCK_HOUR_OF_DAY            => (hour + 23) % 24 + 1
          case ChronoField.AMPM_OF_DAY                  => hour / 12
          case ChronoField.DAY_OF_WEEK                  => DateValue.getDayOfWeek(year, month, day)
          case ChronoField.ALIGNED_DAY_OF_WEEK_IN_MONTH => (day - 1) % 7 + 1
          case ChronoField.ALIGNED_DAY_OF_WEEK_IN_YEAR  => (DateValue.getDayWithinYear(year, month, day) - 1) % 7 + 1
          case ChronoField.DAY_OF_MONTH                 => day
          case ChronoField.DAY_OF_YEAR                  => DateValue.getDayWithinYear(year, month, day)
          case ChronoField.EPOCH_DAY =>
            val secs = secondsSinceEpoch
            val days = secondsSinceEpoch.longValue / (24 * 60 * 60)
            if (secs.signum() < 0) days - 1 else days
          case ChronoField.ALIGNED_WEEK_OF_MONTH        => (day - 1) / 7 + 1
          case ChronoField.ALIGNED_WEEK_OF_YEAR         => (DateValue.getDayWithinYear(year, month, day) - 1) / 7 + 1
          case ChronoField.MONTH_OF_YEAR                => month
          case ChronoField.PROLEPTIC_MONTH              => year * 12 + month - 1
          case ChronoField.YEAR_OF_ERA                  => Math.abs(year) + (if (year < 0) 1 else 0)
          case ChronoField.YEAR                         => year
          case ChronoField.ERA                          => if (year < 0) 0 else 1
          case ChronoField.INSTANT_SECONDS              => secondsSinceEpoch.setScale(0, BigDecimal.ROUND_FLOOR).longValue
          case ChronoField.OFFSET_SECONDS               =>
            val tz = getTimezoneInMinutes
            if (tz == NO_TIMEZONE)
              throw new UnsupportedTemporalTypeException("xs:dateTime value has no timezone")
            else
              tz * 60
          case _ => throw new UnsupportedTemporalTypeException(field.toString)
        }
      case _ =>
        field.getFrom(this)
    }

  def compareTo(other: CalendarValue, implicitTimezone: Int): Int = {
    if (! other.isInstanceOf[DateTimeValue])
      throw new ClassCastException("DateTime values are not comparable to " + other.getClass)
    val v2 = other.asInstanceOf[DateTimeValue]
    if (getTimezoneInMinutes == v2.getTimezoneInMinutes) {
      if (year != v2.year)
        IntegerValue.signum(year - v2.year)
      else if (month != v2.month)
        IntegerValue.signum(month - v2.month)
      else if (day != v2.day)
        IntegerValue.signum(day - v2.day)
      else if (hour != v2.hour)
        IntegerValue.signum(hour - v2.hour)
      else if (minute != v2.minute)
        IntegerValue.signum(minute - v2.minute)
      else if (second != v2.second)
        IntegerValue.signum(second - v2.second)
      else if (nanosecond != v2.nanosecond)
        IntegerValue.signum(nanosecond - v2.nanosecond)
      else
        0
    } else
      adjustToUTC(implicitTimezone).compareTo(v2.adjustToUTC(implicitTimezone), implicitTimezone)
  }

  def compareTo(v2: AnyRef): Int =
    compareTo(v2.asInstanceOf[DateTimeValue], MISSING_TIMEZONE)

  def getSchemaComparable: Comparable[AnyRef] = new DateTimeComparable().asInstanceOf[Comparable[AnyRef]]

  class DateTimeComparable extends Comparable[AnyRef] {

    private def asDateTimeValue(): DateTimeValue = DateTimeValue.this

    override def compareTo(o: AnyRef): Int =
      o match {
        case dateTimeComparable: DateTimeComparable =>
          var dt0: DateTimeValue = DateTimeValue.this
          var dt1: DateTimeValue = dateTimeComparable.asDateTimeValue()
          if (dt0.hasTimezone) {
            if (dt1.hasTimezone) {
              dt0 = dt0.adjustTimezone(0)
              dt1 = dt1.adjustTimezone(0)
              dt0.compareTo(dt1)
            } else {
              val dt1max: DateTimeValue = dt1.adjustTimezone(14 * 60)
              if (dt0.compareTo(dt1max) < 0)
                return -1
              val dt1min: DateTimeValue = dt1.adjustTimezone(-14 * 60)
              if (dt0.compareTo(dt1min) > 0)
                return +1
              SequenceTool.INDETERMINATE_ORDERING
            }
          } else {
            if (dt1.hasTimezone) {
              val dt0min: DateTimeValue = dt0.adjustTimezone(-14 * 60)
              if (dt0min.compareTo(dt1) < 0)
                return -1
              val dt0max: DateTimeValue = dt0.adjustTimezone(14 * 60)
              if (dt0max.compareTo(dt1) > 0)
                return +1
              SequenceTool.INDETERMINATE_ORDERING
            } else {
              dt0 = dt0.adjustTimezone(0)
              dt1 = dt1.adjustTimezone(0)
              dt0.compareTo(dt1)
            }
          }
        case _                              =>
          SequenceTool.INDETERMINATE_ORDERING
      }

    override def equals(o: Any): Boolean =
      o.isInstanceOf[DateTimeComparable] &&
        DateTimeValue.this.hasTimezone ==
          o.asInstanceOf[DateTimeComparable].asDateTimeValue().hasTimezone &&
        compareTo(o.asInstanceOf[AnyRef]) == 0

    override def hashCode: Int = {
      val dt0 = adjustTimezone(0)
      (dt0.year << 20) ^ (dt0.month << 16) ^ (dt0.day << 11) ^
        (dt0.hour << 7) ^
        (dt0.minute << 2) ^
        (dt0.second * 1000000000 + dt0.nanosecond)
    }
  }

  override def equals(o: Any): Boolean = o match {
    case o: DateTimeValue => compareTo(o) == 0
    case _ => false
  }

  override def hashCode: Int =
    DateTimeValue.hashCode(year,
      month,
      day,
      hour,
      minute,
      second,
      nanosecond,
      getTimezoneInMinutes)

}