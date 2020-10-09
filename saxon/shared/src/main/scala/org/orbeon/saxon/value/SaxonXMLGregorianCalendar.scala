//package org.orbeon.saxon.value
//
//import java.math.{BigDecimal, BigInteger}
//import java.util._
//
//import javax.xml.datatype.{DatatypeConstants, Duration, XMLGregorianCalendar}
//import javax.xml.namespace.QName
//import org.orbeon.saxon.functions.AccessorFn
//import org.orbeon.saxon.om.Item
//
//// ORBEON: Unused class
//class SaxonXMLGregorianCalendar(value: CalendarValue)
//  extends XMLGregorianCalendar {
//
//  private var calendarValue: CalendarValue = value
//
//  private var year: BigInteger = _
//
//  //@BeanProperty
//  var month: Int = DatatypeConstants.FIELD_UNDEFINED
//
//  //@BeanProperty
//  var day: Int = DatatypeConstants.FIELD_UNDEFINED
//
//  //@BeanProperty
//  var hour: Int = DatatypeConstants.FIELD_UNDEFINED
//
//  //@BeanProperty
//  var minute: Int = DatatypeConstants.FIELD_UNDEFINED
//
//  //@BeanProperty
//  var second: Int = DatatypeConstants.FIELD_UNDEFINED
//
//  private var microsecond: Int = DatatypeConstants.FIELD_UNDEFINED
//
//  private var tzOffset: Int = DatatypeConstants.FIELD_UNDEFINED
//
//  clear()
//
//
//  def setCalendarValue(value: CalendarValue): Unit = {
//    calendarValue = value
//    if (value.isInstanceOf[GYearValue]) {
//      year = BigInteger.valueOf(
//        value
//          .getComponent(AccessorFn.Component.YEAR)
//          .asInstanceOf[Int64Value]
//          .longValue())
//    } else if (value.isInstanceOf[GYearMonthValue]) {
//      year = BigInteger.valueOf(
//        value
//          .getComponent(AccessorFn.Component.YEAR)
//          .asInstanceOf[Int64Value]
//          .longValue())
//      month = value
//        .getComponent(AccessorFn.Component.MONTH)
//        .asInstanceOf[Int64Value]
//        .longValue()
//        .toInt
//    } else if (value.isInstanceOf[GMonthValue]) {
//      month = value
//        .getComponent(AccessorFn.Component.MONTH)
//        .asInstanceOf[Int64Value]
//        .longValue()
//        .toInt
//    } else if (value.isInstanceOf[GMonthDayValue]) {
//      month = value
//        .getComponent(AccessorFn.Component.MONTH)
//        .asInstanceOf[Int64Value]
//        .longValue()
//        .toInt
//      day = value
//        .getComponent(AccessorFn.Component.DAY)
//        .asInstanceOf[Int64Value]
//        .longValue()
//        .toInt
//    } else if (value.isInstanceOf[GDayValue]) {
//      day = value
//        .getComponent(AccessorFn.Component.DAY)
//        .asInstanceOf[Int64Value]
//        .longValue()
//        .toInt
//    } else if (value.isInstanceOf[DateValue]) {
//      year = BigInteger.valueOf(
//        value
//          .getComponent(AccessorFn.Component.YEAR)
//          .asInstanceOf[Int64Value]
//          .longValue())
//      month = value
//        .getComponent(AccessorFn.Component.MONTH)
//        .asInstanceOf[Int64Value]
//        .longValue()
//        .toInt
//      day = value
//        .getComponent(AccessorFn.Component.DAY)
//        .asInstanceOf[Int64Value]
//        .longValue()
//        .toInt
//    } else if (value.isInstanceOf[TimeValue]) {
//      hour = value
//        .getComponent(AccessorFn.Component.HOURS)
//        .asInstanceOf[Int64Value]
//        .longValue()
//        .toInt
//      minute = value
//        .getComponent(AccessorFn.Component.MINUTES)
//        .asInstanceOf[Int64Value]
//        .longValue()
//        .toInt
//      second = value
//        .getComponent(AccessorFn.Component.WHOLE_SECONDS)
//        .asInstanceOf[Int64Value]
//        .longValue()
//        .toInt
//      microsecond = value
//        .getComponent(AccessorFn.Component.MICROSECONDS)
//        .asInstanceOf[Int64Value]
//        .longValue()
//        .toInt
//    } else {
//      year = BigInteger.valueOf(
//        value
//          .getComponent(AccessorFn.Component.YEAR)
//          .asInstanceOf[Int64Value]
//          .longValue())
//      month = value
//        .getComponent(AccessorFn.Component.MONTH)
//        .asInstanceOf[Int64Value]
//        .longValue()
//        .toInt
//      day = value
//        .getComponent(AccessorFn.Component.DAY)
//        .asInstanceOf[Int64Value]
//        .longValue()
//        .toInt
//      hour = value
//        .getComponent(AccessorFn.Component.HOURS)
//        .asInstanceOf[Int64Value]
//        .longValue()
//        .toInt
//      minute = value
//        .getComponent(AccessorFn.Component.MINUTES)
//        .asInstanceOf[Int64Value]
//        .longValue()
//        .toInt
//      second = value
//        .getComponent(AccessorFn.Component.WHOLE_SECONDS)
//        .asInstanceOf[Int64Value]
//        .longValue()
//        .toInt
//      microsecond = value
//        .getComponent(AccessorFn.Component.MICROSECONDS)
//        .asInstanceOf[Int64Value]
//        .longValue()
//        .toInt
//    }
//  }
//
//  def clear(): Unit = {
//    year = null
//    month = DatatypeConstants.FIELD_UNDEFINED
//    day = DatatypeConstants.FIELD_UNDEFINED
//    hour = DatatypeConstants.FIELD_UNDEFINED
//    minute = DatatypeConstants.FIELD_UNDEFINED
//    second = DatatypeConstants.FIELD_UNDEFINED
//    microsecond = DatatypeConstants.FIELD_UNDEFINED
//    tzOffset = DatatypeConstants.FIELD_UNDEFINED
//  }
//
//  def reset(): Unit = {
//    clear()
//  }
//
//  def setYear(year: BigInteger): Unit = {
//    calendarValue = null
//    this.year = year
//  }
//
//  def setYear(year: Int): Unit = {
//    calendarValue = null
//    this.year = BigInteger.valueOf(year)
//  }
//
//  def setMonth(month: Int): Unit = {
//    calendarValue = null
//    this.month = month
//  }
//
//  def setDay(day: Int): Unit = {
//    calendarValue = null
//    this.day = day
//  }
//
//  def setTimezone(offset: Int): Unit = {
//    calendarValue = null
//    tzOffset = offset
//  }
//
//  def setHour(hour: Int): Unit = {
//    calendarValue = null
//    this.hour = hour
//  }
//
//  def setMinute(minute: Int): Unit = {
//    calendarValue = null
//    this.minute = minute
//  }
//
//  def setSecond(second: Int): Unit = {
//    calendarValue = null
//    this.second = second
//  }
//
//  def setMillisecond(millisecond: Int): Unit = {
//    calendarValue = null
//    microsecond = millisecond * 1000
//  }
//
//  def setFractionalSecond(fractional: BigDecimal): Unit = {
//    calendarValue = null
//    second = fractional.intValue()
//    var micros: BigInteger = fractional.movePointRight(6).toBigInteger()
//    micros = micros.remainder(BigInteger.valueOf(1000000))
//    microsecond = micros.intValue()
//  }
//
//  def getEon(): BigInteger = year.divide(BigInteger.valueOf(1000000000))
//
//  def getYear(): Int = year.intValue()
//
//  def getEonAndYear(): BigInteger = year
//
//  def getTimezone: Int = tzOffset
//
//  def getMicrosecond: Int = {
//    val fractionalSeconds: BigDecimal = getFractionalSecond
//    if (fractionalSeconds == null) {
//      DatatypeConstants.FIELD_UNDEFINED
//    }
//    getFractionalSecond.movePointRight(6).intValue()
//  }
//
//  def getFractionalSecond(): BigDecimal = {
//    if (second == DatatypeConstants.FIELD_UNDEFINED) {
//      return null
//    }
//    BigDecimal.valueOf(microsecond).movePointLeft(6)
//  }
//
//  def compare(xmlGregorianCalendar: XMLGregorianCalendar): Int =
//    toCalendarValue.getSchemaComparable.compareTo(
//      xmlGregorianCalendar
//        .asInstanceOf[SaxonXMLGregorianCalendar]
//        .toCalendarValue
//        .getSchemaComparable.asInstanceOf[Item])
//
//  def normalize(): XMLGregorianCalendar =
//    new SaxonXMLGregorianCalendar(toCalendarValue.adjustTimezone(0))
//
//  def toXMLFormat(): String = toCalendarValue.getStringValue
//
//  def getXMLSchemaType(): QName =
//    if (second == DatatypeConstants.FIELD_UNDEFINED) {
//      if (year == null) {
//        if (month == DatatypeConstants.FIELD_UNDEFINED) {
//          DatatypeConstants.GDAY
//        } else if (day == DatatypeConstants.FIELD_UNDEFINED) {
//          DatatypeConstants.GMONTH
//        } else {
//          DatatypeConstants.GMONTHDAY
//        }
//      } else if (day == DatatypeConstants.FIELD_UNDEFINED) {
//        if (month == DatatypeConstants.FIELD_UNDEFINED) {
//          DatatypeConstants.GYEAR
//        } else {
//          DatatypeConstants.GYEARMONTH
//        }
//      }
//      DatatypeConstants.DATE
//    } else if (year == null) {
//      DatatypeConstants.TIME
//    } else {
//      DatatypeConstants.DATETIME
//    }
//
//  def isValid(): Boolean = true
//
//  def add(duration: Duration): Unit = {
//    val cv: CalendarValue = toCalendarValue.add(
//      duration.asInstanceOf[SaxonDuration].getDurationValue)
//    this.calendarValue = cv
//  }
//
//  def toGregorianCalendar: GregorianCalendar =
//    toCalendarValue.getCalendar
//
//  def toGregorianCalendar(timezone: TimeZone,
//                          aLocale: Locale,
//                          defaults: XMLGregorianCalendar): GregorianCalendar = {
//    val gc: GregorianCalendar = new GregorianCalendar(timezone, aLocale)
//    gc.setGregorianChange(new Date(java.lang.Long.MIN_VALUE))
//    gc.set(Calendar.ERA,
//      if (year == null) if (defaults.getYear > 0) +1 else -1
//      else year.signum())
//    gc.set(Calendar.YEAR,
//      if (year == null) defaults.getYear else year.abs().intValue())
//    gc.set(Calendar.MONTH,
//      if (month == DatatypeConstants.FIELD_UNDEFINED) defaults.getMonth
//      else month)
//    gc.set(
//      Calendar.DAY_OF_MONTH,
//      if (day == DatatypeConstants.FIELD_UNDEFINED) defaults.getDay else day)
//    gc.set(Calendar.HOUR,
//      if (hour == DatatypeConstants.FIELD_UNDEFINED) defaults.getHour
//      else hour)
//    gc.set(Calendar.MINUTE,
//      if (minute == DatatypeConstants.FIELD_UNDEFINED) defaults.getMinute
//      else minute)
//    gc.set(Calendar.SECOND,
//      if (second == DatatypeConstants.FIELD_UNDEFINED) defaults.getSecond
//      else second)
//    gc.set(Calendar.MILLISECOND,
//      if (microsecond == DatatypeConstants.FIELD_UNDEFINED)
//        defaults.getMillisecond
//      else microsecond / 1000)
//    gc
//  }
//
//  def getTimeZone(defaultZoneoffset: Int): TimeZone =
//    if (tzOffset == DatatypeConstants.FIELD_UNDEFINED) {
//      if (defaultZoneoffset == DatatypeConstants.FIELD_UNDEFINED) {
//        new GregorianCalendar().getTimeZone
//      } else {
//        new SimpleTimeZone(defaultZoneoffset * 60000, "XXX")
//      }
//    } else {
//      new SimpleTimeZone(tzOffset * 60000, "XXX")
//    }
//
//  override def clone: AnyRef = {
//    val s: SaxonXMLGregorianCalendar = new SaxonXMLGregorianCalendar(null)
//    s.setYear(year)
//    s.setMonth(month)
//    s.setDay(day)
//    s.setHour(hour)
//    s.setMinute(minute)
//    s.setSecond(second)
//    s.setMillisecond(microsecond / 1000)
//    s.setTimezone(tzOffset)
//    s
//  }
//
//  def toCalendarValue: CalendarValue = {
//    if (calendarValue != null) {
//      return calendarValue
//    }
//    if (second == DatatypeConstants.FIELD_UNDEFINED) {
//      if (year == null) {
//        if (month == DatatypeConstants.FIELD_UNDEFINED) {
//          new GDayValue(day.toByte, tzOffset)
//        } else if (day == DatatypeConstants.FIELD_UNDEFINED) {
//          new GMonthValue(month.toByte, tzOffset)
//        } else {
//          new GMonthDayValue(month.toByte, day.toByte, tzOffset)
//        }
//      } else if (day == DatatypeConstants.FIELD_UNDEFINED) {
//        if (month == DatatypeConstants.FIELD_UNDEFINED) {
//          new GYearValue(year.intValue(), tzOffset, true)
//        } else {
//          new GYearMonthValue(year.intValue(), month.toByte, tzOffset, true)
//        }
//      }
//      new DateValue(year.intValue(), month.toByte, day.toByte, tzOffset, true)
//    } else if (year == null) {
//      new TimeValue(hour.toByte,
//        minute.toByte,
//        second.toByte,
//        getMicrosecond * 1000,
//        tzOffset,
//        "")
//    } else {
//      new DateTimeValue(year.intValue(),
//        month.toByte,
//        day.toByte,
//        hour.toByte,
//        minute.toByte,
//        second.toByte,
//        getMicrosecond,
//        tzOffset,
//        true)
//    }
//  }
//
//  override def getMonth: Int = return month.intValue()
//
//  override def getDay: Int = return day.intValue()
//
//  override def getHour: Int = return hour.intValue()
//
//  override def getMinute: Int = return minute.intValue()
//
//  override def getSecond: Int = return second.intValue()
//}