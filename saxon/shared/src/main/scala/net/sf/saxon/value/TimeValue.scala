package net.sf.saxon.value

import net.sf.saxon.expr.XPathContext
import net.sf.saxon.functions.AccessorFn
import net.sf.saxon.model.AtomicType
import net.sf.saxon.model.BuiltInAtomicType
import net.sf.saxon.model.ConversionResult
import net.sf.saxon.model.ValidationFailure
import net.sf.saxon.om.{Item, SequenceTool}
import net.sf.saxon.trans.Err
import net.sf.saxon.trans.NoDynamicContextException
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.util.FastStringBuffer
import java.math.BigDecimal
import java.math.RoundingMode
import java.util._

import TimeValue._
import net.sf.saxon.functions.AccessorFn.Component._
import net.sf.saxon.functions.AccessorFn._
import net.sf.saxon.value.CalendarValue.appendTwoDigits

import scala.beans.{BeanProperty, BooleanBeanProperty}


object TimeValue {

  def makeTimeValue(s: CharSequence): ConversionResult = {
    val tv: TimeValue = new TimeValue()
    val tok: StringTokenizer =
      new StringTokenizer(Whitespace.trimWhitespace(s).toString, "-:.+Z", true)
    if (!tok.hasMoreElements()) {
      badTime("too short", s)
    }
    var part: String = tok.nextElement().asInstanceOf[String]
    if (part.length != 2) {
      badTime("hour must be two digits", s)
    }
    var value: Int = DurationValue.simpleInteger(part)
    if (value < 0) {
      badTime("Non-numeric hour component", s)
    }
    tv.hour = value.toByte
    if (tv.hour > 24) {
      badTime("hour is out of range", s)
    }
    if (!tok.hasMoreElements()) {
      badTime("too short", s)
    }
    if (":" != tok.nextElement()) {
      badTime("wrong delimiter after hour", s)
    }
    if (!tok.hasMoreElements()) {
      badTime("too short", s)
    }
    part = tok.nextElement().asInstanceOf[String]
    if (part.length != 2) {
      badTime("minute must be two digits", s)
    }
    value = DurationValue.simpleInteger(part)
    if (value < 0) {
      badTime("Non-numeric minute component", s)
    }
    tv.minute = value.toByte
    if (tv.minute > 59) {
      badTime("minute is out of range", s)
    }
    if (tv.hour == 24 && tv.minute != 0) {
      badTime("If hour is 24, minute must be 00", s)
    }
    if (!tok.hasMoreElements()) {
      badTime("too short", s)
    }
    if (":" != tok.nextElement()) {
      badTime("wrong delimiter after minute", s)
    }
    if (!tok.hasMoreElements()) {
      badTime("too short", s)
    }
    part = tok.nextElement().asInstanceOf[String]
    if (part.length != 2) {
      badTime("second must be two digits", s)
    }
    value = DurationValue.simpleInteger(part)
    if (value < 0) {
      badTime("Non-numeric second component", s)
    }
    tv.second = value.toByte
    if (tv.second > 59) {
      badTime("second is out of range", s)
    }
    if (tv.hour == 24 && tv.second != 0) {
      badTime("If hour is 24, second must be 00", s)
    }
    var tz: Int = 0
    var negativeTz: Boolean = false
    var state: Int = 0
    while (tok.hasMoreElements()) {
      if (state == 9) {
        badTime("characters after the end", s)
      }
      val delim: String = tok.nextElement().asInstanceOf[String]
      if ("." == delim) {
        if (state != 0) {
          badTime("decimal separator occurs twice", s)
        }
        if (!tok.hasMoreElements()) {
          badTime("decimal point must be followed by digits", s)
        }
        part = tok.nextElement().asInstanceOf[String]
        if (part.length > 9 && part.matches("^[0-9]+$")) {
          part = part.substring(0, 9)
        }
        value = DurationValue.simpleInteger(part)
        if (value < 0) {
          badTime("Non-numeric fractional seconds component", s)
        }
        val fractionalSeconds: Double =
          java.lang.Double.parseDouble("."+ part)
        tv.nanosecond = Math.round(fractionalSeconds * 1000000000).toInt
        if (tv.hour == 24 && tv.nanosecond != 0) {
          badTime("If hour is 24, fractional seconds must be 0", s)
        }
        state = 1
      } else if ("Z" == delim) {
        if (state > 1) {
          badTime("Z cannot occur here", s)
        }
        tz = 0
        state = 9
        tv.setTimezoneInMinutes(0)
      } else if ("+" == delim || "-" == delim) {
        if (state > 1) {
          badTime(delim + " cannot occur here", s)
        }
        state = 2
        if (!tok.hasMoreElements()) {
          badTime("missing timezone", s)
        }
        part = tok.nextElement().asInstanceOf[String]
        if (part.length != 2) {
          badTime("timezone hour must be two digits", s)
        }
        value = DurationValue.simpleInteger(part)
        if (value < 0) {
          badTime("Non-numeric timezone hour component", s)
        }
        tz = value * 60
        if (tz > 14 * 60) {
          badTime("timezone hour is out of range", s)
        }
        if ("-" == delim) {
          negativeTz = true
        }
      } else if (":" == delim) {
        if (state != 2) {
          badTime("colon cannot occur here", s)
        }
        state = 9
        part = tok.nextElement().asInstanceOf[String]
        value = DurationValue.simpleInteger(part)
        if (value < 0) {
          badTime("Non-numeric timezone minute component", s)
        }
        val tzminute: Int = value
        if (part.length != 2) {
          badTime("timezone minute must be two digits", s)
        }
        if (tzminute > 59) {
          badTime("timezone minute is out of range", s)
        }
        tz += tzminute
        if (negativeTz) {
          tz = -tz
        }
        tv.setTimezoneInMinutes(tz)
      } else {
        badTime("timezone format is incorrect", s)
      }
    }
    if (state == 2 || state == 3) {
      badTime("timezone incomplete", s)
    }
    if (tv.hour == 24) {
      tv.hour = 0
    }
    tv.typeLabel = BuiltInAtomicType.TIME
    tv
  }

  private def badTime(msg: String, value: CharSequence): ValidationFailure = {
    val err: ValidationFailure = new ValidationFailure(
      "Invalid time " + Err.wrap(value, Err.VALUE) + " (" +
        msg +
        ")")
    err.setErrorCode("FORG0001")
    err
  }

}

class TimeValue extends CalendarValue with Comparable[AnyRef] {

  @BeanProperty
  var hour: Byte = _

  @BeanProperty
  var minute: Byte = _

  @BeanProperty
  var second: Byte = _

  @BeanProperty
  var nanosecond: Int = _

  def this(hour: Byte, minute: Byte, second: Byte, microsecond: Int, tz: Int) = {
    this()
    this.hour = hour
    this.minute = minute
    this.second = second
    this.nanosecond = microsecond * 1000
    this.setTimezoneInMinutes(tz)
    typeLabel = BuiltInAtomicType.TIME
  }

  def this(hour: Byte,
           minute: Byte,
           second: Byte,
           nanosecond: Int,
           tz: Int,
           flag: String) = {
    this()
    if (!flag.isEmpty) {
      throw new IllegalArgumentException()
    }
    this.hour = hour
    this.minute = minute
    this.second = second
    this.nanosecond = nanosecond
    this.setTimezoneInMinutes(tz)
    typeLabel = BuiltInAtomicType.TIME
  }

  def makeTimeValue(hour: Byte,
                    minute: Byte,
                    second: Byte,
                    nanosecond: Int,
                    tz: Int): TimeValue =
    new TimeValue(hour, minute, second, nanosecond, tz, "")

  def this(calendar: GregorianCalendar, tz: Int) = {
    this()
    hour = calendar.get(Calendar.HOUR_OF_DAY).toByte
    minute = calendar.get(Calendar.MINUTE).toByte
    second = calendar.get(Calendar.SECOND).toByte
    nanosecond = calendar.get(Calendar.MILLISECOND) * 1000000
    this.setTimezoneInMinutes(tz)
    typeLabel = BuiltInAtomicType.TIME
  }

  def getPrimitiveType(): BuiltInAtomicType = BuiltInAtomicType.TIME

  def getMicrosecond(): Int = nanosecond / 1000

  def getPrimitiveStringValue(): CharSequence = {
    val sb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C16)
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
    if (hasTimezone()) {
      appendTimezone(sb)
    }
    sb
  }

  override def getCanonicalLexicalRepresentation(): CharSequence =
    if (hasTimezone() && getTimezoneInMinutes != 0) {
      adjustTimezone(0).getStringValueCS
    } else {
      getStringValueCS
    }

  def toDateTime(): DateTimeValue =
    new DateTimeValue(1972,
      12.toByte,
      31.toByte,
      hour,
      minute,
      second,
      nanosecond,
      getTimezoneInMinutes)

  def getCalendar(): GregorianCalendar = {
    val tz: Int = if (hasTimezone()) getTimezoneInMinutes * 60000 else 0
    val zone: TimeZone = new SimpleTimeZone(tz, "LLL")
    val calendar: GregorianCalendar = new GregorianCalendar(zone)
    calendar.setLenient(false)
    if (tz < calendar.getMinimum(Calendar.ZONE_OFFSET) || tz > calendar
      .getMaximum(Calendar.ZONE_OFFSET)) {
      adjustTimezone(0).getCalendar
    }
    calendar.set(1972, Calendar.DECEMBER, 31, hour, minute, second)
    calendar.set(Calendar.MILLISECOND, nanosecond / 1000000)
    calendar.set(Calendar.ZONE_OFFSET, tz)
    calendar.set(Calendar.DST_OFFSET, 0)
    calendar.getTime
    calendar
  }

  def copyAsSubType(typeLabel: AtomicType): AtomicValue = {
    val v: TimeValue =
      new TimeValue(hour, minute, second, nanosecond, getTimezoneInMinutes, "")
    v.typeLabel = typeLabel
    v
  }

  def adjustTimezone(timezone: Int): TimeValue = {
    val dt: DateTimeValue = toDateTime().adjustTimezone(timezone)
    new TimeValue(dt.getHour,
      dt.getMinute,
      dt.getSecond,
      dt.getNanosecond,
      dt.getTimezoneInMinutes,
      "")
  }

  override def getComponent(component: Component): AtomicValue =
    component match {
      case HOURS => Int64Value.makeIntegerValue(hour)
      case MINUTES => Int64Value.makeIntegerValue(minute)
      case SECONDS =>
        var d: BigDecimal = BigDecimal.valueOf(nanosecond)
        d = d.divide(BigDecimalValue.BIG_DECIMAL_ONE_BILLION,
          6,
          RoundingMode.HALF_UP)
        d = d.add(BigDecimal.valueOf(second))
        new BigDecimalValue(d)
      case WHOLE_SECONDS => Int64Value.makeIntegerValue(second)
      case MICROSECONDS => new Int64Value(nanosecond / 1000)
      case NANOSECONDS => new Int64Value(nanosecond)
      case TIMEZONE =>
        if (hasTimezone()) {
          DayTimeDurationValue.fromMilliseconds(60000L * getTimezoneInMinutes)
        } else {
          null
        }
      case _ =>
        throw new IllegalArgumentException(
          "Unknown component for time: " + component)

    }

  def compareTo(other: AnyRef): Int = {
    val otherTime: TimeValue = other.asInstanceOf[TimeValue]
    if (getTimezoneInMinutes == otherTime.getTimezoneInMinutes) {
      if (hour != otherTime.hour) {
        IntegerValue.signum(hour - otherTime.hour)
      } else if (minute != otherTime.minute) {
        IntegerValue.signum(minute - otherTime.minute)
      } else if (second != otherTime.second) {
        IntegerValue.signum(second - otherTime.second)
      } else if (nanosecond != otherTime.nanosecond) {
        IntegerValue.signum(nanosecond - otherTime.nanosecond)
      } else {
        0
      }
    } else {
      toDateTime().compareTo(otherTime.toDateTime())
    }
  }

  def compareTo(other: CalendarValue, implicitTimezone: Int): Int = {
    if (!(other.isInstanceOf[TimeValue])) {
      throw new ClassCastException(
        "Time values are not comparable to " + other.getClass)
    }
    val otherTime: TimeValue = other.asInstanceOf[TimeValue]
    if (getTimezoneInMinutes == otherTime.getTimezoneInMinutes) {
      compareTo(other)
    } else {
      toDateTime().compareTo(otherTime.toDateTime(), implicitTimezone)
    }
  }

  def getSchemaComparable(): Comparable[AnyRef] = new TimeComparable().asInstanceOf[Comparable[AnyRef]]

  private class TimeComparable extends Comparable[AnyRef] {

    def asTimeValue(): TimeValue = TimeValue.this

    def compareTo(o: AnyRef): Int =
      if (o.isInstanceOf[TimeComparable]) {
        val dt0: DateTimeValue = asTimeValue().toDateTime()
        val dt1: DateTimeValue =
          o.asInstanceOf[TimeComparable].asTimeValue().toDateTime()
        dt0.getSchemaComparable.compareTo(dt1.getSchemaComparable.asInstanceOf)
      } else {
        SequenceTool.INDETERMINATE_ORDERING
      }

    override def equals(o: Any): Boolean = return compareTo(o.asInstanceOf) == 0

    override def hashCode(): Int =
      TimeValue.this.toDateTime().getSchemaComparable.hashCode

  }

  override def equals(other: Any): Boolean = other match {
    case other: TimeValue => compareTo(other) == 0
    case _ => false

  }

  override def hashCode(): Int =
    DateTimeValue.hashCode(1951,
      10.toByte,
      11.toByte,
      hour,
      minute,
      second,
      nanosecond,
      getTimezoneInMinutes)

  def add(duration: DurationValue): TimeValue =
    if (duration.isInstanceOf[DayTimeDurationValue]) {
      val dt: DateTimeValue = toDateTime().add(duration)
      new TimeValue(dt.getHour,
        dt.getMinute,
        dt.getSecond,
        dt.getNanosecond,
        getTimezoneInMinutes,
        "")
    } else {
      val err = new XPathException(
        "Time+Duration arithmetic is supported only for xs:dayTimeDuration")
      err.setErrorCode("XPTY0004")
      err.setIsTypeError(true)
      throw err
    }

  override def subtract(other: CalendarValue,
                        context: XPathContext): DayTimeDurationValue = {
    if (!(other.isInstanceOf[TimeValue])) {
      val err = new XPathException(
        "First operand of '-' is a time, but the second is not")
      err.setIsTypeError(true)
      throw err
    }
    super.subtract(other, context)
  }

}
