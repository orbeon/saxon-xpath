package net.sf.saxon.value

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.sort.AtomicMatchKey

import net.sf.saxon.expr.sort.CodepointCollator

import net.sf.saxon.lib.ConversionRules

import net.sf.saxon.lib.StringCollator

import net.sf.saxon.model.ConversionResult

import net.sf.saxon.model.ValidationFailure

import net.sf.saxon.trans.NoDynamicContextException

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.util.FastStringBuffer

import javax.xml.datatype.XMLGregorianCalendar

import java.math.BigDecimal

import java.util.GregorianCalendar

import CalendarValue._

object CalendarValue {

  val NO_TIMEZONE: Int = java.lang.Integer.MIN_VALUE

  val MISSING_TIMEZONE: Int = java.lang.Integer.MAX_VALUE

  def makeCalendarValue(s: CharSequence,
                        rules: ConversionRules): ConversionResult = {
    var cr: ConversionResult = DateTimeValue.makeDateTimeValue(s, rules)
    val firstError: ConversionResult = cr
    if (cr.isInstanceOf[ValidationFailure]) {
      cr = DateValue.makeDateValue(s, rules)
    }
    if (cr.isInstanceOf[ValidationFailure]) {
      cr = TimeValue.makeTimeValue(s)
    }
    if (cr.isInstanceOf[ValidationFailure]) {
      cr = GYearValue.makeGYearValue(s, rules)
    }
    if (cr.isInstanceOf[ValidationFailure]) {
      cr = GYearMonthValue.makeGYearMonthValue(s, rules)
    }
    if (cr.isInstanceOf[ValidationFailure]) {
      cr = GMonthValue.makeGMonthValue(s)
    }
    if (cr.isInstanceOf[ValidationFailure]) {
      cr = GMonthDayValue.makeGMonthDayValue(s)
    }
    if (cr.isInstanceOf[ValidationFailure]) {
      cr = GDayValue.makeGDayValue(s)
    }
    if (cr.isInstanceOf[ValidationFailure]) {
      return firstError
    }
    cr
  }

  def appendTimezone(tz: Int, sb: FastStringBuffer): Unit = {
    var TZ = tz
    if (TZ == 0) {
      sb.append("Z")
    } else {
      sb.append(if (TZ > 0) "+" else "-")
      TZ = Math.abs(TZ)
      appendTwoDigits(sb, TZ / 60)
      sb.cat(':')
      appendTwoDigits(sb, TZ % 60)
    }
  }

  def appendString(sb: FastStringBuffer, value: Int, size: Int): Unit = {
    val s: String = "000000000" + value
    sb.append(s.substring(s.length - size))
  }

  def appendTwoDigits(sb: FastStringBuffer, value: Int): Unit = {
    sb.cat((value / 10 + '0').toChar)
    sb.cat((value % 10 + '0').toChar)
  }

}

abstract class CalendarValue extends AtomicValue with AtomicMatchKey {

  private var tzMinutes: Int = NO_TIMEZONE

  def hasTimezone: Boolean = tzMinutes != NO_TIMEZONE

  def setTimezoneInMinutes(minutes: Int): Unit = {
    tzMinutes = minutes
  }

  def toDateTime: DateTimeValue

  def getTimezoneInMinutes: Int = tzMinutes

  def getCalendar: GregorianCalendar

  def getXMLGregorianCalendar: XMLGregorianCalendar =
    new SaxonXMLGregorianCalendar(this)

  def add(duration: DurationValue): CalendarValue

  def subtract(other: CalendarValue,
               context: XPathContext): DayTimeDurationValue = {
    var dt1: DateTimeValue = toDateTime
    var dt2: DateTimeValue = other.toDateTime
    if (dt1.getTimezoneInMinutes != dt2.getTimezoneInMinutes) {
      var tz: Int = CalendarValue.NO_TIMEZONE
      if (context == null || ({
        tz = context.getImplicitTimezone
        tz
      }) == CalendarValue.MISSING_TIMEZONE) {
        throw new NoDynamicContextException("Implicit timezone required");
      }
      dt1 = dt1.adjustToUTC(tz)
      dt2 = dt2.adjustToUTC(tz)
    }
    val d1: BigDecimal = dt1.toJulianInstant
    val d2: BigDecimal = dt2.toJulianInstant
    val difference: BigDecimal = d1.subtract(d2)
    DayTimeDurationValue.fromSeconds(difference)
  }

  def removeTimezone: CalendarValue = {
    val c: CalendarValue = copyAsSubType(typeLabel).asInstanceOf[CalendarValue]
    c.tzMinutes = NO_TIMEZONE
    c
  }

  def adjustTimezone(tz: Int): CalendarValue

  def adjustTimezone(tz: DayTimeDurationValue): CalendarValue = {
    val microseconds: Long = tz.getLengthInMicroseconds
    if (microseconds % 60000000 != 0) {
      val err = new XPathException(
        "Timezone is not an integral number of minutes")
      err.setErrorCode("FODT0003")
      throw err
    }
    val tzminutes: Int = (microseconds / 60000000).toInt
    if (Math.abs(tzminutes) > 14 * 60) {
      val err = new XPathException(
        "Timezone out of range (-14:00 to +14:00)")
      err.setErrorCode("FODT0003")
      throw err
    }
    adjustTimezone(tzminutes)
  }

  def getXPathComparable(ordered: Boolean,
                         collator: StringCollator,
                         implicitTimezone: Int): AtomicMatchKey = {
    if (ordered && !(this.isInstanceOf[Comparable[_]])) {
      return null
    }
    if (hasTimezone) {
      return this
    }
    if (implicitTimezone == MISSING_TIMEZONE) {
      throw new NoDynamicContextException("Unknown implicit timezone")
    }
    if (hasTimezone) this else adjustTimezone(implicitTimezone)
  }

  def getComparisonKey(context: XPathContext): AtomicMatchKey =
    try getXPathComparable(ordered = false,
      CodepointCollator.getInstance,
      context.getImplicitTimezone)
    catch {
      case e: NoDynamicContextException => null

    }

  override def asMapKey(): AtomicMatchKey = new CalendarValueMapKey()

  def compareTo(other: CalendarValue, implicitTimezone: Int): Int

  override def isIdentical(v: AtomicValue): Boolean =
    super.isIdentical(v) &&
      tzMinutes == v.asInstanceOf[CalendarValue].tzMinutes

  override def identityHashCode(): Int = hashCode ^ tzMinutes

  def appendTimezone(sb: FastStringBuffer): Unit = {
    if (hasTimezone) {
      CalendarValue.appendTimezone(getTimezoneInMinutes, sb)
    }
  }

  private class CalendarValueMapKey extends AtomicMatchKey {

    def asAtomic(): CalendarValue = CalendarValue.this

    override def equals(obj: Any): Boolean = obj match {
      case obj: CalendarValueMapKey => {
        val a: CalendarValue = CalendarValue.this
        val b: CalendarValue = obj.asAtomic()
        if (a.hasTimezone == b.hasTimezone) {
          if (a.hasTimezone) {
            a.adjustTimezone(b.tzMinutes).isIdentical(b)
          } else {
            a.isIdentical(b)
          }
        } else {
          false
        }
      }
      case _ => false

    }

    override def hashCode(): Int = asAtomic().hashCode

  }

}
