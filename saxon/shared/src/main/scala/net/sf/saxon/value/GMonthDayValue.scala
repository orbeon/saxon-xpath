package net.sf.saxon.value

import net.sf.saxon.model.AtomicType
import net.sf.saxon.model.BuiltInAtomicType
import net.sf.saxon.model.ConversionResult
import net.sf.saxon.model.ValidationFailure
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.util.FastStringBuffer
import java.util.regex.Matcher
import java.util.regex.Pattern

import GMonthDayValue._
import net.sf.saxon.value.CalendarValue.appendTwoDigits
import net.sf.saxon.value.GDateValue.setLexicalValue


object GMonthDayValue {

  var regex: Pattern =
    Pattern.compile("--([0-9][0-9]-[0-9][0-9])(Z|[+-][0-9][0-9]:[0-9][0-9])?")

  def makeGMonthDayValue(value: CharSequence): ConversionResult = {
    val m: Matcher = regex.matcher(Whitespace.trimWhitespace(value))
    if (!m.matches()) {
      new ValidationFailure("Cannot convert '" + value + "' to a gMonthDay")
    }
    val g: GMonthDayValue = new GMonthDayValue()
    val base: String = m.group(1)
    val tz: String = m.group(2)
    val date: String = "2000-" + base + (if (tz == null) "" else tz)
    g.typeLabel = BuiltInAtomicType.G_MONTH_DAY
    setLexicalValue(g, date, allowYearZero = true)
  }

}

class GMonthDayValue() extends GDateValue {

  def this(month: Byte, day: Byte, tz: Int, `type`: AtomicType) = {
    this()
    this.year = 2000
    this.month = month
    this.day = day
    this.setTimezoneInMinutes(tz)
    this.typeLabel = `type`
  }

  def this(month: Byte, day: Byte, tz: Int) =
    this(month, day, tz, BuiltInAtomicType.G_MONTH_DAY)


  def copyAsSubType(typeLabel: AtomicType): AtomicValue = {
    val v: GMonthDayValue =
      new GMonthDayValue(month, day, getTimezoneInMinutes)
    v.typeLabel = typeLabel
    v
  }

  def getPrimitiveType(): BuiltInAtomicType = BuiltInAtomicType.G_MONTH_DAY

  def getPrimitiveStringValue(): CharSequence = {
    val sb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C16)
    sb.append("--")
    appendTwoDigits(sb, month)
    sb.cat('-')
    appendTwoDigits(sb, day)
    if (hasTimezone()) {
      appendTimezone(sb)
    }
    sb
  }

  def add(duration: DurationValue): CalendarValue = {
    val err: XPathException = new XPathException(
      "Cannot add a duration to an xs:gMonthDay")
    err.setErrorCode("XPTY0004")
    throw err
  }

  def adjustTimezone(tz: Int): CalendarValue = {
    val dt: DateTimeValue =
      toDateTime().adjustTimezone(tz)
    new GMonthDayValue(dt.getMonth, dt.getDay, dt.getTimezoneInMinutes)
  }

}
