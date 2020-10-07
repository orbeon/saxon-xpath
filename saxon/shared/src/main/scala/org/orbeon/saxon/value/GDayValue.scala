package org.orbeon.saxon.value

import org.orbeon.saxon.model.AtomicType
import org.orbeon.saxon.model.BuiltInAtomicType
import org.orbeon.saxon.model.ConversionResult
import org.orbeon.saxon.model.ValidationFailure
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.util.FastStringBuffer
import java.util.regex.Matcher
import java.util.regex.Pattern

import GDayValue._
import org.orbeon.saxon.value.CalendarValue.appendTwoDigits
import org.orbeon.saxon.value.GDateValue.setLexicalValue


object GDayValue {

  var regex: Pattern =
    Pattern.compile("---([0-9][0-9])(Z|[+-][0-9][0-9]:[0-9][0-9])?")

  def makeGDayValue(value: CharSequence): ConversionResult = {
    val m: Matcher = regex.matcher(Whitespace.trimWhitespace(value))
    if (!m.matches()) {
      new ValidationFailure("Cannot convert '" + value + "' to a gDay")
    }
    val g: GDayValue = new GDayValue()
    val base: String = m.group(1)
    val tz: String = m.group(2)
    val date: String = "2000-01-" + base + (if (tz == null) "" else tz)
    g.typeLabel = BuiltInAtomicType.G_DAY
    setLexicalValue(g, date, allowYearZero = true)
  }

}

class GDayValue() extends GDateValue {

  def this(day: Byte, tz: Int, `type`: AtomicType) = {
    this()
    this.year = 2000
    this.month = 1
    this.day = day
    this.setTimezoneInMinutes(tz)
    this.typeLabel = `type`
  }

  def this(day: Byte, tz: Int) = this(day, tz, BuiltInAtomicType.G_DAY)


  def copyAsSubType(typeLabel: AtomicType): AtomicValue = {
    val v: GDayValue = new GDayValue(day, getTimezoneInMinutes)
    v.typeLabel = typeLabel
    v
  }

  def getPrimitiveType: BuiltInAtomicType = BuiltInAtomicType.G_DAY

  def getPrimitiveStringValue(): CharSequence = {
    val sb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C16)
    sb.append("---")
    appendTwoDigits(sb, day)
    if (hasTimezone) {
      appendTimezone(sb)
    }
    sb
  }

  def add(duration: DurationValue): CalendarValue = {
    val err = new XPathException(
      "Cannot add a duration to an xs:gDay")
    err.setErrorCode("XPTY0004")
    throw err
  }

  def adjustTimezone(tz: Int): CalendarValue = {
    val dt: DateTimeValue =
      toDateTime.adjustTimezone(tz)
    new GDayValue(dt.getDay, dt.getTimezoneInMinutes)
  }

}