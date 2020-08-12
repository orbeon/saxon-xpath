package net.sf.saxon.value

import net.sf.saxon.lib.ConversionRules
import net.sf.saxon.model.AtomicType
import net.sf.saxon.model.BuiltInAtomicType
import net.sf.saxon.model.ConversionResult
import net.sf.saxon.model.ValidationFailure
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.util.FastStringBuffer
import java.util.regex.Matcher
import java.util.regex.Pattern

import GYearValue._
import net.sf.saxon.value.CalendarValue.appendString
import net.sf.saxon.value.GDateValue.setLexicalValue


object GYearValue {

  var regex: Pattern =
    Pattern.compile("(-?[0-9]+)(Z|[+-][0-9][0-9]:[0-9][0-9])?")

  def makeGYearValue(value: CharSequence,
                     rules: ConversionRules): ConversionResult = {
    val g: GYearValue = new GYearValue()
    val m: Matcher = regex.matcher(Whitespace.trimWhitespace(value))
    if (!m.matches()) {
      new ValidationFailure("Cannot convert '" + value + "' to a gYear")
    }
    val base: String = m.group(1)
    val tz: String = m.group(2)
    val date: String = base + "-01-01" + (if (tz == null) "" else tz)
    g.typeLabel = BuiltInAtomicType.G_YEAR
    setLexicalValue(g, date, rules.isAllowYearZero)
  }

}

class GYearValue() extends GDateValue {

  def this(year: Int, tz: Int, `type`: AtomicType) = {
    this()
    this.year = year
    this.month = 1
    this.day = 1
    this.setTimezoneInMinutes(tz)
    this.typeLabel = `type`
  }

  def this(year: Int, tz: Int, xsd10: Boolean) = {
    this(year, tz, BuiltInAtomicType.G_YEAR)
    this.hasNoYearZero = xsd10
  }

  def copyAsSubType(typeLabel: AtomicType): AtomicValue = {
    val v: GYearValue = new GYearValue(year, getTimezoneInMinutes, true)
    v.typeLabel = typeLabel
    v
  }

  def getPrimitiveType(): BuiltInAtomicType = BuiltInAtomicType.G_YEAR

  def getPrimitiveStringValue(): CharSequence = {
    val sb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C16)
    var yr: Int = year
    if (year <= 0) {
      yr = -yr + (if (hasNoYearZero) 1 else 0)
      if (yr != 0) {
        sb.cat('-')
      }
    }
    appendString(sb, yr, (if (yr > 9999) (yr + "").length else 4))
    if (hasTimezone()) {
      appendTimezone(sb)
    }
    sb
  }

  def add(duration: DurationValue): CalendarValue = {
    val err: XPathException = new XPathException(
      "Cannot add a duration to an xs:gYear")
    err.setErrorCode("XPTY0004")
    throw err
  }

  def adjustTimezone(tz: Int): CalendarValue = {
    val dt: DateTimeValue =
      toDateTime().adjustTimezone(tz).asInstanceOf[DateTimeValue]
    new GYearValue(dt.getYear, dt.getTimezoneInMinutes, hasNoYearZero)
  }

}