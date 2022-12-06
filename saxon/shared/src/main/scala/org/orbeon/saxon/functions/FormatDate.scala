package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.expr.number._
import org.orbeon.saxon.functions.FormatDate._
import org.orbeon.saxon.lib.Numberer
import org.orbeon.saxon.om.{Sequence, StructuredQName, ZeroOrOne}
import org.orbeon.saxon.regex.UnicodeString
import org.orbeon.saxon.regex.charclass.Categories
import org.orbeon.saxon.trans.{Err, XPathException}
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.value._

import java.util.Arrays
import java.util.regex.Pattern
import java.{lang => jl}
import scala.annotation.tailrec
import scala.util.control.Breaks._


object FormatDate {

  val knownCalendars: Array[String] =
    Array(
      "AD",
      "AH",
      "AME",
      "AM",
      "AP",
      "AS",
      "BE",
      "CB",
      "CE",
      "CL",
      "CS",
      "EE",
      "FE",
      "ISO",
      "JE",
      "KE",
      "KY",
      "ME",
      "MS",
      "NS",
      "OS",
      "RS",
      "SE",
      "SH",
      "SS",
      "TE",
      "VE",
      "VS"
    )

  private def formatDate(
    value   : CalendarValue,
    format  : String,
    language: String,
    place   : String,
    context : XPathContext
  ): CharSequence = {
    val config            = context.getConfiguration
    val languageDefaulted = language == null
    var lang = language
    var placeStr = place
    val calVal            = value
    if (lang == null)
      lang = config.getDefaultLanguage
    if (placeStr == null)
      placeStr = config.getDefaultCountry
    if (calVal.hasTimezone && placeStr.contains("/")) {
      // ORBEON: TimeZone
//      val tz = TimeZone.getTimeZone(placeStr)
//      if (tz != null) {
//        // ORBEON: GregorianCalendar
////        val milliOffset = tz.getOffset(calVal.toDateTime.getCalendar.getTime.getTime)
//        val milliOffset = tz.getOffset(calVal.toDateTime.secondsSinceEpoch.longValue * 1000)
//        calVal = calVal.adjustTimezone(milliOffset / 60000)
//      }
    }

    val numberer = config.makeNumberer(lang, placeStr)
    val sb       = new FastStringBuffer(FastStringBuffer.C64)

    if (numberer.getClass == classOf[Numberer_en] && "en" != lang && ! languageDefaulted)
      sb.append("[Language: en]")
    if (numberer.defaultedLocale() != null)
      sb.append("[Language: " + numberer.defaultedLocale().getLanguage + "]")

    var i = 0
    breakable {
      while (true) {
        while (i < format.length && format.charAt(i) != '[') {
          sb.cat(format.charAt(i))
          if (format.charAt(i) == ']') {
            i += 1
            if (i == format.length || format.charAt(i) != ']') {
              val e = new XPathException("Closing ']' in date picture must be written as ']]'")
              e.setErrorCode("FOFD1340")
              e.setXPathContext(context)
              throw e
            }
          }
          i += 1
        }
        if (i == format.length)
          break()
        i += 1
        if (i < format.length && format.charAt(i) == '[') {
          sb.cat('[')
          i += 1
        } else {
          val close = if (i < format.length) format.indexOf("]", i) else -1
          if (close == -1) {
            val e = new XPathException("Date format contains a '[' with no matching ']'")
            e.setErrorCode("FOFD1340")
            e.setXPathContext(context)
            throw e
          }
          val componentFormat = format.substring(i, close)
          sb.cat(
            formatComponent(
              calVal,
              Whitespace.removeAllWhitespace(componentFormat),
              numberer,
              placeStr,
              context
            )
          )
          i = close + 1
        }
      }
    }
    sb
  }

  private val componentPattern: Pattern =
    Pattern.compile("([YMDdWwFHhmsfZzPCE])\\s*(.*)")

  private def formatComponent(
    value    : CalendarValue,
    specifier: CharSequence,
    numberer : Numberer,
    country  : String,
    context  : XPathContext
  ): CharSequence = {
    val ignoreDate = value.isInstanceOf[TimeValue]
    val ignoreTime = value.isInstanceOf[DateValue]
    val dtvalue = value.toDateTime
    val matcher = componentPattern.matcher(specifier)

    if (! matcher.matches()) {
      val error = new XPathException("Unrecognized date/time component [" + specifier + ']')
      error.setErrorCode("FOFD1340")
      error.setXPathContext(context)
      throw error
    }
    val component = matcher.group(1)
    var format = matcher.group(2)
    if (format == null)
      format = ""
    var defaultFormat = false
    if ("" == format || format.startsWith(",")) {
      defaultFormat = true
      component.charAt(0) match {
        case 'F'       => format = s"Nn$format"
        case 'P'       => format = s"n$format"
        case 'C' | 'E' => format = s"N$format"
        case 'm' | 's' => format = s"01$format"
        case 'z' | 'Z' =>
        case _         => format = s"1$format"
      }
    }
    component.charAt(0) match {
      case 'Y' =>
        if (ignoreDate) {
          val error = new XPathException("In format-time(): an xs:time value does not contain a year component")
          error.setErrorCode("FOFD1350")
          error.setXPathContext(context)
          throw error
        } else {
          var year = dtvalue.getYear
          if (year < 0)
            year = 0 - year
          formatNumber(
            component,
            year,
            format,
            defaultFormat,
            numberer,
            context
          )
        }
      case 'M' =>
        if (ignoreDate) {
          val error = new XPathException("In format-time(): an xs:time value does not contain a month component")
          error.setErrorCode("FOFD1350")
          error.setXPathContext(context)
          throw error
        } else {
          val month = dtvalue.getMonth
          formatNumber(
            component,
            month,
            format,
            defaultFormat,
            numberer,
            context
          )
        }
      case 'D' =>
        if (ignoreDate) {
          val error = new XPathException("In format-time(): an xs:time value does not contain a day component")
          error.setErrorCode("FOFD1350")
          error.setXPathContext(context)
          throw error
        } else {
          val day = dtvalue.getDay
          formatNumber(
            component,
            day,
            format,
            defaultFormat,
            numberer,
            context
          )
        }
      case 'd' =>
        if (ignoreDate) {
          val error = new XPathException("In format-time(): an xs:time value does not contain a day component")
          error.setErrorCode("FOFD1350")
          error.setXPathContext(context)
          throw error
        } else {
          val day =
            DateValue.getDayWithinYear(
              dtvalue.getYear,
              dtvalue.getMonth,
              dtvalue.getDay
            )
          formatNumber(
            component,
            day,
            format,
            defaultFormat,
            numberer,
            context
          )
        }
      case 'W' =>
        if (ignoreDate) {
          val error = new XPathException("In format-time(): cannot obtain the week number from an xs:time value")
          error.setErrorCode("FOFD1350")
          error.setXPathContext(context)
          throw error
        } else {
          val week =
            DateValue.getWeekNumber(
              dtvalue.getYear,
              dtvalue.getMonth,
              dtvalue.getDay
            )
          formatNumber(
            component,
            week,
            format,
            defaultFormat,
            numberer,
            context
          )
        }
      case 'w' =>
        if (ignoreDate) {
          val error = new XPathException("In format-time(): cannot obtain the week number from an xs:time value")
          error.setErrorCode("FOFD1350")
          error.setXPathContext(context)
          throw error
        } else {
          val week =
            DateValue.getWeekNumberWithinMonth(
              dtvalue.getYear,
              dtvalue.getMonth,
              dtvalue.getDay
            )
          formatNumber(
            component,
            week,
            format,
            defaultFormat,
            numberer,
            context
          )
        }
      case 'H' =>
        if (ignoreTime) {
          val error = new XPathException("In format-date(): an xs:date value does not contain an hour component")
          error.setErrorCode("FOFD1350")
          error.setXPathContext(context)
          throw error
        } else {
          val hour = value
            .getComponent(AccessorFn.Component.HOURS)
            .asInstanceOf[Int64Value]
          assert(hour != null)
          formatNumber(
            component,
            hour.longValue.toInt,
            format,
            defaultFormat,
            numberer,
            context
          )
        }
      case 'h' =>
        if (ignoreTime) {
          val error = new XPathException("In format-date(): an xs:date value does not contain an hour component")
          error.setErrorCode("FOFD1350")
          error.setXPathContext(context)
          throw error
        } else {
          val hour = value
            .getComponent(AccessorFn.Component.HOURS)
            .asInstanceOf[Int64Value]
          assert(hour != null)
          var hr = hour.longValue.toInt
          if (hr > 12)
            hr = hr - 12
          if (hr == 0)
            hr = 12
          formatNumber(component, hr, format, defaultFormat, numberer, context)
        }
      case 'm' =>
        if (ignoreTime) {
          val error = new XPathException("In format-date(): an xs:date value does not contain a minutes component")
          error.setErrorCode("FOFD1350")
          error.setXPathContext(context)
          throw error
        } else {
          val minutes = value
            .getComponent(AccessorFn.Component.MINUTES)
            .asInstanceOf[Int64Value]
          assert(minutes != null)
          formatNumber(
            component,
            minutes.longValue.toInt,
            format,
            defaultFormat,
            numberer,
            context
          )
        }
      case 's' =>
        if (ignoreTime) {
          val error = new XPathException("In format-date(): an xs:date value does not contain a seconds component")
          error.setErrorCode("FOFD1350")
          error.setXPathContext(context)
          throw error
        } else {
          val seconds = value
            .getComponent(AccessorFn.Component.WHOLE_SECONDS)
            .asInstanceOf[IntegerValue]
          assert(seconds != null)
          formatNumber(
            component,
            seconds.longValue.toInt,
            format,
            defaultFormat,
            numberer,
            context
          )
        }
      case 'f' =>
        if (ignoreTime) {
          val error = new XPathException("In format-date(): an xs:date value does not contain a fractional seconds component")
          error.setErrorCode("FOFD1350")
          error.setXPathContext(context)
          throw error
        } else {
          val micros = value
            .getComponent(AccessorFn.Component.MICROSECONDS)
            .asInstanceOf[Int64Value]
          assert(micros != null)
          formatNumber(
            component,
            micros.longValue.toInt,
            format,
            defaultFormat,
            numberer,
            context
          )
        }
      case 'z' | 'Z' =>
        var dtv: DateTimeValue = null
        value match {
          case timeValue: TimeValue =>
            val now = DateTimeValue.getCurrentDateTime(context)
            val year = now.getYear
            val tzoffset = value.getTimezoneInMinutes
            var baseDate =
              new DateTimeValue(
                year,
                1.toByte,
                1.toByte,
                0.toByte,
                0.toByte,
                0.toByte,
                0,
                tzoffset,
                false
              )
            // ORBEON: TimeZone
//            val b = NamedTimeZone.inSummerTime(baseDate, country)
            val b: jl.Boolean = false
            if (b != null && b) {
              baseDate =
                new DateTimeValue(
                  year,
                  7.toByte,
                  1.toByte,
                  0.toByte,
                  0.toByte,
                  0.toByte,
                  0,
                  tzoffset,
                  false
                )
            }
            dtv = DateTimeValue.makeDateTimeValue(baseDate.toDateValue, timeValue)
          case _ =>
            dtv = value.toDateTime
        }
        formatTimeZone(dtv, component.charAt(0), format, country)
      case 'F' =>
        if (ignoreDate) {
          val error = new XPathException("In format-time(): an xs:time value does not contain day-of-week component")
          error.setErrorCode("FOFD1350")
          error.setXPathContext(context)
          throw error
        } else {
          val day =
            DateValue.getDayOfWeek(
              dtvalue.getYear,
              dtvalue.getMonth,
              dtvalue.getDay
            )
          formatNumber(
            component,
            day,
            format,
            defaultFormat,
            numberer,
            context
          )
        }
      case 'P' =>
        if (ignoreTime) {
          val error = new XPathException("In format-date(): an xs:date value does not contain an am/pm component")
          error.setErrorCode("FOFD1350")
          error.setXPathContext(context)
          throw error
        } else {
          val minuteOfDay = dtvalue.getHour * 60 + dtvalue.getMinute
          formatNumber(
            component,
            minuteOfDay,
            format,
            defaultFormat,
            numberer,
            context
          )
        }
      case 'C' => numberer.getCalendarName("AD")
      case 'E' =>
        if (ignoreDate) {
          val error = new XPathException("In format-time(): an xs:time value does not contain an AD/BC component")
          error.setErrorCode("FOFD1350")
          error.setXPathContext(context)
          throw error
        } else {
          val year = dtvalue.getYear
          numberer.getEraName(year)
        }
      case _ =>
        val e = new XPathException("Unknown format-date/time component specifier '" + format.charAt(0) + '\'')
        e.setErrorCode("FOFD1340")
        e.setXPathContext(context)
        throw e
    }
  }

  private var formatPattern                : Pattern = Pattern.compile("([^,]*)(,.*)?")
  private val widthPattern                 : Pattern = Pattern.compile(",(\\*|[0-9]+)(\\-(\\*|[0-9]+))?")
  private var alphanumericPattern          : Pattern = Pattern.compile("([A-Za-z0-9]|\\p{L}|\\p{N})*")
  private val digitsPattern                : Pattern = Pattern.compile("\\p{Nd}+")
  private val digitsOrOptionalDigitsPattern: Pattern = Pattern.compile("[#\\p{Nd}]+")
  private val fractionalDigitsPattern      : Pattern = Pattern.compile("\\p{Nd}+#*")

  private def formatNumber(
    component    : String,
    value        : Int,
    format       : String,
    defaultFormat: Boolean,
    numberer     : Numberer,
    context      : XPathContext
  ): CharSequence = {
    var formatStr = format
    var intVal = value
    val comma     = formatStr.lastIndexOf(',')
    var widths    = ""
    if (comma >= 0) {
      widths = formatStr.substring(comma)
      formatStr = formatStr.substring(0, comma)
    }
    var primary          = formatStr
    var modifier: String = null
    if (primary.endsWith("t")) {
      primary = primary.substring(0, primary.length - 1)
      modifier = "t"
    } else if (primary.endsWith("o")) {
      primary = primary.substring(0, primary.length - 1)
      modifier = "o"
    }
    val letterValue = if ("t" == modifier) "traditional" else null
    val ordinal     =
      if ("o" == modifier)
        numberer.getOrdinalSuffixForDateTime(component)
      else
        null
    var min = 1
    var max = java.lang.Integer.MAX_VALUE
    if (digitsPattern.matcher(primary).matches()) {
      val len = StringValue.getStringLength(primary)
      if (len > 1) {
        min = len
        max = len
      }
    }
    if ("Y" == component) {
      max = 0
      min = max
      if (widths.nonEmpty) {
        max = getWidths(widths)(1)
      } else if (digitsPattern.matcher(primary).find()) {
        val uPrimary = UnicodeString.makeUnicodeString(primary)
        for (i <- 0 until uPrimary.uLength) {
          val c = uPrimary.uCharAt(i)
          if (c == '#') {
            max += 1
          } else if ((c >= '0' && c <= '9') || Categories.ESCAPE_d.test(c)) {
            min += 1
            max += 1
          }
        }
      }
      if (max <= 1) {
        max = java.lang.Integer.MAX_VALUE
      }
      if (max < 4 ||
        (max < java.lang.Integer.MAX_VALUE && intVal > 9999)) {
        intVal = intVal % Math.pow(10, max).toInt
      }
    }
    if (primary == "I" || primary == "i") {
      val range = getWidths(widths)
      min = range(0)
      val roman =
        numberer.format(
          intVal,
          UnicodeString.makeUnicodeString(primary),
          null,
          letterValue,
          ordinal
        )
      val s     = new StringBuilder(roman)
      var len   = StringValue.getStringLength(roman)
      while (len < min) {
        s.append(' ')
        len += 1
      }
      return s.toString
    } else if (widths.nonEmpty) {
      val range = getWidths(widths)
      min = Math.max(min, range(0))
      max =
        if (max == java.lang.Integer.MAX_VALUE)
          range(1)
        else
          Math.max(max, range(1))
      if (defaultFormat) {
        if (primary.endsWith("1") && min != primary.length) {
          val sb = new FastStringBuffer(min + 1)
          for (_ <- 1 until min)
            sb.cat('0')
          sb.cat('1')
          primary = sb.toString
        }
      }
    }
    if ("P" == component) {
      if (! ("N" == primary || "n" == primary || "Nn" == primary))
        primary = "n"
      if (max == java.lang.Integer.MAX_VALUE) {
        max = 4
      }
    } else if ("Y" == component) {
      if (max < java.lang.Integer.MAX_VALUE)
        intVal = intVal % Math.pow(10, max).toInt
    } else if ("f" == component) {
      val uFormat = UnicodeString.makeUnicodeString(format)
      if (! digitsPattern.matcher(primary).find()) {
        return formatNumber(component, intVal, "1", defaultFormat, numberer, context)
      }
      if (! digitsOrOptionalDigitsPattern.matcher(primary).matches()) {
        val reverseFormat = reverse(uFormat)
        val reverseValue  = reverse(UnicodeString.makeUnicodeString("" + intVal))
        val reverseResult =
          formatNumber(
            "s",
            java.lang.Integer.parseInt(reverseValue.toString),
            reverseFormat.toString,
            defaultFormat = false,
            numberer,
            context
          )
        var correctedResult = reverse(
          UnicodeString.makeUnicodeString(reverseResult))
        if (correctedResult.uLength > max)
          correctedResult = correctedResult.uSubstring(0, max)
       return correctedResult.toString
      }
      if (! fractionalDigitsPattern.matcher(primary).matches())
        throw new XPathException("Invalid picture for fractional seconds: " + primary, "FOFD1340")
      var s: StringBuilder = null
      if (intVal == 0) {
        s = new StringBuilder("0")
      } else {
        s = new StringBuilder((1000000 + intVal).toString.substring(1))
        if (s.length > max) {
          s = new StringBuilder(s.substring(0, max))
        }
      }
      while (s.length < min)
        s.append('0')
      while (s.length > min && s.charAt(s.length - 1) == '0')
        s = new StringBuilder(s.substring(0, s.length - 1))
      val zeroDigit = Alphanumeric.getDigitFamily(uFormat.uCharAt(0))
      if (zeroDigit >= 0 && zeroDigit != '0') {
        val digits = Array.ofDim[Int](10)
        var z = 0
        while (z <= 9) {
          digits(z) = zeroDigit + z
          z += 1
        }
        val n              = java.lang.Long.parseLong(s.toString)
        val requiredLength = s.length
        s = new StringBuilder(
          AbstractNumberer
            .convertDigitSystem(n, digits, requiredLength)
            .toString
        )
      }
      s.toString
    }
    if ("N" == primary || "n" == primary || "Nn" == primary) {
      var s = ""
      if ("M" == component) {
        s = numberer.monthName(intVal, min, max)
      } else if ("F" == component) {
        s = numberer.dayName(intVal, min, max)
      } else if ("P" == component) {
        s = numberer.halfDayName(intVal, min, max)
      } else {
        primary = "1"
      }
      if ("N" == primary) {
        return s.toUpperCase()
      } else if ("n" == primary) {
        return s.toLowerCase()
      } else {
        return s
      }
    }
    val picGroupFormat =
      try
        FormatInteger.getPicSeparators(primary)
      catch {
        case e: XPathException =>
          if ("FODF1310" == e.getErrorCodeLocalPart)
            e.setErrorCode("FOFD1340")
          throw e
      }
    val adjustedPicture = picGroupFormat.adjustedPicture
    var s              =
      numberer.format(intVal,
        adjustedPicture,
        picGroupFormat,
        letterValue,
        ordinal
      )
    var len = StringValue.getStringLength(s)
    var zeroDigit = 0
    if (len < min) {
      zeroDigit = Alphanumeric.getDigitFamily(adjustedPicture.uCharAt(0))
      val fsb = new FastStringBuffer(s)
      while (len < min) {
        fsb.prependWideChar(zeroDigit)
        len = len + 1
      }
      s = fsb.toString
    }
    s
  }

  private def reverse(in: UnicodeString): UnicodeString = {
    val out = Array.ofDim[Int](in.uLength)
    var i      = in.uLength - 1
    var j      = 0
    while (i >= 0) {
      out(j) = in.uCharAt(i)
      i -= 1
      j += 1
    }
    UnicodeString.makeUnicodeString(out)
  }

  private def getWidths(widths: String): Array[Int] =
    try {
      var min = -1
      var max = -1
      if ("" != widths) {
        val widthMatcher = widthPattern.matcher(widths)
        if (widthMatcher.matches()) {
          val smin = widthMatcher.group(1)
          min =
            if (smin == null || "" == smin || "*" == smin)
              1
            else
              java.lang.Integer.parseInt(smin)
          val smax = widthMatcher.group(3)
          max =
            if (smax == null || "" == smax || "*" == smax)
              java.lang.Integer.MAX_VALUE
            else
              java.lang.Integer.parseInt(smax)
          if (min < 1)
            throw new XPathException("Invalid min value in format picture " + Err.wrap(widths, Err.VALUE), "FOFD1340")
          if (max < 1 || max < min)
            throw new XPathException("Invalid max value in format picture " + Err.wrap(widths, Err.VALUE), "FOFD1340")
        } else {
          throw new XPathException("Unrecognized width specifier in format picture " + Err.wrap(widths, Err.VALUE), "FOFD1340")
        }
      }
      if (min > max) {
        val e = new XPathException("Minimum width in date/time picture exceeds maximum width")
        e.setErrorCode("FOFD1340")
        throw e
      }
      val result = Array.ofDim[Int](2)
      result(0) = min
      result(1) = max
      result
    } catch {
      case _: NumberFormatException =>
        val e = new XPathException("Invalid integer used as width in date/time picture")
        e.setErrorCode("FOFD1340")
        throw e
    }

  @tailrec
  private def formatTimeZone(
    value    : DateTimeValue,
    component: Char,
    format   : String,
    country  : String
  ): String = {
    var formatStr = format
    val comma = formatStr.lastIndexOf(',')
    var widthModifier = ""
    if (comma >= 0) {
      widthModifier = formatStr.substring(comma)
      formatStr = formatStr.substring(0, comma)
    }
    if (! value.hasTimezone) {
      if (formatStr.==("Z")) {
        return "J"
      } else {
        return ""
      }
    }
    if (formatStr.isEmpty && widthModifier.nonEmpty) {
      val widths   = getWidths(widthModifier)
      val min = widths(0)
      val max      = widths(1)
      formatStr =
        if (min <= 1)
          if (max >= 4)
            "0:00"
          else
            "0"
        else if (min <= 4)
          if (max >= 5)
            "00:00"
          else
            "00"
        else
          "00:00"
    }
    if (formatStr.isEmpty)
      formatStr = "00:00"
    var tz = value.getTimezoneInMinutes
    val useZforZero = formatStr.endsWith("t")
    if (useZforZero && tz == 0)
      return "Z"
    if (useZforZero)
      formatStr = formatStr.substring(0, formatStr.length - 1)

    var digits        = 0
    var separators    = 0
    var separatorChar: Int = ':'
    var zeroDigit     = -1

    val expandedFormat = StringValue.expand(formatStr)
    for (ch <- expandedFormat) {
      if (Character.isDigit(ch)) {
        digits += 1
        if (zeroDigit < 0)
          zeroDigit = Alphanumeric.getDigitFamily(ch)
      } else {
        separators += 1
        separatorChar = ch
      }
    }
    val buffer = Array.ofDim[Int](10)
    var used = 0
    if (digits > 0) {
      if (component == 'z') {
        buffer(0) = 'G'
        buffer(1) = 'M'
        buffer(2) = 'T'
        used = 3
      }
      val negative = tz < 0
      tz = java.lang.Math.abs(tz)
      buffer({
        used += 1
        used - 1
      }) = if (negative) '-' else '+'

      val hour   = tz / 60
      val minute = tz % 60
      val includeMinutes = minute != 0 || digits >= 3 || separators > 0
      val includeSep     = (minute != 0 && digits <= 2) || (separators > 0 && (minute != 0 || digits >= 3))
      val hourDigits     = if (digits <= 2) digits else digits - 2
      if (hour > 9 || hourDigits >= 2) {
        buffer({
          used += 1
          used - 1
        }) = zeroDigit + hour / 10
      }
      buffer({
        used += 1
        used - 1
      }) = (hour % 10) + zeroDigit
      if (includeSep) {
        buffer({
          used += 1
          used - 1
        }) = separatorChar
      }
      if (includeMinutes) {
        buffer({
          used += 1
          used - 1
        }) = minute / 10 + zeroDigit
        buffer({
          used += 1
          used - 1
        }) = minute % 10 + zeroDigit
      }
      StringValue.contract(buffer, used).toString
    } else if (formatStr.==("Z")) {
      val hour   = tz / 60
      val minute = tz % 60
      if (hour < -12 || hour > 12 || minute != 0)
        formatTimeZone(value, 'Z', "00:00", country)
      else
        Character.toString("YXWVUTSRQPONZABCDEFGHIKLM".charAt(hour + 12))
    } else if (formatStr.charAt(0) == 'N' || formatStr.charAt(0) == 'n') {
      getNamedTimeZone(value, country, formatStr)
    } else {
      formatTimeZone(value, 'Z', "00:00", country)
    }
  }

  private def getNamedTimeZone(
    value  : DateTimeValue,
    country: String,
    format : String
  ): String = {
    var min = 1
    val comma = format.indexOf(',')
    if (comma > 0) {
      val widths = format.substring(comma)
      val range  = getWidths(widths)
      min = range(0)
    }
    if (format.charAt(0) == 'N' || format.charAt(0) == 'n') {
      // ORBEON: TimeZone
      return "???"
//      if (min <= 5) {
//        var tzname = NamedTimeZone.getTimeZoneNameForDate(value, country)
//        if (format.charAt(0) == 'n')
//          tzname = tzname.toLowerCase()
//        return tzname
//      } else {
//        NamedTimeZone.getOlsenTimeZoneName(value, country)
//      }
    }
    val sbz: FastStringBuffer = new FastStringBuffer(8)
    value.appendTimezone(sbz)
    sbz.toString
  }
}

class FormatDate extends SystemFunction {

  private def adjustCalendar(
    calendarVal: StringValue,
    result     : CharSequence,
    context    : XPathContext
  ): CharSequence = {
    var cal: StructuredQName = null
    var charSeqResult = result
    try {
      val c = calendarVal.getStringValue
      cal =
        StructuredQName.fromLexicalQName(
          c,
          useDefault = false,
          allowEQName = true,
          getRetainedStaticContext
        )
    } catch {
      case e: XPathException =>
        val err = new XPathException("Invalid calendar name. " + e.getMessage)
        err.setErrorCode("FOFD1340")
        err.setXPathContext(context)
        throw err
    }
    if (cal.hasURI("")) {
      val calLocal = cal.getLocalPart
      if (calLocal == "AD" || calLocal == "ISO") {
        // NOP
      } else if (Arrays.binarySearch(knownCalendars.asInstanceOf[Array[AnyRef]], calLocal.asInstanceOf[AnyRef]) >= 0) {
        charSeqResult = "[Calendar: AD]" + charSeqResult
      } else {
        val err = new XPathException("Unknown no-namespace calendar: " + calLocal)
        err.setErrorCode("FOFD1340")
        err.setXPathContext(context)
        throw err
      }
    } else {
      charSeqResult = "[Calendar: AD]" + charSeqResult
    }
    charSeqResult
  }

  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[StringValue] = {
    val value = arguments(0).head.asInstanceOf[CalendarValue]
    if (value == null) {
      ZeroOrOne.empty
    } else {
      val format = arguments(1).head.getStringValue

      val (languageVal, calendarVal, countryVal) =
        if (getArity > 2)
          (
            arguments(2).head.asInstanceOf[StringValue],
            arguments(3).head.asInstanceOf[StringValue],
            arguments(4).head.asInstanceOf[StringValue]
          )
        else
          (null, null, null)

      val language = if (languageVal == null) null else languageVal.getStringValue
      val place    = if (countryVal == null) null else countryVal.getStringValue

      if (place != null && place.contains("/") && value.hasTimezone && ! value.isInstanceOf[TimeValue]) {
        // ORBEON: TimeZone
        ???
//        val zone = NamedTimeZone.getNamedTimeZone(place)
//        if (zone != null) {
//          // ORBEON: GregorianCalendar
////          val offset = zone.getOffset(value.toDateTime.getCalendar.getTimeInMillis)
//          val offset = zone.getOffset(value.toDateTime.secondsSinceEpoch.longValue * 1000)
//          value = value.adjustTimezone(offset / 60000)
//        }
      }
      var result = formatDate(value, format, language, place, context)
      if (calendarVal != null)
        result = adjustCalendar(calendarVal, result, context)
      new ZeroOrOne(new StringValue(result))
    }
  }
}
