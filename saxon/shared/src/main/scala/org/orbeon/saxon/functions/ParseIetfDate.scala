


package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Callable
import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.om.{Item, Sequence, ZeroOrOne}
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.value._
import java.util


object ParseIetfDate {
  @throws[XPathException]
  private def badDate(msg: String, value: String) = {
    val err = new XPathException("Invalid IETF date value " + value + " (" + msg + ")")
    err.setErrorCode("FORG0010")
    throw err
  }


  def isValidTime(hour: Int, minute: Int, second: Int, microsecond: Int, tz: Int) = (hour >= 0 && hour <= 23 && minute >= 0 && minute < 60 && second >= 0 && second < 60 && microsecond >= 0 && microsecond < 1000000 || hour == 24 && minute == 0 && second == 0 && microsecond == 0) && tz >= -14 * 60 && tz <= 14 * 60

  private val EOF = ""
}

class ParseIetfDate extends SystemFunction {

  @throws[XPathException]
  override def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[_ <: Item] = {
    val stringValue = arguments(0).head.asInstanceOf[StringValue]
    if (stringValue == null)
      ZeroOrOne.empty
    else
      new ZeroOrOne(parse(stringValue.getStringValue, context))
  }

  private val dayNames = Array[String]("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

  private def isDayName(string: String): Boolean = {
    for (s <- dayNames) {
      if (s.equalsIgnoreCase(string)) return true
    }
    false
  }

  private val monthNames = Array[String]("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

  private def isMonthName(string: String): Boolean = {
    for (s <- monthNames) {
      if (s.equalsIgnoreCase(string)) return true
    }
    false
  }

  private def getMonthNumber(string: String): Byte = {
    if ("Jan".equalsIgnoreCase(string)) return 1.toByte
    else if ("Feb".equalsIgnoreCase(string)) return 2.toByte
    else if ("Mar".equalsIgnoreCase(string)) return 3.toByte
    else if ("Apr".equalsIgnoreCase(string)) return 4.toByte
    else if ("May".equalsIgnoreCase(string)) return 5.toByte
    else if ("Jun".equalsIgnoreCase(string)) return 6.toByte
    else if ("Jul".equalsIgnoreCase(string)) return 7.toByte
    else if ("Aug".equalsIgnoreCase(string)) return 8.toByte
    else if ("Sep".equalsIgnoreCase(string)) return 9.toByte
    else if ("Oct".equalsIgnoreCase(string)) return 10.toByte
    else if ("Nov".equalsIgnoreCase(string)) return 11.toByte
    else if ("Dec".equalsIgnoreCase(string)) return 12.toByte
    0.toByte
  }

  @throws[XPathException]
  private def requireDSep(tokens: util.List[String], i: Int, input: String) = {
    var found = false
    var in = i
    if (" " == tokens.get(in)) {
      in += 1
      found = true
    }
    if ("-" == tokens.get(in)) {
      in += 1
      found = true
    }
    if (" " == tokens.get(in)) {
      in += 1
      found = true
    }
    if (!found) ParseIetfDate.badDate("Date separator missing", input)
    in
  }

  private val timezoneNames = Array[String]("UT", "UTC", "GMT", "EST", "EDT", "CST", "CDT", "MST", "MDT", "PST", "PDT")

  private def isTimezoneName(string: String): Boolean = {
    for (s <- timezoneNames) {
      if (s.equalsIgnoreCase(string)) return true
    }
    false
  }

  private def getTimezoneOffsetFromName(string: String): Int = {
    if ("UT".equalsIgnoreCase(string) | "UTC".equalsIgnoreCase(string) | "GMT".equalsIgnoreCase(string)) return 0
    else if ("EST".equalsIgnoreCase(string)) return -5 * 60
    else if ("EDT".equalsIgnoreCase(string)) return -4 * 60
    else if ("CST".equalsIgnoreCase(string)) return -6 * 60
    else if ("CDT".equalsIgnoreCase(string)) return -5 * 60
    else if ("MST".equalsIgnoreCase(string)) return -7 * 60
    else if ("MDT".equalsIgnoreCase(string)) return -6 * 60
    else if ("PST".equalsIgnoreCase(string)) return -8 * 60
    else if ("PDT".equalsIgnoreCase(string)) return -7 * 60
    0
  }


  @throws[XPathException]
  def parse(input: String, context: XPathContext) = {
    val tokens = tokenize(input)
    var year = 0
    var month = 0
    var day = 0
    val timeValue = new util.ArrayList[TimeValue]
    var i = 0
    var currentToken = tokens.get(i)
    if (currentToken.matches("[A-Za-z]+") && isDayName(currentToken)) {
      currentToken = tokens.get({
        i += 1;
        i
      })
      if ("," == currentToken) currentToken = tokens.get({
        i += 1;
        i
      })
      if (!(" " == currentToken)) ParseIetfDate.badDate("Space missing after day name", input)
      currentToken = tokens.get({
        i += 1;
        i
      })

    }
    if (isMonthName(currentToken)) {
      month = getMonthNumber(currentToken)
      i = requireDSep(tokens, i + 1, input)
      currentToken = tokens.get(i)
      if (!currentToken.matches("[0-9]+")) ParseIetfDate.badDate("Day number expected after month name", input)
      if (currentToken.length > 2) ParseIetfDate.badDate("Day number exceeds two digits", input)
      day = currentToken.toInt.toByte
      currentToken = tokens.get({
        i += 1;
        i
      })
      if (!(" " == currentToken)) ParseIetfDate.badDate("Space missing after day number", input)
      i = parseTime(tokens, {
        i += 1;
        i
      }, timeValue, input)
      currentToken = tokens.get({
        i += 1;
        i
      })
      if (!(" " == currentToken)) ParseIetfDate.badDate("Space missing after time string", input)
      currentToken = tokens.get({
        i += 1;
        i
      })
      if (currentToken.matches("[0-9]+")) year = checkTwoOrFourDigits(input, currentToken)
      else ParseIetfDate.badDate("Year number expected after time", input)
    }
    else if (currentToken.matches("[0-9]+")) {
      if (currentToken.length > 2) ParseIetfDate.badDate("First number in string expected to be day in two digits", input)
      day = currentToken.toInt.toByte
      i = requireDSep(tokens, {
        i += 1;
        i
      }, input)
      currentToken = tokens.get(i)
      if (!isMonthName(currentToken)) ParseIetfDate.badDate("Abbreviated month name expected after day number", input)
      month = getMonthNumber(currentToken)
      i = requireDSep(tokens, {
        i += 1;
        i
      }, input)
      currentToken = tokens.get(i)
      if (currentToken.matches("[0-9]+")) year = checkTwoOrFourDigits(input, currentToken)
      else ParseIetfDate.badDate("Year number expected after month name", input)
      currentToken = tokens.get({
        i += 1;
        i
      })
      if (!(" " == currentToken)) ParseIetfDate.badDate("Space missing after year number", input)
      i = parseTime(tokens, {
        i += 1;
        i
      }, timeValue, input)
    }
    else ParseIetfDate.badDate("String expected to begin with month name or day name (or day number)", input)
    if (!GDateValue.isValidDate(year, month, day)) ParseIetfDate.badDate("Date is not valid", input)
    currentToken = tokens.get({
      i += 1;
      i
    })
    if (!(currentToken == ParseIetfDate.EOF)) ParseIetfDate.badDate("Extra content found in string after date", input)
    var date = new DateValue(year, month.toByte, day.toByte)
    var time = timeValue.get(0)
    if (time.getHour == 24) {
      date = DateValue.tomorrow(date.getYear, date.getMonth, date.getDay)
      time = new TimeValue(0.toByte, 0.toByte, 0.toByte, 0, time.getTimezoneInMinutes, "")
    }
    DateTimeValue.makeDateTimeValue(date, time)
  }

  @throws[XPathException]
  private def checkTwoOrFourDigits(input: String, currentToken: String) = {
    var year = 0
    if (currentToken.length == 4) year = currentToken.toInt
    else if (currentToken.length == 2) year = currentToken.toInt + 1900
    else {
      ParseIetfDate.badDate("Year number must be two or four digits", input)
      year = 0
    }
    year
  }


  @throws[XPathException]
  def parseTime(tokens: util.List[String], currentPosition: Int, result: util.List[TimeValue], input: String) = {
    var hour = 0
    var minute = 0
    var second = 0
    var microsecond = 0

    var tz = 0

    var i = currentPosition
    var n = currentPosition

    var currentToken = new StringBuilder(tokens.get(i))
    if (!currentToken.toString.matches("[0-9]+")) ParseIetfDate.badDate("Hour number expected", input)
    if (currentToken.length > 2) ParseIetfDate.badDate("Hour number exceeds two digits", input)
    hour = currentToken.toString.toInt.toByte
    currentToken = new StringBuilder(tokens.get({
      i += 1;
      i
    }))
    if (!(":" == currentToken.toString)) ParseIetfDate.badDate("Separator ':' missing after hour", input)
    currentToken = new StringBuilder(tokens.get({
      i += 1;
      i
    }))
    if (!currentToken.toString.matches("[0-9]+")) ParseIetfDate.badDate("Minutes expected after hour", input)
    if (currentToken.length != 2) ParseIetfDate.badDate("Minutes must be exactly two digits", input)
    minute = currentToken.toString.toInt.toByte
    currentToken = new StringBuilder(tokens.get({
      i += 1;
      i
    }))
    var finished = false
    if (currentToken.toString == ParseIetfDate.EOF) {
      n = i - 1
      finished = true
    }
    else if (":" == currentToken.toString) {
      currentToken = new StringBuilder(tokens.get({
        i += 1;
        i
      }))
      if (!currentToken.toString.matches("[0-9]+")) ParseIetfDate.badDate("Seconds expected after ':' separator after minutes", input)
      if (currentToken.length != 2) ParseIetfDate.badDate("Seconds number must have exactly two digits (before decimal point)", input)
      second = currentToken.toString.toInt.toByte
      currentToken = new StringBuilder(tokens.get({
        i += 1;
        i
      }))
      if (currentToken.toString == ParseIetfDate.EOF) {
        n = i - 1
        finished = true
      }
      else if ("." == currentToken.toString) {
        currentToken = new StringBuilder(tokens.get({
          i += 1;
          i
        }))
        if (!currentToken.toString.matches("[0-9]+")) ParseIetfDate.badDate("Fractional part of seconds expected after decimal point", input)
        val len = Math.min(6, currentToken.length)
        currentToken = new StringBuilder(currentToken.substring(0, len))
        while ( {
          currentToken.length < 6
        }) currentToken.append("0")
        microsecond = currentToken.toString.toInt
        if (i < tokens.size - 1) currentToken = new StringBuilder(tokens.get({
          i += 1;
          i
        }))
      }
    }
    if (!finished) {
      if (" " == currentToken.toString) {
        currentToken = new StringBuilder(tokens.get({
          i += 1;
          i
        }))
        if (currentToken.toString.matches("[0-9]+")) {
          n = i - 2
          finished = true
        }
      }
      if (!finished) if (currentToken.toString.matches("[A-Za-z]+")) {
        if (!isTimezoneName(currentToken.toString)) ParseIetfDate.badDate("Timezone name not recognised", input)
        tz = getTimezoneOffsetFromName(currentToken.toString)
        n = i
        finished = true
      }
      else if ("+" == currentToken.toString | "-" == currentToken.toString) {
        val sign = currentToken.toString
        var tzOffsetHours = 0
        var tzOffsetMinutes = 0
        currentToken = new StringBuilder(tokens.get({
          i += 1;
          i
        }))
        if (!currentToken.toString.matches("[0-9]+")) ParseIetfDate.badDate("Parsing timezone offset, number expected after '" + sign + "'", input)
        val tLength = currentToken.length
        if (tLength > 4) ParseIetfDate.badDate("Timezone offset does not have the correct number of digits", input)
        else if (tLength >= 3) {
          tzOffsetHours = currentToken.substring(0, tLength - 2).toInt
          tzOffsetMinutes = currentToken.substring(tLength - 2, tLength).toInt
          currentToken = new StringBuilder(tokens.get({
            i += 1;
            i
          }))
        }
        else {
          tzOffsetHours = currentToken.toString.toInt
          currentToken = new StringBuilder(tokens.get({
            i += 1;
            i
          }))
          if (":" == currentToken.toString) {
            currentToken = new StringBuilder(tokens.get({
              i += 1;
              i
            }))
            if (currentToken.toString.matches("[0-9]+")) {
              if (currentToken.length != 2) ParseIetfDate.badDate("Parsing timezone offset, minutes must be two digits", input)
              else tzOffsetMinutes = currentToken.toString.toInt
              currentToken = new StringBuilder(tokens.get({
                i += 1;
                i
              }))
            }
          }
        }
        if (tzOffsetMinutes > 59) ParseIetfDate.badDate("Timezone offset minutes out of range", input)
        tz = tzOffsetHours * 60 + tzOffsetMinutes
        if (sign == "-") tz = -tz
        if (currentToken.toString == ParseIetfDate.EOF) {
          n = i - 1
          finished = true
        }
        else if (" " == currentToken.toString) {
          currentToken = new StringBuilder(tokens.get({
            i += 1;
            i
          }))
          if (currentToken.toString.matches("[0-9]+")) {
            n = i - 2
            finished = true
          }
        }
        if (!finished && "(" == currentToken.toString) {
          currentToken = new StringBuilder(tokens.get({
            i += 1;
            i
          }))
          if (" " == currentToken.toString) currentToken = new StringBuilder(tokens.get({
            i += 1;
            i
          }))
          if (!currentToken.toString.matches("[A-Za-z]+")) ParseIetfDate.badDate("Timezone name expected after '('", input)
          else if (currentToken.toString.matches("[A-Za-z]+")) {
            if (!isTimezoneName(currentToken.toString)) ParseIetfDate.badDate("Timezone name not recognised", input)
            currentToken = new StringBuilder(tokens.get({
              i += 1;
              i
            }))
          }
          if (" " == currentToken.toString) currentToken = new StringBuilder(tokens.get({
            i += 1;
            i
          }))
          if (!(")" == currentToken.toString)) ParseIetfDate.badDate("Expected ')' after timezone name", input)
          n = i
          finished = true
        }
        else if (!finished) ParseIetfDate.badDate("Unexpected content after timezone offset", input)
      }
      else ParseIetfDate.badDate("Unexpected content in time (after minutes)", input)
    }
    if (!finished) throw new AssertionError("Should have finished")
    if (!ParseIetfDate.isValidTime(hour, minute, second, microsecond, tz)) ParseIetfDate.badDate("Time/timezone is not valid", input)
    val timeValue = new TimeValue(hour.toByte, minute.toByte, second.toByte, microsecond * 1000, tz, "")
    result.add(timeValue)
    n
  }

  @throws[XPathException]
  private def tokenize(input: String): util.List[String] = {
    val tokens = new util.ArrayList[String]
    var inputStr = input
    inputStr = inputStr.trim
    if (input.isEmpty) {
      ParseIetfDate.badDate("Input is empty", inputStr)
      return tokens
    }
    var i = 0
    inputStr = inputStr + 0.toChar
    while (true) {
      val c = inputStr.charAt(i)
      if (c == 0) {
        tokens.add(ParseIetfDate.EOF)
        return tokens
      }
      if (Whitespace.isWhite(c)) {
        var j = i
        while ( {
          Whitespace.isWhite(inputStr.charAt({
            j += 1;
            j - 1
          }))
        }) {
        }
        tokens.add(" ")
        i = j - 1
      }
      else if (Character.isLetter(c)) {
        var j = i
        while ( {
          Character.isLetter(inputStr.charAt({
            j += 1;
            j - 1
          }))
        }) {
        }
        tokens.add(inputStr.substring(i, j - 1))
        i = j - 1
      }
      else if (Character.isDigit(c)) {
        var j = i
        while ( {
          Character.isDigit(inputStr.charAt({
            j += 1;
            j - 1
          }))
        }) {
        }
        tokens.add(inputStr.substring(i, j - 1))
        i = j - 1
      }
      else {
        tokens.add(inputStr.substring(i, i + 1))
        i += 1
      }
    }
    null
  }
}