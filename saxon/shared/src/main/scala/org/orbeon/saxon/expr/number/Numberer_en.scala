package org.orbeon.saxon.expr.number

import Numberer_en._

import  AbstractNumberer._

object Numberer_en {

  private var englishUnits: Array[String] = Array(
    "Zero",
    "One",
    "Two",
    "Three",
    "Four",
    "Five",
    "Six",
    "Seven",
    "Eight",
    "Nine",
    "Ten",
    "Eleven",
    "Twelve",
    "Thirteen",
    "Fourteen",
    "Fifteen",
    "Sixteen",
    "Seventeen",
    "Eighteen",
    "Nineteen"
  )

  private var englishTens: Array[String] = Array("",
    "Ten",
    "Twenty",
    "Thirty",
    "Forty",
    "Fifty",
    "Sixty",
    "Seventy",
    "Eighty",
    "Ninety")

  private var englishOrdinalUnits: Array[String] = Array(
    "Zeroth",
    "First",
    "Second",
    "Third",
    "Fourth",
    "Fifth",
    "Sixth",
    "Seventh",
    "Eighth",
    "Ninth",
    "Tenth",
    "Eleventh",
    "Twelfth",
    "Thirteenth",
    "Fourteenth",
    "Fifteenth",
    "Sixteenth",
    "Seventeenth",
    "Eighteenth",
    "Nineteenth"
  )

  private var englishOrdinalTens: Array[String] = Array("",
    "Tenth",
    "Twentieth",
    "Thirtieth",
    "Fortieth",
    "Fiftieth",
    "Sixtieth",
    "Seventieth",
    "Eightieth",
    "Ninetieth")

  private var englishMonths: Array[String] = Array("January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December")

  private var englishDays: Array[String] = Array("Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday",
    "Sunday")

  private var englishDayAbbreviations: Array[String] =
    Array("Mon", "Tues", "Weds", "Thurs", "Fri", "Sat", "Sun")

  private var minUniqueDayLength: Array[Int] = Array(1, 2, 1, 2, 1, 2, 2)

}

class Numberer_en extends AbstractNumberer {

  private var tensUnitsSeparatorCardinal: String = " "

  private var tensUnitsSeparatorOrdinal: String = "-"

  def setTensUnitsSeparatorCardinal(separator: String): Unit = {
    tensUnitsSeparatorCardinal = separator
  }

  def setTensUnitsSeparatorOrdinal(separator: String): Unit = {
    tensUnitsSeparatorOrdinal = separator
  }

 override def setLanguage(language: String): Unit = {
    super.setLanguage(language)
    if (language.endsWith("-x-hyphen")) {
      this.tensUnitsSeparatorOrdinal = "-"
      this.tensUnitsSeparatorCardinal = "-"
    } else if (language.endsWith("-x-nohyphen")) {
      this.tensUnitsSeparatorOrdinal = " "
      this.tensUnitsSeparatorCardinal = " "
    }
  }

  override  def ordinalSuffix(ordinalParam: String, number: Long): String = {
    val penult: Int = (number % 100).toInt / 10
    val ult: Int = (number % 10).toInt
    if (penult == 1) {
      "th"
    } else {
      if (ult == 1) {
        "st"
      } else if (ult == 2) {
        "nd"
      } else if (ult == 3) {
        "rd"
      } else {
        "th"
      }
    }
  }

  def toWords(number: Long): String =
    if (number >= 1000000000) {
      val rem: Long = number % 1000000000
      toWords(number / 1000000000) + " Billion" +
        (if (rem == 0) ""
        else (if (rem < 100) " and " else " ") + toWords(rem))
    } else if (number >= 1000000) {
      val rem: Long = number % 1000000
      toWords(number / 1000000) + " Million" +
        (if (rem == 0) ""
        else (if (rem < 100) " and " else " ") + toWords(rem))
    } else if (number >= 1000) {
      val rem: Long = number % 1000
      toWords(number / 1000) + " Thousand" +
        (if (rem == 0) ""
        else (if (rem < 100) " and " else " ") + toWords(rem))
    } else if (number >= 100) {
      val rem: Long = number % 100
      toWords(number / 100) + " Hundred" + (if (rem == 0) ""
      else " and " + toWords(rem))
    } else {
      if (number < 20) {
        return englishUnits(number.toInt)
      }
      val rem: Int = (number % 10).toInt
      englishTens(number.toInt / 10) +
        (if (rem == 0) "" else tensUnitsSeparatorCardinal + englishUnits(rem))
    }

  def toOrdinalWords(ordinalParam: String,
                     number: Long,
                     wordCase: Int): String = {
    var s: String = null
    if (number >= 1000000000) {
      val rem: Long = number % 1000000000
      s = toWords(number / 1000000000) + " Billion" +
        (if (rem == 0) "th"
        else
          (if (rem < 100) " and " else " ") + toOrdinalWords(ordinalParam,
            rem,
            wordCase))
    } else if (number >= 1000000) {
      val rem: Long = number % 1000000
      s = toWords(number / 1000000) + " Million" +
        (if (rem == 0) "th"
        else
          (if (rem < 100) " and " else " ") + toOrdinalWords(ordinalParam,
            rem,
            wordCase))
    } else if (number >= 1000) {
      val rem: Long = number % 1000
      s = toWords(number / 1000) + " Thousand" +
        (if (rem == 0) "th"
        else
          (if (rem < 100) " and " else " ") + toOrdinalWords(ordinalParam,
            rem,
            wordCase))
    } else if (number >= 100) {
      val rem: Long = number % 100
      s = toWords(number / 100) + " Hundred" +
        (if (rem == 0) "th"
        else " and " + toOrdinalWords(ordinalParam, rem, wordCase))
    } else {
      if (number < 20) {
        s = englishOrdinalUnits(number.toInt)
      } else {
        val rem: Int = (number % 10).toInt
        s =
          if (rem == 0) englishOrdinalTens(number.toInt / 10)
          else
            englishTens(number.toInt / 10) + tensUnitsSeparatorOrdinal +
              englishOrdinalUnits(rem)
      }
    }
    if (wordCase == UPPER_CASE) {
      s.toUpperCase()
    } else if (wordCase == LOWER_CASE) {
      s.toLowerCase()
    } else {
      s
    }
  }

  def monthName(month: Int, minWidth: Int, maxWidth: Int): String = {
    var maximumWidth = maxWidth
    var name: String = englishMonths(month - 1)
    if (maximumWidth < 3) {
      maximumWidth = 3
    }
    if (name.length > maximumWidth) {
      name = name.substring(0, maximumWidth)
    }
    val nameBuilder: StringBuilder = new StringBuilder(name)
    while (nameBuilder.length < minWidth) nameBuilder.append(' ')
    name = nameBuilder.toString
    name
  }

  def dayName(day: Int, minWidth: Int, maxWidth: Int): String = {
    var maximumWidth = maxWidth
    var name: String = englishDays(day - 1)
    if (maximumWidth < 2) {
      maximumWidth = 2
    }
    if (name.length > maximumWidth) {
      name = englishDayAbbreviations(day - 1)
      if (name.length > maximumWidth) {
        name = name.substring(0, maximumWidth)
      }
    }
    val nameBuilder: StringBuilder = new StringBuilder(name)
    while (nameBuilder.length < minWidth) nameBuilder.append(' ')
    name = nameBuilder.toString
    if (minWidth == 1 && maximumWidth == 2) {
      name = name.substring(0, minUniqueDayLength(day - 1))
    }
    name
  }

}
