package org.orbeon.saxon.expr.number

import java.util.Locale

import org.orbeon.saxon.expr.number.AbstractNumberer._
import org.orbeon.saxon.lib.Numberer
import org.orbeon.saxon.regex.{EmptyString, UnicodeString}
import org.orbeon.saxon.tree.util.FastStringBuffer

object AbstractNumberer {

  val UPPER_CASE: Int = 0
  val LOWER_CASE: Int = 1
  val TITLE_CASE: Int = 2

  val westernDigits: Array[Int] = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')

  val latinUpper: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  val latinLower: String = "abcdefghijklmnopqrstuvwxyz"
  val greekUpper: String = "ΑΒΓΔΕΖΗΘΙΚ" + "ΛΜΝΞΟΠΡ΢ΣΤ" + "ΥΦΧΨΩ"
  val greekLower: String = "αβγδεζηθικ" + "λμνξοπρςστ" + "υφχψω"
  val cyrillicUpper: String = "АБВГДЕЖЗИ" + "КЛМНОПРССУ" + "ФХЦЧШЩЫЭЮЯ"
  val cyrillicLower: String = "абвгдежзи" + "клмнопрссу" + "фхцчшщыэюя"
  val hebrew: String = "אבגדהוזחטיכל" + "מנסעפצקרשת"

  val hiraganaA: String = "あいうえおかきくけこ" + "さしすせそたちつてと" + "なにぬねのはひふへほ" + "まみむめもやゆよらり" +
    "るれろわをん"

  val katakanaA: String = "アイウエオカキクケコ" + "サシスセソタチツテト" + "ナニヌネノハヒフヘホ" + "マミムメモヤユヨラリ" +
    "ルレロワヲン"

  val hiraganaI: String = "いろはにほへとちりぬ" + "るをわかよたれそつね" + "ならむうゐのおくやま" + "けふこえてあさきゆめ" +
    "みしゑひもせす"

  val katakanaI: String = "イロハニホヘトチリヌ" + "ルヲワカヨタレソツネ" + "ナラムウヰノオクヤマ" + "ケフコエテアサキユメ" +
    "ミシヱヒモセス"

  def convertDigitSystem(number: Long,
                         digits: Array[Int],
                         requiredLength: Int): FastStringBuffer = {
    val temp: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C16)
    val base: Int = digits.length
    val s: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C16)
    var n: Long = number
    var count: Int = 0
    while (n > 0) {
      val digit: Int = digits((n % base).toInt)
      s.prependWideChar(digit)
      count += 1
      n = n / base
    }
    for (i <- 0 until (requiredLength - count)) {
      temp.appendWideChar(digits(0))
    }
    temp.append(s)
    temp
  }

  def toRoman(n: Long): String = {
    if (n <= 0 || n > 9999) {
      return "" + n
    }
    romanThousands(n.toInt / 1000) + romanHundreds((n.toInt / 100) % 10) +
      romanTens((n.toInt / 10) % 10) +
      romanUnits(n.toInt % 10)
  }

  private var romanThousands: Array[String] = Array("",
    "m",
    "mm",
    "mmm",
    "mmmm",
    "mmmmm",
    "mmmmmm",
    "mmmmmmm",
    "mmmmmmmm",
    "mmmmmmmmm")

  private var romanHundreds: Array[String] =
    Array("", "c", "cc", "ccc", "cd", "d", "dc", "dcc", "dccc", "cm")

  private var romanTens: Array[String] =
    Array("", "x", "xx", "xxx", "xl", "l", "lx", "lxx", "lxxx", "xc")

  private var romanUnits: Array[String] =
    Array("", "i", "ii", "iii", "iv", "v", "vi", "vii", "viii", "ix")

  private def toJapanese(nr: Int,
                         fsb: FastStringBuffer,
                         isInitial: Boolean): Unit = {
    if (nr == 0) {} else if (nr <= 9) {
      if (!(nr == 1 && isInitial)) {
        fsb.appendWideChar(kanjiDigits(nr))
      }
    } else if (nr == 10) {
      fsb.appendWideChar(0x5341)
    } else if (nr <= 99) {
      toJapanese(nr / 10, fsb, isInitial = true)
      fsb.appendWideChar(0x5341)
      toJapanese(nr % 10, fsb, isInitial = false)
    } else if (nr <= 999) {
      toJapanese(nr / 100, fsb, isInitial = true)
      fsb.appendWideChar(0x767e)
      toJapanese(nr % 100, fsb, isInitial = false)
    } else if (nr <= 9999) {
      toJapanese(nr / 1000, fsb, isInitial = true)
      fsb.appendWideChar(0x5343)
      toJapanese(nr % 1000, fsb, isInitial = false)
    }
  }

  private val kanjiDigits: Array[Int] = Array(0x3007, 0x4e00, 0x4e8c, 0x4e09,
    0x56db, 0x4e94, 0x516d, 0x4e03, 0x516b, 0x4e5d)

}

abstract class AbstractNumberer extends Numberer {

  var country: String = _

  var language: String = _

  def getLanguage: String = language

  def setLanguage(value: String): Unit = {
    language = value
  }

  def defaultedLocale(): Locale = null

  def format(number: Long,
             picture: UnicodeString,
             groupSize: Int,
             groupSeparator: String,
             letterValue: String,
             ordinal: String): String =
    format(number,
      picture,
      new RegularGroupFormatter(groupSize,
        groupSeparator,
        EmptyString.THE_INSTANCE),
      letterValue,
      ordinal)

  def format(number: Long,
             picture: UnicodeString,
             numGroupFormatter: NumericGroupFormatter,
             letterValue: String,
             ordinal: String): String = {
    if (number < 0) {
      return "" + number
    }
    if (picture == null || picture.uLength == 0) {
      return "" + number
    }
    val pictureLength: Int = picture.uLength
    val sb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C16)
    val formchar: Int = picture.uCharAt(0)
    val fsb: FastStringBuffer = new FastStringBuffer(2)
    formchar match {
      case '0' | '1' =>
        sb.append(
          toRadical(number, westernDigits, pictureLength, numGroupFormatter))
        if (ordinal != null && !ordinal.isEmpty) {
          sb.append(ordinalSuffix(ordinal, number))
        }
      case 'A' =>
        if (number == 0) {
          return "0"
        }
        toAlphaSequence(number, latinUpper)
      case 'a' =>
        if (number == 0) {
          return "0"
        }
        toAlphaSequence(number, latinLower)
      case 'w' | 'W' =>
        var wordCase: Int = 0
        wordCase =
          if (picture.uLength == 1)
            if (formchar == 'W') UPPER_CASE else LOWER_CASE
          else TITLE_CASE
        if (ordinal != null && !ordinal.isEmpty) {
          toOrdinalWords(ordinal, number, wordCase)
        } else {
          toWords(number, wordCase)
        }
      case 'i' =>
        if (number == 0) {
          return "0"
        }
        if (letterValue == null || letterValue.isEmpty || letterValue.==(
          "traditional")) {
          toRoman(number)
        } else {
          alphaDefault(number, 'i', sb)
        }
      case 'I' =>
        if (number == 0) {
          return "0"
        }
        if (letterValue == null || letterValue.isEmpty || letterValue.==(
          "traditional")) {
          toRoman(number).toUpperCase()
        } else {
          alphaDefault(number, 'I', sb)
        }
      case '①' =>
        if (number == 0) {
          return "" + 0x24EA.toChar
        }
        if (number > 20 && number <= 35) {
          return "" + (0x3251 + number - 21).toChar
        }
        if (number > 35 && number <= 50) {
          return "" + (0x32B1 + number - 36).toChar
        }
        if (number > 50) {
          return "" + number
        }
        return "" + (0x2460 + number - 1).toChar
      case '⑴' =>
        if (number == 0 || number > 20) {
          return "" + number
        }
        return "" + (0x2474 + number - 1).toChar
      case '⒈' =>
        if (number == 0) {
          return "" + 0xD83C.toChar + 0xDD00.toChar
        }
        if (number > 20) {
          return "" + number
        }
        return "" + (0x2488 + number - 1).toChar
      case '❶' =>
        if (number == 0) {
          return "" + 0x24FF.toChar
        }
        if (number > 10 && number <= 20) {
          return "" + (0x24EB + number - 11).toChar
        }
        if (number > 20) {
          return "" + number
        }
        return "" + (0x2776 + number - 1).toChar
      case '➀' =>
        if (number == 0) {
          return "" + 0xD83C.toChar + 0xDD0B.toChar
        }
        if (number > 10) {
          return "" + number
        }
        return "" + (0x2780 + number - 1).toChar
      case '⓵' =>
        if (number == 0 || number > 10) {
          return "" + number
        }
        return "" + (0x24F5 + number - 1).toChar
      case '➊' =>
        if (number == 0) {
          return "" + 0xD83C.toChar + 0xDD0C.toChar
        }
        if (number > 10) {
          return "" + number
        }
        return "" + (0x278A + number - 1).toChar
      case '㈠' =>
        if (number == 0 || number > 10) {
          return "" + number
        }
        return "" + (0x3220 + number - 1).toChar
      case '㊀' =>
        if (number == 0 || number > 10) {
          return "" + number
        }
        return "" + (0x3280 + number - 1).toChar
      case 65799 =>
        if (number == 0 || number > 10) {
          return "" + number
        }
        fsb.appendWideChar(65799 + number.toInt - 1)
        fsb.toString
      case 69216 =>
        if (number == 0 || number > 10) {
          return "" + number
        }
        fsb.appendWideChar(69216 + number.toInt - 1)
        fsb.toString
      case 69714 =>
        if (number == 0 || number > 10) {
          return "" + number
        }
        fsb.appendWideChar(69714 + number.toInt - 1)
        fsb.toString
      case 119648 =>
        if (number == 0 || number >= 10) {
          return "" + number
        }
        fsb.appendWideChar(119648 + number.toInt - 1)
        fsb.toString
      case 127234 =>
        if (number == 0) {
          fsb.appendWideChar(127233)
          fsb.toString
        }
        if (number >= 10) {
          return "" + number
        }
        fsb.appendWideChar(127234 + number.toInt - 1)
        fsb.toString
      case 'Α' =>
        if (number == 0) {
          return "0"
        }
        toAlphaSequence(number, greekUpper)
      case 'α' =>
        if (number == 0) {
          return "0"
        }
        toAlphaSequence(number, greekLower)
      case 'А' =>
        if (number == 0) {
          return "0"
        }
        toAlphaSequence(number, cyrillicUpper)
      case 'а' =>
        if (number == 0) {
          return "0"
        }
        toAlphaSequence(number, cyrillicLower)
      case 'א' =>
        if (number == 0) {
          return "0"
        }
        toAlphaSequence(number, hebrew)
      case 'あ' =>
        if (number == 0) {
          return "0"
        }
        toAlphaSequence(number, hiraganaA)
      case 'ア' =>
        if (number == 0) {
          return "0"
        }
        toAlphaSequence(number, katakanaA)
      case 'い' =>
        if (number == 0) {
          return "0"
        }
        toAlphaSequence(number, hiraganaI)
      case 'イ' =>
        if (number == 0) {
          return "0"
        }
        toAlphaSequence(number, katakanaI)
      case '一' => toJapanese(number)
      case _ =>
        var digitValue: Int = Alphanumeric.getDigitValue(formchar)
        if (digitValue >= 0) {
          val zero: Int = formchar - digitValue
          val digits: Array[Int] = Array.ofDim[Int](10)
          var z: Int = 0
          while (z <= 9) {
            digits(z) = zero + z
            z += 1
          }
          toRadical(number, digits, pictureLength, numGroupFormatter)
        } else {
          if (formchar < 'ᄀ' && java.lang.Character.isLetter(formchar.toChar) &&
            number > 0) {
            alphaDefault(number, formchar.toChar, sb)
          } else {
            sb.append(
              toRadical(number,
                westernDigits,
                pictureLength,
                numGroupFormatter))
            if (ordinal != null && !ordinal.isEmpty) {
              sb.append(ordinalSuffix(ordinal, number))
            }
          }
        }

    }
    sb.toString
  }

  def ordinalSuffix(ordinalParam: String, number: Long): String = ""

  def alphaDefault(number: Long,
                   formchar: Char,
                   sb: FastStringBuffer): Unit = {
    val min: Int = formchar.toInt
    var max: Int = formchar.toInt
    while (java.lang.Character.isLetterOrDigit((max + 1).toChar)) {
      max += 1;
      max - 1
    }
    sb.append(toAlpha(number, min, max))
  }

  def toAlpha(number: Long, min: Int, max: Int): String = {
    if (number <= 0) {
      return "" + number
    }
    val range: Int = max - min + 1
    val last: Char = (((number - 1) % range) + min).toChar
    if (number > range) {
      toAlpha((number - 1) / range, min, max) + last
    } else {
      "" + last
    }
  }

  def toAlphaSequence(number: Long, alphabet: String): String = {
    if (number <= 0) {
      return "" + number
    }
    val range: Int = alphabet.length
    val last: Char = alphabet.charAt(((number - 1) % range).toInt)
    if (number > range) {
      toAlphaSequence((number - 1) / range, alphabet) + last
    } else {
      "" + last
    }
  }

  private def toRadical(number: Long,
                        digits: Array[Int],
                        pictureLength: Int,
                        numGroupFormatter: NumericGroupFormatter): String = {
    val temp: FastStringBuffer =
      convertDigitSystem(number, digits, pictureLength)
    if (numGroupFormatter == null) {
      return temp.toString
    }
    numGroupFormatter.format(temp)
  }

  def toJapanese(number: Long): String = {
    val fsb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C16)
    if (number == 0) {
      fsb.appendWideChar(0x3007)
    } else if (number <= 9999) {
      AbstractNumberer.toJapanese(number.toInt, fsb, isInitial = false)
    } else {
      fsb.append("" + number)
    }
    fsb.toString
  }

  def toWords(number: Long): String

  def toWords(number: Long, wordCase: Int): String = {
    var s: String = null
    s = if (number == 0) "Zero" else toWords(number)
    wordCase match {
      case UPPER_CASE => s.toUpperCase()
      case LOWER_CASE => s.toLowerCase()
      case _ => s

    }
  }

  def toOrdinalWords(ordinalParam: String, number: Long, wordCase: Int): String

  def monthName(month: Int, minWidth: Int, maxWidth: Int): String

  def dayName(day: Int, minWidth: Int, maxWidth: Int): String

  def halfDayName(minutes: Int, minWidth: Int, maxWidth: Int): String = {
    var s: String = null
    if (minutes == 0 && maxWidth >= 8 && "gb" == country) {
      s = "Midnight"
    } else if (minutes < 12 * 60) {
      maxWidth match {
        case 1 => s = "A"
        case 2 | 3 => s = "Am"
        case _ => s = "A.M."

      }
    } else if (minutes == 12 * 60 && maxWidth >= 8 && "gb" == country) {
      s = "Noon"
    } else {
      maxWidth match {
        case 1 => s = "P"
        case 2 | 3 => s = "Pm"
        case _ => s = "P.M."

      }
    }
    s
  }

  def setCountry(country: String): Unit = this.country = country

  def getCountry: String = country

  def getOrdinalSuffixForDateTime(component: String): String = "yes"

  def getEraName(year: Int): String = if (year > 0) "AD" else "BC"

  def getCalendarName(code: String): String =
    if (code.==("AD"))
      "Gregorian"
    else
      code
}
