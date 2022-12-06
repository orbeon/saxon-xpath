package org.orbeon.saxon.expr.number

import org.orbeon.saxon.expr.number.NumberFormatter._
import org.orbeon.saxon.lib.Numberer
import org.orbeon.saxon.regex.{EmptyString, UnicodeString}
import org.orbeon.saxon.tree.util.FastStringBuffer

import java.math.BigInteger
import java.util
import java.util.function.IntPredicate
import scala.util.control.Breaks._


object NumberFormatter {

  def isLetterOrDigit(c: Int): Boolean =
    if (c <= 0x7F) {
      (c >= 0x30 && c <= 0x39) || (c >= 0x41 && c <= 0x5A) ||
        (c >= 0x61 && c <= 0x7A)
    } else {
      alphanumeric.test(c)
    }

  // ORBEON: Use standard Java `Character` vs. Saxon `Categories`
  private val alphanumeric: IntPredicate = Character.isLetterOrDigit
}

class NumberFormatter {

  private var formatTokens: util.ArrayList[UnicodeString] = _
  private var punctuationTokens: util.ArrayList[UnicodeString] = _
  private var startsWithPunctuation: Boolean = _

  def prepare(format: String): Unit = {
    var strFormat = format
    if (strFormat.isEmpty)
      strFormat = "1"
    formatTokens = new util.ArrayList[UnicodeString](10)
    punctuationTokens = new util.ArrayList[UnicodeString](10)

    val uFormat = UnicodeString.makeUnicodeString(strFormat)
    val len     = uFormat.uLength

    var i = 0
    var t = 0
    var first  = true
    startsWithPunctuation = true

    breakable {
      while (i < len) {
        var c = uFormat.uCharAt(i)
        t = i

        while (isLetterOrDigit(c)) {
          i += 1
          if (i == len)
            break()
          c = uFormat.uCharAt(i)
        }

        if (i > t) {
          val tok = uFormat.uSubstring(t, i)
          formatTokens.add(tok)
          if (first) {
            punctuationTokens.add(UnicodeString.makeUnicodeString("."))
            startsWithPunctuation = false
            first = false
          }
        }
        if (i == len)
          break()
        t = i
        c = uFormat.uCharAt(i)
        breakable {
          while (!isLetterOrDigit(c)) {
            first = false
            i += 1
            if (i == len)
              break()
            c = uFormat.uCharAt(i)
          }
        }
        if (i > t) {
          val sep = uFormat.uSubstring(t, i)
          punctuationTokens.add(sep)
        }
      }
    }
    if (formatTokens.isEmpty) {
      formatTokens.add(UnicodeString.makeUnicodeString("1"))
      if (punctuationTokens.size == 1)
        punctuationTokens.add(punctuationTokens.get(0))
    }
  }

  def format(
    numbers       : util.List[_],
    groupSize     : Int,
    groupSeparator: String,
    letterValue   : String,
    ordinal       : String,
    numberer      : Numberer
  ): CharSequence = {
    val sb = new FastStringBuffer(FastStringBuffer.C16)
    var num = 0
    var tok = 0
    if (startsWithPunctuation)
      sb.append(punctuationTokens.get(tok))
    while (num < numbers.size) {
      if (num > 0) {
        if (tok == 0 && startsWithPunctuation)
          sb.append(".")
        else
          sb.append(punctuationTokens.get(tok))
      }
      num += 1
      val o: Any = numbers.get(num)
      var s: String = null
      if (o.isInstanceOf[java.lang.Long]) {
        val nr: Long = o.asInstanceOf[java.lang.Long]
        val rgf: RegularGroupFormatter = new RegularGroupFormatter(
          groupSize,
          groupSeparator,
          EmptyString.THE_INSTANCE)
        s =
          numberer.format(nr, formatTokens.get(tok), rgf, letterValue, ordinal)
      } else if (o.isInstanceOf[BigInteger]) {
        val fsb = new FastStringBuffer(FastStringBuffer.C64)
        fsb.append(o.toString)
        val rgf: RegularGroupFormatter = new RegularGroupFormatter(
          groupSize,
          groupSeparator,
          EmptyString.THE_INSTANCE)
        s = rgf.format(fsb)
        s = translateDigits(s, formatTokens.get(tok))
      } else {
        s = o.toString
      }
      sb.append(s)
      tok += 1
      if (tok == formatTokens.size)
        tok -= 1
    }
    if (punctuationTokens.size > formatTokens.size)
      sb.append(punctuationTokens.get(punctuationTokens.size - 1))
    sb.condense()
  }

  private def translateDigits(in: String, picture: UnicodeString): String = {
    if (picture.length == 0)
      return in
    val formchar   = picture.uCharAt(0)
    val digitValue = Alphanumeric.getDigitValue(formchar)
    if (digitValue >= 0) {
      val zero = formchar - digitValue
      if (zero == '0'.toInt)
        return in
      val digits = Array.ofDim[Int](10)
      var z = 0
      while (z <= 9) {
        digits(z) = zero + z
        z += 1
      }
      val sb = new FastStringBuffer(128)
      for (i <- 0 until in.length) {
        val c = in.charAt(i)
        if (c >= '0' && c <= '9')
          sb.appendWideChar(digits(c - '0'))
        else
          sb.cat(c)
      }
      sb.toString
    } else {
      in
    }
  }
}
