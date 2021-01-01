package org.orbeon.saxon.expr.number

import org.orbeon.saxon.lib.Numberer

import org.orbeon.saxon.regex.EmptyString

import org.orbeon.saxon.regex.UnicodeString

import org.orbeon.saxon.regex.charclass.Categories

import org.orbeon.saxon.tree.util.FastStringBuffer

import java.math.BigInteger

import java.util.ArrayList

import java.util.List

import java.util.function.IntPredicate

import NumberFormatter._

import scala.util.control.Breaks._

object NumberFormatter {

  def isLetterOrDigit(c: Int): Boolean =
    if (c <= 0x7F) {
      (c >= 0x30 && c <= 0x39) || (c >= 0x41 && c <= 0x5A) ||
        (c >= 0x61 && c <= 0x7A)
    } else {
      alphanumeric.test(c)
    }

  private var alphanumeric: IntPredicate =
    Categories.getCategory("N").or(Categories.getCategory("L"))

}

class NumberFormatter {

  private var formatTokens: ArrayList[UnicodeString] = _

  private var punctuationTokens: ArrayList[UnicodeString] = _

  private var startsWithPunctuation: Boolean = _

  def prepare(format: String): Unit = {
    var strFormat = format
    if (strFormat.isEmpty) {
      strFormat = "1"
    }
    formatTokens = new ArrayList[UnicodeString](10)
    punctuationTokens = new ArrayList[UnicodeString](10)
    val uFormat: UnicodeString = UnicodeString.makeUnicodeString(strFormat)
    val len = uFormat.uLength
    var i: Int = 0
    var t: Int = 0
    var first: Boolean = true
    startsWithPunctuation = true
    breakable {
      while (i < len) {
        var c: Int = uFormat.uCharAt(i)
        t = i

        while (isLetterOrDigit(c)) {
          i += 1
          if (i == len) break()
          c = uFormat.uCharAt(i)
        }

        if (i > t) {
          val tok: UnicodeString = uFormat.uSubstring(t, i)
          formatTokens.add(tok)
          if (first) {
            punctuationTokens.add(UnicodeString.makeUnicodeString("."))
            startsWithPunctuation = false
            first = false
          }
        }
        if (i == len) break()
        t = i
        c = uFormat.uCharAt(i)
        breakable {
          while (!isLetterOrDigit(c)) {
            first = false
            i += 1
            if (i == len) break()
            c = uFormat.uCharAt(i)
          }
        }
        if (i > t) {
          val sep: UnicodeString = uFormat.uSubstring(t, i)
          punctuationTokens.add(sep)
        }
      }
    }
    if (formatTokens.isEmpty) {
      formatTokens.add(UnicodeString.makeUnicodeString("1"))
      if (punctuationTokens.size == 1) {
        punctuationTokens.add(punctuationTokens.get(0))
      }
    }
  }

  def format(numbers: List[_],
             groupSize: Int,
             groupSeparator: String,
             letterValue: String,
             ordinal: String,
             numberer: Numberer): CharSequence = {
    val sb = new FastStringBuffer(FastStringBuffer.C16)
    var num: Int = 0
    var tok: Int = 0
    if (startsWithPunctuation) {
      sb.append(punctuationTokens.get(tok))
    }
    while (num < numbers.size) {
      if (num > 0) {
        if (tok == 0 && startsWithPunctuation) {
          sb.append(".")
        } else {
          sb.append(punctuationTokens.get(tok))
        }
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
      if (tok == formatTokens.size) {
          tok -= 1
      }
    }
    if (punctuationTokens.size > formatTokens.size) {
      sb.append(punctuationTokens.get(punctuationTokens.size - 1))
    }
    sb.condense()
  }

  private def translateDigits(in: String, picture: UnicodeString): String = {
    if (picture.length == 0) {
      return in
    }
    val formchar: Int = picture.uCharAt(0)
    val digitValue: Int = Alphanumeric.getDigitValue(formchar)
    if (digitValue >= 0) {
      val zero: Int = formchar - digitValue
      if (zero == '0'.toInt) {
        return in
      }
      val digits: Array[Int] = Array.ofDim[Int](10)
      var z: Int = 0
      while (z <= 9) {
        digits(z) = zero + z
        z += 1
      }
      val sb = new FastStringBuffer(128)
      for (i <- 0 until in.length) {
        val c: Char = in.charAt(i)
        if (c >= '0' && c <= '9') {
          sb.appendWideChar(digits(c - '0'))
        } else {
          sb.cat(c)
        }
      }
      sb.toString
    } else {
      in
    }
  }

}
