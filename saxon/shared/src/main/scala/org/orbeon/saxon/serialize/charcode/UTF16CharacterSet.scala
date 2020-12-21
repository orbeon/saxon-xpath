package org.orbeon.saxon.serialize.charcode

import java.util.function.IntPredicate

object UTF16CharacterSet {

  private var theInstance: UTF16CharacterSet = new UTF16CharacterSet()

  def getInstance: UTF16CharacterSet = theInstance

  val NONBMP_MIN: Int = 0x10000

  val NONBMP_MAX: Int = 0x10FFFF

  val SURROGATE1_MIN: Char = 0xD800

  val SURROGATE1_MAX: Char = 0xDBFF

  val SURROGATE2_MIN: Char = 0xDC00

  val SURROGATE2_MAX: Char = 0xDFFF

  def combinePair(high: Char, low: Char): Int =
    (high - SURROGATE1_MIN) * 0x400 + (low - SURROGATE2_MIN) +
      NONBMP_MIN

  def highSurrogate(ch: Int): Char =
    (((ch - NONBMP_MIN) >> 10) + SURROGATE1_MIN).toChar

  def lowSurrogate(ch: Int): Char =
    (((ch - NONBMP_MIN) & 0x3FF) + SURROGATE2_MIN).toChar

  def isSurrogate(c: Int): Boolean = (c & 0xF800) == 0xD800

  def isHighSurrogate(ch: Int): Boolean =
    (SURROGATE1_MIN <= ch && ch <= SURROGATE1_MAX)

  def isLowSurrogate(ch: Int): Boolean =
    (SURROGATE2_MIN <= ch && ch <= SURROGATE2_MAX)

  def containsSurrogates(s: CharSequence): Boolean = {
    for (i <- 0 until s.length) {
      if (isSurrogate(s.charAt(i))) return true
    }
    false
  }


  def firstInvalidChar(chars: CharSequence, predicate: IntPredicate): Int = {
    for (c <- 0 until chars.length) {
      var ch = c
      var ch32: Int = chars.charAt(ch)
      if (UTF16CharacterSet.isHighSurrogate(ch32)) {
        val low: Char = chars.charAt({
          ch += 1;
          ch - 1
        })
        ch32 = UTF16CharacterSet.combinePair(ch32.toChar, low)
      }
      if (!predicate.test(ch32)) {
        return ch32
      }
    }
    -1
  }

  def main(args: Array[String]): Unit = {
    System.err.println(java.lang.Integer.toHexString(highSurrogate(0xeffff)))
    System.err.println(java.lang.Integer.toHexString(lowSurrogate(0xeffff)))
  }

}

class UTF16CharacterSet private() extends CharacterSet {

  def inCharset(c: Int): Boolean = true

  def getCanonicalName(): String = "UTF-16"

}
