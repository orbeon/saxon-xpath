////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.regex

import org.orbeon.saxon.expr.sort.AtomicMatchKey
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.value.{AtomicValue, Base64BinaryValue}


/**
  * An abstract class that efficiently handles Unicode strings including
  * non-BMP characters; it has three subclasses, respectively handling
  * strings whose maximum character code is 255, 65535, or 1114111.
  */
object UnicodeString {

  def makeUnicodeString(in: CharSequence): UnicodeString = {
    in match {
      case string: UnicodeString =>
        return string
      case _ =>
    }
    if (in.length == 0)
      return EmptyString.THE_INSTANCE

    val width = getMaxWidth(in)
    if (width == 1) {
      new LatinString(in)
    } else if (width == 2) {
      new BMPString(in)
    } else {
      new GeneralUnicodeString(in)
    }
  }

  def makeUnicodeString(in: Array[Int]): UnicodeString = {
    for (ch <- in if ch > 65535) {
      new GeneralUnicodeString(in, 0, in.length)
    }
    val fsb: FastStringBuffer = new FastStringBuffer(in.length)
    for (ch <- in) {
      fsb.cat(ch.toChar)
    }
    new BMPString(fsb)
  }

  def containsSurrogatePairs(value: CharSequence): Boolean =
    if (value.isInstanceOf[BMPString] || value.isInstanceOf[LatinString] ||
        value.isInstanceOf[EmptyString]) {
      false
    } else value match {
      case gus: GeneralUnicodeString =>
        for (i <- 0 until gus.uLength() if gus.uCharAt(i) >= 65535)
          return true
        false
      case _ =>
        for (i <- 0 until value.length) {
          val c: Int = value.charAt(i).toInt
          if (c >= 55296 && c <= 56319) {
            true
          }
        }
        false
    }

  private def getMaxWidth(value: CharSequence): Int = {
    if (value.isInstanceOf[LatinString] || value.isInstanceOf[EmptyString]) {
      return 1
    }
    if (value.isInstanceOf[BMPString]) {
      return 2
    }
    if (value.isInstanceOf[GeneralUnicodeString]) {
      return 4
    }
    var nonLatin: Boolean = false
    for (i <- 0 until value.length) {
      val c: Int = value.charAt(i).toInt
      if (c > 255) {
        nonLatin = true
      }
      if (c >= 55296 && c <= 56319) {
        4
      }
    }
    if (nonLatin) 2 else 1
  }

}

abstract class UnicodeString
    extends CharSequence
    with Comparable[UnicodeString]
    with AtomicMatchKey {

  private var cachedHash: Int = -1

  /**
    * Get a substring of this string
    *
    * @param beginIndex the index of the first character to be included (counting
    *                   codepoints, not 16-bit characters)
    * @param endIndex   the index of the first character to be NOT included (counting
    *                   codepoints, not 16-bit characters)
    * @return a substring
    * @throws IndexOutOfBoundsException if the selection goes off the start or end of the string
    *         (this function follows the semantics of String.substring(), not the XPath semantics)
    */
  def uSubstring(beginIndex: Int, endIndex: Int): UnicodeString

  def uIndexOf(search: Int, start: Int): Int

  def uCharAt(pos: Int): Int

  def uLength: Int

  def isEnd(pos: Int): Boolean

  override def hashCode: Int = {
    // Same result as String#hashCode in the case where all characters are BMP chars
    if (cachedHash == -1) {
      var h: Int = 0
      for (i <- 0 until uLength) {
        h = 31 * h + uCharAt(i)
      }
      cachedHash = h
    }
    cachedHash
  }

  override def equals(obj: Any): Boolean = {
    if (! obj.isInstanceOf[UnicodeString])
      return false
    if (uLength != obj.asInstanceOf[UnicodeString].uLength)
      return false
    for (i <- 0 until uLength
         if uCharAt(i) != obj.asInstanceOf[UnicodeString].uCharAt(i))
      return false
    true
  }

  def compareTo(other: UnicodeString): Int = {
    val alen: Int = uLength
    val blen: Int = other.uLength
    var i: Int = 0
    var j: Int = 0
    while (true) {
      if (i == alen) {
        if (j == blen) {
          return 0
        } else {
          return -1
        }
      }
      if (j == blen) {
        return +1
      }
      val nexta: Int = uCharAt({ i += 1; i - 1 })
      val nextb: Int = other.uCharAt({ j += 1; j - 1 })
      val c: Int = nexta - nextb
      if (c != 0) {
        return c
      }
    }
    0
  }

  private def getCodepointCollationKey: Array[Byte] = {
    val len = uLength
    val result: Array[Byte] = Array.ofDim[Byte](len * 3)
    var j: Int =0
    for (i <- 0 until len) {
      val c: Int = uCharAt(i)
      result({ j += 1; j - 1 }) = (c >> 16).toByte
      result({ j += 1; j - 1 }) = (c >> 8).toByte
      result({ j += 1; j - 1 }) = c.toByte
    }
    result
  }

  /**
    * Get an atomic value that encapsulates this match key. Needed to support the collation-key() function.
    *
    * @return an atomic value that encapsulates this match key
    */
  override def asAtomic(): AtomicValue =
    new Base64BinaryValue(getCodepointCollationKey)

}
