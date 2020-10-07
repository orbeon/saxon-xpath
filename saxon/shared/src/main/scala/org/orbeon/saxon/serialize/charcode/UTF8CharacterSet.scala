////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.serialize.charcode

import UTF8CharacterSet._




object UTF8CharacterSet {

  private var theInstance: UTF8CharacterSet = new UTF8CharacterSet()

  def getInstance: UTF8CharacterSet = theInstance

  def getUTF8Encoding(in: Char, in2: Char, out: Array[Byte]): Int = {
// See Tony Graham, "Unicode, a Primer", page 92
    val i: Int = in.toInt
    if (i <= 0x7f) {
      out(0) = i.toByte
      1
    } else if (i <= 0x7ff) {
      out(0) = (0xc0 | ((in >> 6) & 0x1f)).toByte
      out(1) = (0x80 | (in & 0x3f)).toByte
      2
    } else if (i >= 0xd800 && i <= 0xdbff) {
// surrogate pair
      val j: Int = in2.toInt
      if (!(j >= 0xdc00 && j <= 0xdfff)) {
        throw new IllegalArgumentException(
          "Malformed Unicode Surrogate Pair (" + i + ',' + j + ')')
      }
      val xxxxxx: Byte = (j & 0x3f).toByte
      val yyyyyy: Byte = (((i & 0x03) << 4) | ((j >> 6) & 0x0f)).toByte
      val zzzz: Byte = ((i >> 2) & 0x0f).toByte
      val uuuuu: Byte = (((i >> 6) & 0x0f) + 1).toByte
      out(0) = (0xf0 | ((uuuuu >> 2) & 0x07)).toByte
      out(1) = (0x80 | ((uuuuu & 0x03) << 4) | zzzz).toByte
      out(2) = (0x80 | yyyyyy).toByte
      out(3) = (0x80 | xxxxxx).toByte
      4
    } else if (i >= 0xdc00 && i <= 0xdfff) {
// second half of surrogate pair - ignore it
      0
    } else {
      out(0) = (0xe0 | ((in >> 12) & 0x0f)).toByte
      out(1) = (0x80 | ((in >> 6) & 0x3f)).toByte
      out(2) = (0x80 | (in & 0x3f)).toByte
      3
    }
  }

  def decodeUTF8(in: Array[Byte], used: Int): Int = {
    var bottom: Int = 0
    for (i <- 1 until used) {
      if ((in(i) & 0xc0) != 0x80) {
        throw new IllegalArgumentException(
          "Byte " + (i + 1) + " in UTF-8 sequence has wrong top bits")
      }
      bottom = (bottom << 6) + (in(i) & 0x3f)
    }
    if ((in(0) & 0x80) == 0) {
// single byte sequence 0xxxxxxx
      if (used == 1) {
        in(0)
      } else {
        throw new IllegalArgumentException("UTF8 single byte expected")
      }
    } else if ((in(0) & 0xe0) == 0xc0) {
// two byte sequence
      if (used != 2) {
        throw new IllegalArgumentException(
          "UTF8 sequence of two bytes expected")
      }
      ((in(0) & 0x1f) << 6) + bottom
    } else if ((in(0) & 0xf0) == 0xe0) {
// three byte sequence
      if (used != 3) {
        throw new IllegalArgumentException(
          "UTF8 sequence of three bytes expected")
      }
      ((in(0) & 0x0f) << 12) + bottom
    } else if ((in(0) & 0xf8) == 0xf8) {
// four-byte sequence 11110uuu 10uuzzzz 10yyyyyy 10xxxxxx
      if (used != 4) {
        throw new IllegalArgumentException(
          "UTF8 sequence of four bytes expected")
      }
      ((in(0) & 0x07) << 24) + bottom
    } else {
      throw new IllegalArgumentException("UTF8 invalid first byte")
    }
  }

}

class UTF8CharacterSet private () extends CharacterSet {

  def inCharset(c: Int): Boolean = true

  /*@NotNull*/

  def getCanonicalName(): String = "UTF-8"

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class defines properties of the UTF-8 character set
  */
