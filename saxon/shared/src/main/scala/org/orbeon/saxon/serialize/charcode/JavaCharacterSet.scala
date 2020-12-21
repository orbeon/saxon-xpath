//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//// Copyright (c) 2018-2020 Saxonica Limited
//// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
//// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
//// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//package org.orbeon.saxon.serialize.charcode
//
//import java.nio.charset.{Charset, CharsetEncoder}
//import java.util.HashMap
//
//import org.orbeon.saxon.serialize.charcode.JavaCharacterSet._
//import org.orbeon.saxon.tree.tiny.CharSlice
//
//
///**
// * This class establishes properties of a character set that is
// * known to the Java VM but not specifically known to Saxon. It determines whether particular
// * characters are encodable by calling {@link CharsetEncoder#canEncode(char)}, and then caches
// * this information locally.
// */
//object JavaCharacterSet {
//
//  private var map: HashMap[Charset, JavaCharacterSet] = _
//
//  //private final static byte UNKNOWN = 0;
//  private val GOOD: Byte = 1
//  private val BAD: Byte = 2
//
//  def makeCharSet(charset: Charset): JavaCharacterSet = synchronized {
//
//    if (map == null)
//      map = new HashMap[Charset, JavaCharacterSet](10)
//
//    var c = map.get(charset)
//    if (c == null) {
//      c = new JavaCharacterSet(charset)
//      map.put(charset, c)
//    }
//    c
//  }
//}
//
//class JavaCharacterSet private(charset: Charset) extends CharacterSet {
//
//  private val encoder: CharsetEncoder = charset.newEncoder()
//  private val charinfo: Array[Byte] = new Array[Byte](65536)
//
//  def inCharset(c: Int): Boolean = {
//    // Assume ASCII chars are always OK
//    if (c <= 127) {
//      return true
//    }
//    if (c <= 65535) {
//      if (charinfo(c) == GOOD) {
//        true
//      } else if (charinfo(c) == BAD) {
//        false
//      } else {
//        if (encoder.canEncode(c.toChar)) {
//          charinfo(c) = GOOD
//          true
//        } else {
//          charinfo(c) = BAD
//          false
//        }
//      }
//    } else {
//      val cc: Array[Char] = Array.ofDim[Char](2)
//      cc(0) = UTF16CharacterSet.highSurrogate(c)
//      cc(1) = UTF16CharacterSet.lowSurrogate(c)
//      encoder.canEncode(new CharSlice(cc))
//    }
//  }
//
//  def getCanonicalName: String = encoder.charset().name()
//}
