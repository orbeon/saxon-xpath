////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.value

import java.{lang => jl}

import org.orbeon.saxon.expr.sort.AtomicMatchKey
import org.orbeon.saxon.lib.StringCollator
import org.orbeon.saxon.model.{AtomicType, BuiltInAtomicType}
import org.orbeon.saxon.regex.{BMPString, EmptyString, LatinString, UnicodeString}
import org.orbeon.saxon.tree.iter.AtomicIterator
import org.orbeon.saxon.tree.util.{CharSequenceConsumer, FastStringBuffer}
import org.orbeon.saxon.value.StringValue._

/**
  * An atomic value of type xs:string. This class is also used for types derived from xs:string.
  * Subclasses of StringValue are used for xs:untypedAtomic and xs:anyURI values.
  */
object StringValue {

  val EMPTY_STRING: StringValue = new StringValue(EmptyString.THE_INSTANCE)
  val SINGLE_SPACE: StringValue = new StringValue(LatinString.SINGLE_SPACE)
  val TRUE: StringValue = new StringValue(new LatinString("true"))
  val FALSE: StringValue = new StringValue(new LatinString("false"))

  /*@NotNull*/
  def makeStringValue(value: CharSequence): StringValue =
    if (value == null || value.length == 0) {
      StringValue.EMPTY_STRING
    } else {
      new StringValue(value)
    }

  def isEmpty(string: CharSequence): Boolean =
    string match {
      case str: String =>
        str.isEmpty
      case unicodeString: UnicodeString =>
        unicodeString.uLength == 0
      case _ =>
        string.length == 0
    }

  def getStringLength(s: CharSequence): Int = {
    s match {
      case string: UnicodeString =>
        return string.uLength
      case _ =>
    }
    var n: Int = 0
    for (i <- 0 until s.length) {
      val c: Int = s.charAt(i).toInt
      if (c < 55296 || c > 56319) {
// don't count high surrogates, i.e. D800 to DBFF
        { n += 1; n - 1 }
      }
    }
    n
  }

  /*@NotNull*/

  def expand(/*@NotNull*/ s: CharSequence): Array[Int] = {
    val array = new Array[Int](getStringLength(s))
    var o = 0
    var i = 0
    while (i < s.length) {
      var charval = 0
      val c = s.charAt(i)
      if (c >= 55296 && c <= 56319) { // we'll trust the data to be sound
        charval = ((c - 55296) * 1024) + (s.charAt(i + 1).asInstanceOf[Int] - 56320) + 65536
        i += 1
      }
      else charval = c
      array({
        o += 1; o - 1
      }) = charval

      i += 1
    }
    array
  }

  /*@NotNull*/

  def contract(codes: Array[Int], used: Int): CharSequence = {
    val sb = new FastStringBuffer(codes.length)
    for (i <- 0 until used)
      sb.appendWideChar(codes(i))
    sb
  }

  /*@NotNull*/

  def diagnosticDisplay(s: String): String = {
    val fsb: FastStringBuffer = new FastStringBuffer(s.length)
    for (i <- 0 until s.length) {
      val c: Char = s.charAt(i)
      if (c >= 0x20 && c <= 0x7e) {
        fsb.cat(c)
      } else {
        fsb.append("\\u")
        var shift: Int = 12
        while (shift >= 0) {
          fsb.cat("0123456789ABCDEF".charAt((c >> shift) & 0xF))
          shift -= 4
        }
      }
    }
    fsb.toString
  }

  class CharacterIterator(private var value: CharSequence)
      extends AtomicIterator[Int64Value] {

// 0-based index of the current Java char
    var inpos: Int = 0

    /*@Nullable*/

    def next(): Int64Value =
      if (inpos < value.length) {
        val c: Int = value.charAt({ inpos += 1; inpos - 1 })
        var current: Int = 0
        if (c >= 55296 && c <= 56319) {
// we'll trust the data to be sound
          try current = ((c - 55296) * 1024) +
              (value.charAt({ inpos += 1; inpos - 1 }).toInt - 56320) +
              65536
          catch {
            case e: StringIndexOutOfBoundsException =>
              System.err.println("Invalid surrogate at end of string")
              System.err.println(diagnosticDisplay(value.toString))
              throw e
          }
        } else {
          current = c
        }
        new Int64Value(current)
      } else {
        null
      }
  }

  class UnicodeCharacterIterator(value: UnicodeString)
      extends AtomicIterator[Int64Value] {

    var uValue: UnicodeString = value

// 0-based index of the current Java char
    var inpos: Int = 0

    /*@Nullable*/

    def next(): Int64Value =
      if (inpos < uValue.uLength) {
        new Int64Value(uValue.uCharAt({ inpos += 1; inpos - 1 }))
      } else {
        null
      }

  }

  class Builder extends CharSequenceConsumer {

    var buffer: FastStringBuffer = new FastStringBuffer(256)

    override def cat(chars: CharSequence): CharSequenceConsumer =
      buffer.cat(chars)

    override def cat(c: Char): CharSequenceConsumer = buffer.cat(c)

    def getStringValue: StringValue = new StringValue(buffer.condense())
  }

  // ORBEON
  def contentEquals(s: String, cs: CharSequence): Boolean = {

    def compareChars(s1: String, cs2: CharSequence): Boolean = {

      val l1 = s1.length

      if (l1 != cs2.length)
        return false

      for (i <- 0 until l1)
        if (s1.charAt(i) != cs2.charAt(i))
          return false

      true
    }

    cs match {
      case s2: String =>
        s == s2
      case b: jl.StringBuffer =>
        cs synchronized {
          compareChars(s, b)
        }
      case b =>
        compareChars(s, b)
    }
  }
}

class StringValue extends AtomicValue {

  // may be zero-length, will never be null
  var value: CharSequence = ""

  typeLabel = BuiltInAtomicType.STRING

  def this(value: CharSequence) = {
    this()
    this.value = if (value == null) "" else value
    typeLabel = BuiltInAtomicType.STRING
  }

  def this(value: CharSequence, typeLabel: AtomicType) = {
    this()
    this.value = value
    this.typeLabel = typeLabel
  }

  def setContainsNoSurrogates(): Unit =
    synchronized {
      if (!(value.isInstanceOf[BMPString] || value.isInstanceOf[LatinString] || value.isInstanceOf[EmptyString]))
        value = new BMPString(value)
    }

  def copyAsSubType(typeLabel: AtomicType): AtomicValue = {
    val v: StringValue = new StringValue(value)
    v.typeLabel = typeLabel
    v
  }

  def getPrimitiveType: BuiltInAtomicType = BuiltInAtomicType.STRING

  def getPrimitiveStringValue: CharSequence = value

  def setStringValueCS(value: CharSequence): Unit =
    this.value = value

  def getStringLength: Int = synchronized {
    if (! value.isInstanceOf[UnicodeString])
      makeUnicodeString()
    value.asInstanceOf[UnicodeString].uLength
  }

  def getStringLengthUpperBound: Int = synchronized {
    value match {
      case unicodeString: UnicodeString =>
        unicodeString.uLength
      case _ =>
        value.length
    }
  }

  def getUnicodeString: UnicodeString = synchronized {
    if (! value.isInstanceOf[UnicodeString])
      makeUnicodeString()
    value.asInstanceOf[UnicodeString]
  }

  private def makeUnicodeString(): Unit =
    value = UnicodeString.makeUnicodeString(value)

  def isZeroLength: Boolean = value.length == 0

  def containsSurrogatePairs(): Boolean =
    UnicodeString.containsSurrogatePairs(value)

  def isKnownToContainNoSurrogates: Boolean =
    value.isInstanceOf[BMPString] || value.isInstanceOf[LatinString] ||
      value.isInstanceOf[EmptyString]

  /*@NotNull*/

  def iterateCharacters(): AtomicIterator[Int64Value] = synchronized {
    value match {
      case unicodeString: UnicodeString =>
        new UnicodeCharacterIterator(unicodeString)
      case _ =>
        new CharacterIterator(value)
    }
  }

  def getXPathComparable(ordered: Boolean,
                         collator: StringCollator,
                         implicitTimezone: Int): AtomicMatchKey =
    collator.getCollationKey(value)

  override def equals(other: Any): Boolean =
    throw new ClassCastException("equals on StringValue is not allowed")

  override def hashCode: Int = value.hashCode

  def codepointEquals(other: StringValue): Boolean =
    value match {
      case str: String =>
        StringValue.contentEquals(str, other.value)
      case _ => other.value match {
        case str: String =>
          StringValue.contentEquals(str, value)
        case _ => if (value.isInstanceOf[UnicodeString]) {
          if (! other.value.isInstanceOf[UnicodeString])
            other.makeUnicodeString()
          value == other.value
        } else {
          // Avoid conversion to String unless the lengths are equal
          value.length == other.value.length && value.toString == other.value.toString
        }
      }
    }

  override def effectiveBooleanValue(): Boolean = !isZeroLength

  /*@NotNull*/
  override def toString: String = "\"" + value + '\"'

  override def toShortString: String = {
    var s = value.toString
    if (s.length > 40)
      s = s.substring(0, 35) + "..."
    "\"" + s + '\"'
  }

  def getSchemaComparable: Comparable[AnyRef] = getStringValue.asInstanceOf[Comparable[AnyRef]]

  override def isIdentical(v: AtomicValue): Boolean =
    v.isInstanceOf[StringValue]                                                     &&
      (this.isInstanceOf[AnyURIValue]        == v.isInstanceOf[AnyURIValue])        &&
      (this.isInstanceOf[UntypedAtomicValue] == v.isInstanceOf[UntypedAtomicValue]) &&
      codepointEquals(v.asInstanceOf[StringValue])
}
