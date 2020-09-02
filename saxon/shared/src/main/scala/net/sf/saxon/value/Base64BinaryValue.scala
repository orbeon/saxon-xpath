////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.value

import net.sf.saxon.expr.sort.AtomicMatchKey
import net.sf.saxon.lib.StringCollator
import net.sf.saxon.model.AtomicType
import net.sf.saxon.model.BuiltInAtomicType
import net.sf.saxon.om.SequenceTool
import net.sf.saxon.serialize.charcode.UTF16CharacterSet
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.util.FastStringBuffer
import java.util
import scala.util.control.Breaks._

/**
 * A value of type xs:base64Binary
 * <p><i>Rewritten for Saxon 9.5 to avoid dependency on the open-source Netscape code, whose
 * license many users were unhappy with.</i></p>
 */
object Base64BinaryValue {
  def byteArrayHashCode(/*@NotNull*/ value: Array[Byte]) = {
    var h = 0
    for (i <- 0 until Math.min(value.length, 64)) {
      h = (h << 1) ^ value(i)
    }
    ((h >> 32) ^ h).toInt
  }

  private val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  private val encoding = new Array[Int](64)
  private val decoding = new Array[Int](128)

  /**
   * Encode a byte sequence into base64 representation
   *
   * @param value the byte sequence
   * @return the base64 representation
   */
  def encode(value: Array[Byte]) = {
    val buff = new FastStringBuffer(value.length)
    val whole = value.length - value.length % 3
    // process bytes 3 at a time: 3 bytes => 4 characters
    var i = 0
    while ( {
      i < whole
    }) { // 3 bytes = 24 bits = 4 characters
      val `val` = ((value(i).toInt & 0xff) << 16) + ((value(i + 1).toInt & 0xff) << 8) + ((value(i + 2).toInt) & 0xff)
      buff.cat(encoding((`val` >> 18) & 0x3f).toChar)
      buff.cat(encoding((`val` >> 12) & 0x3f).toChar)
      buff.cat(encoding((`val` >> 6) & 0x3f).toChar)
      buff.cat(encoding(`val` & 0x3f).toChar)
      i += 3
    }
    val remainder = value.length % 3
    remainder match {
      case 0 =>
      // no action
      case 1 =>
        // pad the final 8 bits to 12 (2 groups of 6)
        val `val` = ((value(whole).toInt) & 0xff) << 4
        buff.cat(encoding((`val` >> 6) & 0x3f).toChar)
        buff.cat(encoding(`val` & 0x3f).toChar)
        buff.append("==")
      case 2 =>
        // pad the final 16 bits to 18 (3 groups of 6)
        val `val` = ((value(whole).toInt & 0xff) << 10) + ((value(whole + 1).toInt & 0xff) << 2)
        buff.cat(encoding((`val` >> 12) & 0x3f).toChar)
        buff.cat(encoding((`val` >> 6) & 0x3f).toChar)
        buff.cat(encoding(`val` & 0x3f).toChar)
        buff.append("=")
      case _ =>
    }
    buff.condense
  }

  /**
   * Decode a character string in base64 notation to yield the encoded octets
   *
   * @param in the lexical representation
   * @return the array of octets represented
   * @throws XPathException if the format is invalid (as required by XSD, this method
   *                        does draconian error handling, unlike many other base64 decoders which are liberal
   *                        in what they accept)
   */
  @throws[XPathException]
  def decode(in: CharSequence) = {
    val unit = new Array[Char](4)
    var result = new Array[Byte](in.length)
    var bytesUsed = 0
    var i = 0
    var u = 0
    var pad = 0
    var chars = 0
    var last = 0
    // process characters 4 at a time: 4 characters => 3 bytes
    breakable {
      while ( {
        i < in.length
      }) {
        val c = in.charAt({
          i += 1;
          i - 1
        })
        if (!Whitespace.isWhite(c)) {
          chars += 1
          if (c == '=') { // all following chars must be '=' or whitespace
            pad = 1
            for (k <- i until in.length) {
              val ch = in.charAt(k)
              if (ch == '=') {
                pad += 1
                chars += 1
              }
              else if (Whitespace.isWhite(ch)) {
              }
              else throw new XPathException("Base64 padding character '=' is followed by non-padding characters", "FORG0001")
            }
            if (pad == 1 && "AEIMQUYcgkosw048".indexOf(last) < 0) throw new XPathException("In base64, if the value ends with a single '=' character, then the preceding character must be" + " one of [AEIMQUYcgkosw048]", "FORG0001")
            else if (pad == 2 && "AQgw".indexOf(last) < 0) throw new XPathException("In base64, if the value ends with '==', then the preceding character must be" + " one of [AQgw]", "FORG0001")
            // number of padding characters must be the number required
            if (pad > 2) throw new XPathException("Found " + pad + " '=' characters at end of base64 value; max is 2", "FORG0001")
            if (pad != ((4 - u) % 4)) throw new XPathException("Required " + ((4 - u) % 4) + " '=' characters at end of base64 value; found " + pad, "FORG0001")
            // append 0 sextets corresponding to number of padding characters
            for (p <- 0 until pad) {
              unit({
                u += 1;
                u - 1
              }) = 'A'
            }
            i = in.length
          }
          else {
            last = c
            unit({
              u += 1;
              u - 1
            }) = c
          }
          if (u == 4) {
            val t = (decodeChar(unit(0)) << 18) + (decodeChar(unit(1)) << 12) + (decodeChar(unit(2)) << 6) + decodeChar(unit(3))
            if (bytesUsed + 3 > result.length) {
              val r2 = new Array[Byte](bytesUsed * 2)
              System.arraycopy(result, 0, r2, 0, bytesUsed)
              result = r2
            }
            result({
              bytesUsed += 1;
              bytesUsed - 1
            }) = ((t >> 16) & 0xff).toByte
            result({
              bytesUsed += 1;
              bytesUsed - 1
            }) = ((t >> 8) & 0xff).toByte
            result({
              bytesUsed += 1;
              bytesUsed - 1
            }) = (t & 0xff).toByte
            u = 0
          }
        }
        if (i >= in.length) {
          bytesUsed -= pad
          break()
        }
      }
    }
    if (chars % 4 != 0)
      throw new XPathException("Length of base64 value must be a multiple of four", "FORG0001")
    val r3 = new Array[Byte](bytesUsed)
    System.arraycopy(result, 0, r3, 0, bytesUsed)
    r3
  }

  @throws[XPathException]
  private def decodeChar(c: Char) = {
    val d = if (c < 128) decoding(c)
    else -1
    if (d == -1) if (UTF16CharacterSet.isSurrogate(c)) throw new XPathException("Invalid character (surrogate pair) in base64 value", "FORG0001")
    else throw new XPathException("Invalid character '" + c + "' in base64 value", "FORG0001")
    d
  }

  util.Arrays.fill(decoding, -1)
  for (i <- 0 until alphabet.length) {
    val c = alphabet.charAt(i)
    encoding(i) = c
    decoding(c) = i
  }
}

class Base64BinaryValue extends AtomicValue with AtomicMatchKey with Comparable[Any] {
  private var binaryValue: Array[Byte] = null

  /**
   * Constructor: create a base64Binary value from a supplied string in base64 encoding
   *
   * @param s the lexical representation of the base64 binary value. There is no requirement
   *          that whitespace should already be collapsed.
   * @throws XPathException
   * if the supplied value is not in the lexical
   * space of the xs:base64Binary data type
   */
  def this(s: CharSequence) {
    this()
    binaryValue = Base64BinaryValue.decode(s)
    typeLabel = BuiltInAtomicType.BASE64_BINARY
  }

  /**
   * Constructor: create a base64Binary value from a given array of bytes
   *
   * @param value array of bytes holding the octet sequence
   */
  def this(value: Array[Byte]) {
    this()
    binaryValue = value
    typeLabel = BuiltInAtomicType.BASE64_BINARY
  }

  /**
   * Create a copy of this atomic value (usually so that the type label can be changed).
   * The type label of the copy will be reset to the primitive type.
   *
   * @param typeLabel the type label to be attached to the value, a subtype of xs:base64Binary
   * @return the copied value
   */
  override def copyAsSubType(typeLabel: AtomicType) = {
    val v = new Base64BinaryValue(binaryValue)
    v.typeLabel = typeLabel
    v
  }

  /**
   * Get the binary value
   *
   * @return the octet sequence that is the typed value
   */
  def getBinaryValue: Array[Byte] = binaryValue

  override def getPrimitiveType = BuiltInAtomicType.BASE64_BINARY

  /**
   * Convert to string
   *
   * @return the canonical representation.
   */
  override def getPrimitiveStringValue = Base64BinaryValue.encode(binaryValue).toString

  /**
   * Get the number of octets in the value
   *
   * @return the number of octets
   */
  def getLengthInOctets = binaryValue.length

  /**
   * Support XML Schema comparison semantics
   */
  val base64Bin = new Base64BinaryValue

  override def getSchemaComparable: Comparable[AnyRef] = new base64Bin.Base64BinaryComparable

  /**
   * Private inner class to support XML Schema comparison semantics
   */
  class Base64BinaryComparable extends Comparable[AnyRef] {
    def getBase64BinaryValue = Base64BinaryValue.this

    override def compareTo(o: AnyRef): Int = if (o.isInstanceOf[Base64BinaryValue#Base64BinaryComparable] && util.Arrays.equals(getBase64BinaryValue.binaryValue, o.asInstanceOf[Base64BinaryValue#Base64BinaryComparable].getBase64BinaryValue.binaryValue)) 0
    else SequenceTool.INDETERMINATE_ORDERING

    @SuppressWarnings(Array("EqualsWhichDoesntCheckParameterClass")) override def equals(o: Any): Boolean = compareTo(o.asInstanceOf[AnyRef]) == 0

    override def hashCode = Base64BinaryValue.this.hashCode
  }

  /**
   * Get an object value that implements the XPath equality and ordering comparison semantics for this value.
   * If the ordered parameter is set to true, the result will be a Comparable and will support a compareTo()
   * method with the semantics of the XPath lt/gt operator, provided that the other operand is also obtained
   * using the getXPathComparable() method. In all cases the result will support equals() and hashCode methods
   * that support the semantics of the XPath eq operator, again provided that the other operand is also obtained
   * using the getXPathComparable() method. A context argument is supplied for use in cases where the comparison
   * semantics are context-sensitive, for example where they depend on the implicit timezone or the default
   * collation.
   *
   * @param ordered          true if an ordered comparison is required. In this case the result is null if the
   *                         type is unordered; in other cases the returned value will be a Comparable.
   * @param collator         the collation (not used in this version of the method)
   * @param implicitTimezone the XPath dynamic evaluation context, used in cases where the comparison is context
   */
  /*@Nullable*/ override def getXPathComparable(ordered: Boolean, collator: StringCollator, implicitTimezone: Int) = this

  /**
   * Test if the two base64Binary values are equal.
   */
  override def equals(other: Any) = other.isInstanceOf[Base64BinaryValue] && util.Arrays.equals(binaryValue, other.asInstanceOf[Base64BinaryValue].binaryValue)

  override def hashCode: Int = Base64BinaryValue.byteArrayHashCode(binaryValue)

  override def compareTo(o: Any): Int = {
    val other = o.asInstanceOf[Base64BinaryValue].binaryValue
    val len0 = binaryValue.length
    val len1 = other.length
    val shorter = java.lang.Math.min(len0, len1)
    for (i <- 0 until shorter) {
      val a = binaryValue(i).toInt & 0xff
      val b = other(i).toInt & 0xff
      if (a != b) return if (a < b) -1
      else +1
    }
    Integer.signum(len0 - len1)
  }
}