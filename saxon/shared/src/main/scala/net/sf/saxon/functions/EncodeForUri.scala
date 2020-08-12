package net.sf.saxon.functions

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.Item

import net.sf.saxon.om.ZeroOrOne

import net.sf.saxon.serialize.charcode.UTF8CharacterSet

import net.sf.saxon.trans.Err

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.util.FastStringBuffer

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.StringValue

import EncodeForUri._

object EncodeForUri {

  def escape(s: CharSequence, allowedPunctuation: String): CharSequence = {
    val sb: FastStringBuffer = new FastStringBuffer(s.length)
    for (i <- 0 until s.length) {
      val c: Char = s.charAt(i)
      if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')) {
        sb.cat(c)
      } else if (c <= 0x20 || c >= 0x7f) {
        escapeChar(c, if ((i + 1) < s.length) s.charAt(i + 1) else ' ', sb)
      } else if (allowedPunctuation.indexOf(c) >= 0) {
        sb.cat(c)
      } else {
        escapeChar(c, ' ', sb)
      }
    }
    sb
  }

  private val hex: String = "0123456789ABCDEF"

  def escapeChar(c: Char, c2: Char, sb: FastStringBuffer): Unit = {
    val array: Array[Byte] = Array.ofDim[Byte](4)
    val used: Int = UTF8CharacterSet.getUTF8Encoding(c, c2, array)
    for (b <- 0 until used) {
      val v: Int = array(b).toInt & 0xff
      sb.cat('%')
      sb.cat(hex.charAt(v / 16))
      sb.cat(hex.charAt(v % 16))
    }
  }

  def checkPercentEncoding(uri: String): Unit = {
    val hexDigits: String = "0123456789abcdefABCDEF"
    var i: Int = 0
    while (i < uri.length) {
      val c: Char = uri.charAt(i)
      var bytes: Array[Byte] = null
      var expectedOctets: Int = 0
      if (c == '%') {
        if (i + 2 >= uri.length) {
          throw new XPathException(
            "% sign in URI must be followed by two hex digits" + Err.wrap(uri))
        }
        var h1: Int = hexDigits.indexOf(uri.charAt(i + 1))
        if (h1 > 15) {
          h1 -= 6
        }
        var h2: Int = hexDigits.indexOf(uri.charAt(i + 2))
        if (h2 > 15) {
          h2 -= 6
        }
        if (h1 >= 0 && h2 >= 0) {
          var b: Int = h1 << 4 | h2
          expectedOctets = UTF8RepresentationLength(h1)
          if (expectedOctets == -1) {
            throw new XPathException(
              "First %-encoded octet in URI is not valid as the start of a UTF-8 " +
                "character: first two bits must not be '10'" +
                Err.wrap(uri))
          }
          bytes = Array.ofDim[Byte](expectedOctets)
          bytes(0) = b.toByte
          i += 3
          for (q <- 1 until expectedOctets) {
            if (i + 2 > uri.length || uri.charAt(i) != '%') {
              throw new XPathException(
                "Incomplete %-encoded UTF-8 octet sequence in URI " +
                  Err.wrap(uri))
            }
            h1 = hexDigits.indexOf(uri.charAt(i + 1))
            if (h1 > 15) {
              h1 -= 6
            }
            h2 = hexDigits.indexOf(uri.charAt(i + 2))
            if (h2 > 15) {
              h2 -= 6
            }
            if (h1 < 0 || h2 < 0) {
              throw new XPathException(
                "Invalid %-encoded UTF-8 octet sequence in URI" + Err.wrap(
                  uri))
            }
            if (UTF8RepresentationLength(h1) != -1) {
              throw new XPathException(
                "In a URI, a %-encoded UTF-8 octet after the first " +
                  "must have '10' as the first two bits" +
                  Err.wrap(uri))
            }
            b = h1 << 4 | h2
            bytes(q) = b.toByte
            i += 3
          }
        } else {
          throw new XPathException(
            "% sign in URI must be followed by two hex digits" + Err.wrap(uri))
        }
      } else {
        { i += 1; i - 1 }
      }
    }
  }

  private var UTF8RepresentationLength: Array[Int] =
    Array(1, 1, 1, 1, 1, 1, 1, 1, -1, -1, -1, -1, 2, 2, 3, 4)

}

class EncodeForUri extends ScalarSystemFunction {

  override def evaluate(arg: Item, context: XPathContext): AtomicValue = {
    val s: CharSequence = arg.getStringValueCS
    StringValue.makeStringValue(escape(s, "-_.~"))
  }

  override def resultWhenEmpty(): ZeroOrOne[StringValue] = ScalarSystemFunction.ZERO_LENGTH_STRING

}
