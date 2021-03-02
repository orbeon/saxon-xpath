package org.orbeon.saxon.tree.tiny

import org.orbeon.saxon.tree.tiny.CompressedWhitespace._
import org.orbeon.saxon.tree.util.FastStringBuffer

import java.io.Writer
import scala.util.control.Breaks._


object CompressedWhitespace {

  private val WHITE_CHARS: Array[Char] = Array(0x09, 0x0A, 0x0D, 0x20)

  private val CODES: Array[Int] =
    Array(
      -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 1, -1, -1, 2, -1, -1, -1, -1,
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 3
    )

  def compress(in: CharSequence): CharSequence = {
    val inlen = in.length
    if (inlen == 0)
      return in
    var runlength = 1
    var outlength = 0
    for (i <- 0 until inlen) {
      val c = in.charAt(i)
      if (c <= 32 && CODES(c) >= 0) {
        if (i == inlen - 1 || c != in.charAt(i + 1) || runlength == 63) {
          runlength = 1
          outlength = outlength + 1
          if (outlength > 8)
            return in // TODO: non-local return
        } else {
          runlength = runlength + 1
        }
      } else {
        return in // TODO: non-local return
      }
    }
    var ix = 0
    runlength = 1
    val out = Array.ofDim[Int](outlength)
    for (i <- 0 until inlen) {
      val c = in.charAt(i)
      if (i == inlen - 1 || c != in.charAt(i + 1) || runlength == 63) {
        out(ix) = (CODES(c) << 6) | runlength
        ix = ix + 1
        runlength = 1
      } else {
        runlength += 1
      }
    }
    var value = 0L
    for (i <- 0 until outlength)
      value = (value << 8) | out(i)
    value <<= 8 * (8 - outlength)
    new CompressedWhitespace(value)
  }

  def uncompress(value: Long, buffer: FastStringBuffer): Unit = {
    var s: Int = 56
    breakable {
      while (s >= 0) {
        val b: Byte = ((value >>> s) & 0xff).toByte
        if (b == 0) {
          break()
        }
        val c: Char = WHITE_CHARS(b >>> 6 & 0x3)
        val len = b & 0x3f
        buffer.ensureCapacity(len)
        for (j <- 0 until len) {
          buffer.cat(c)
        }
        s -= 8
      }
    }
  }

}

class CompressedWhitespace(private var value: Long) extends CharSequence {

  def uncompress(buffer: FastStringBuffer): FastStringBuffer = {
    var lBuffer = buffer
    if (lBuffer == null) {
      lBuffer = new FastStringBuffer(length)
    }
    CompressedWhitespace.uncompress(value, lBuffer)
    lBuffer
  }

  def getCompressedValue: Long = value

  def length: Int = {
    var count: Int = 0
    val `val`: Long = value
    var s: Int = 56
    // ORBEON: Avoid non-local return.
    var exitLoop = false
    while (! exitLoop && s >= 0) {
      val c = ((`val` >>> s) & 0x3f).toInt
      if (c == 0) {
        exitLoop = true
      } else {
        count += c
        s -= 8
      }
    }
    count
  }

  def charAt(index: Int): Char = {
    var count = 0
    val `val` = value
    var s     = 56
    // ORBEON: Avoid non-local return.
    var exitLoop = false
    var result: Char = 0
    while (! exitLoop && s >= 0) {
      val b = ((`val` >>> s) & 0xff).toByte
      if (b == 0) {
        exitLoop = true
      } else {
        count += b & 0x3f
        if (count > index) {
          exitLoop = true
          result = WHITE_CHARS(b >>> 6 & 0x3)
        }
        s -= 8
      }
    }
    if (result == 0)
      throw new IndexOutOfBoundsException(s"$index")
    else
      result

  }

  def subSequence(start: Int, end: Int): CharSequence =
    uncompress(null).subSequence(start, end)

  override def equals(obj: Any): Boolean =
    obj match {
      case whitespace: CompressedWhitespace => value == whitespace.value
      case _                                => uncompress(null) == obj
    }

  override def hashCode: Int = uncompress(null).hashCode

  override def toString: String = uncompress(null).toString

  def write(writer: Writer): Unit = {
    val `val` = value
    var s     = 56
    // ORBEON: Avoid non-local return.
    var exitLoop = false
    while (! exitLoop && s >= 0) {
      val b = ((`val` >>> s) & 0xff).toByte
      if (b == 0) {
        exitLoop = true
      } else {
        val c   = WHITE_CHARS(b >>> 6 & 0x3)
        val len = b & 0x3f
        for (_ <- 0 until len)
          writer.write(c)
        s -= 8
      }
    }
  }

  def writeEscape(specialChars: Array[Boolean], writer: Writer): Unit = {
    val `val` = value
    var s     = 56
    // ORBEON: Avoid non-local return.
    var exitLoop = false
    while (! exitLoop && s >= 0) {
      val b = ((`val` >>> s) & 0xff).toByte
      if (b == 0) {
        exitLoop = true
      } else {
        val c   = WHITE_CHARS(b >>> 6 & 0x3)
        val len = b & 0x3f
        if (specialChars(c)) {
          var e = ""
          if (c == '\n') {
            e = "&#xA;"
          } else if (c == '\r') {
            e = "&#xD;"
          } else if (c == '\t') {
            e = "&#x9;"
          }
          for (_ <- 0 until len)
            writer.write(e)
        } else {
          for (_ <- 0 until len)
            writer.write(c)
        }
        s -= 8
      }
    }
  }
}