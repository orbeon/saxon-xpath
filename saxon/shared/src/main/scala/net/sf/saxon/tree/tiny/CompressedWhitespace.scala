package net.sf.saxon.tree.tiny

import net.sf.saxon.tree.util.FastStringBuffer

import java.io.Writer

import CompressedWhitespace._

object CompressedWhitespace  {

  private var WHITE_CHARS: Array[Char] = Array(0x09, 0x0A, 0x0D, 0x20)

  private var CODES: Array[Int] =
    Array(-1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 1, -1, -1, 2, -1, -1, -1, -1,
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 3)

  def compress(in: CharSequence): CharSequence = {
    val inlen: Int = in.length
    if (inlen == 0) {
      in
    }
    var runlength: Int = 1
    var outlength: Int = 0
    for (i <- 0 until inlen) {
      val c: Char = in.charAt(i)
      if (c <= 32 && CODES(c) >= 0) {
        if (i == inlen - 1 || c != in.charAt(i + 1) || runlength == 63) {
          runlength = 1
          outlength = outlength + 1
          if (outlength > 8) {
            in
          }
        } else {
          runlength = runlength + 1
        }
      } else {
        in
      }
    }
    var ix: Int = 0
    runlength = 1
    val out: Array[Int] = Array.ofDim[Int](outlength)
    for (i <- 0 until inlen) {
      val c: Char = in.charAt(i)
      if (i == inlen - 1 || c != in.charAt(i + 1) || runlength == 63) {
        out(ix) = (CODES(c) << 6) | runlength
        ix = ix + 1
        runlength = 1
      } else {
        { runlength += 1; runlength - 1 }
      }
    }
    var value: Long = 0
    for (i <- 0 until outlength) {
      value = (value << 8) | out(i)
    }
    value <<= 8 * (8 - outlength)
    new CompressedWhitespace(value)
  }

  def uncompress(value: Long, buffer: FastStringBuffer): Unit = {
    var s: Int = 56
    while (s >= 0) {
      val b: Byte = ((value >>> s) & 0xff).toByte
      if (b == 0) {
        //break
      }
      val c: Char = WHITE_CHARS(b >>> 6 & 0x3)
      val len: Int = b & 0x3f
      buffer.ensureCapacity(len)
      for (j <- 0 until len) {
        buffer.cat(c)
      }
      s -= 8
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

  def getCompressedValue(): Long = value

  def length(): Int = {
    var count: Int = 0
    val `val`: Long = value
    var s: Int = 56
    while (s >= 0) {
      val c: Int = ((`val` >>> s) & 0x3f).toInt
      if (c == 0) {
        //break
      }
      count += c
      s -= 8
    }
    count
  }

  def charAt(index: Int): Char = {
    var count: Int = 0
    val `val`: Long = value
    var s: Int = 56
    while (s >= 0) {
      val b: Byte = ((`val` >>> s) & 0xff).toByte
      if (b == 0) {
        //break
      }
      count += b & 0x3f
      if (count > index) {
        WHITE_CHARS(b >>> 6 & 0x3)
      }
      s -= 8
    }
    throw new IndexOutOfBoundsException(index + "")
  }

  def subSequence(start: Int, end: Int): CharSequence =
    uncompress(null).subSequence(start, end)

  override def equals(obj: Any): Boolean = {
    if (obj.isInstanceOf[CompressedWhitespace]) {
      value == obj.asInstanceOf[CompressedWhitespace].value
    }
    uncompress(null) == obj
  }

  override def hashCode(): Int = uncompress(null).hashCode

  override def toString(): String = uncompress(null).toString

  def write(writer: Writer): Unit = {
    val `val`: Long = value
    var s: Int = 56
    while (s >= 0) {
      val b: Byte = ((`val` >>> s) & 0xff).toByte
      if (b == 0) {
        //break
      }
      val c: Char = WHITE_CHARS(b >>> 6 & 0x3)
      val len: Int = b & 0x3f
      for (j <- 0 until len) {
        writer.write(c)
      }
      s -= 8
    }
  }

  def writeEscape(specialChars: Array[Boolean], writer: Writer): Unit = {
    val `val`: Long = value
    var s: Int = 56
    while (s >= 0) {
      val b: Byte = ((`val` >>> s) & 0xff).toByte
      if (b == 0) {
        //break
      }
      val c: Char = WHITE_CHARS(b >>> 6 & 0x3)
      val len: Int = b & 0x3f
      if (specialChars(c)) {
        var e: String = ""
        if (c == '\n') {
          e = "&#xA;"
        } else if (c == '\r') {
          e = "&#xD;"
        } else if (c == '\t') {
          e = "&#x9;"
        }
        for (j <- 0 until len) {
          writer.write(e)
        }
      } else {
        for (j <- 0 until len) {
          writer.write(c)
        }
      }
      s -= 8
    }
  }

}