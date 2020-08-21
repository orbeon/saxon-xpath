package net.sf.saxon.tree.tiny

import net.sf.saxon.tree.util.FastStringBuffer

import java.util.Arrays

import LargeStringBuffer._

import scala.util.control.Breaks._

object LargeStringBuffer {

  private val BITS: Int = 16

  private val SEGLEN: Int = 1 << BITS

  private val MASK: Int = SEGLEN - 1

}

class LargeStringBuffer extends AppendableCharSequence {

  private var data: Array[Array[Char]] = new Array[Array[Char]](1)

  var length: Int = 0

  private var segmentsUsed: Int = 0

  private def addSegment(seg: Array[Char]): Unit = {
    val segs: Int = data.length
    if (segmentsUsed + 1 > segs) {
      if (segmentsUsed == 32768) {
        throw new IllegalStateException(
          "Source document too large: more than 1G characters in text nodes")
      }
      data = Arrays.copyOf(data, segs * 2)
    }
    data({
      segmentsUsed += 1; segmentsUsed - 1
    }) = seg
  }

  def cat(s: CharSequence): LargeStringBuffer = {
    var lS = s
    if (lS.isInstanceOf[CompressedWhitespace]) {
      val fsb: FastStringBuffer = new FastStringBuffer(FastStringBuffer.C64)
      lS.asInstanceOf[CompressedWhitespace].uncompress(fsb)
      cat(fsb)
    }
    val len: Int = lS.length
    var firstSeg: Array[Char] = null
    val firstSegOffset: Int = length & MASK
    if (firstSegOffset == 0) {
      firstSeg = Array.ofDim[Char](SEGLEN)
      addSegment(firstSeg)
    } else {
      firstSeg = data(length >> BITS)
    }
    var firstSegLen: Int = 0
    var fullSegments: Int = 0
    var lastSegLen: Int = 0
    if (len <= SEGLEN - firstSegOffset) {
      firstSegLen = len
      fullSegments = 0
      lastSegLen = 0
    } else {
      firstSegLen = SEGLEN - firstSegOffset
      fullSegments = (len - firstSegLen) >> BITS
      lastSegLen = (len - firstSegLen) & MASK
    }
    if (lS.isInstanceOf[CharSlice]) {
      lS.asInstanceOf[CharSlice]
        .getChars(0, firstSegLen, firstSeg, firstSegOffset)
      var start: Int = firstSegLen
      for (i <- 0 until fullSegments) {
        val seg: Array[Char] = Array.ofDim[Char](SEGLEN)
        addSegment(seg)
        lS.asInstanceOf[CharSlice].getChars(start, start + SEGLEN, seg, 0)
        start += SEGLEN
      }
      if (lastSegLen > 0) {
        val seg: Array[Char] = Array.ofDim[Char](SEGLEN)
        addSegment(seg)
        lS.asInstanceOf[CharSlice].getChars(start, len, seg, 0)
      }
      length += len
    } else if (lS.isInstanceOf[FastStringBuffer]) {
      lS.asInstanceOf[FastStringBuffer]
        .getChars(0, firstSegLen, firstSeg, firstSegOffset)
      var start: Int = firstSegLen
      for (i <- 0 until fullSegments) {
        val seg: Array[Char] = Array.ofDim[Char](SEGLEN)
        addSegment(seg)
        lS.asInstanceOf[FastStringBuffer]
          .getChars(start, start + SEGLEN, seg, 0)
        start += SEGLEN
      }
      if (lastSegLen > 0) {
        val seg: Array[Char] = Array.ofDim[Char](SEGLEN)
        addSegment(seg)
        lS.asInstanceOf[FastStringBuffer].getChars(start, len, seg, 0)
      }
      length += len
    } else {
      if (!(lS.isInstanceOf[String])) {
        lS = lS.toString
      }
      lS.asInstanceOf[String].getChars(0, firstSegLen, firstSeg, firstSegOffset)
      var start: Int = firstSegLen
      for (i <- 0 until fullSegments) {
        val seg: Array[Char] = Array.ofDim[Char](SEGLEN)
        addSegment(seg)
        lS.asInstanceOf[String].getChars(start, start + SEGLEN, seg, 0)
        start += SEGLEN
      }
      if (lastSegLen > 0) {
        val seg: Array[Char] = Array.ofDim[Char](SEGLEN)
        addSegment(seg)
        lS.asInstanceOf[String].getChars(start, len, seg, 0)
      }
      length += len
    }
    this
  }

  def cat(c: Char): LargeStringBuffer = cat("" + c)

  def setLength(length: Int): Unit = {
    if (length < this.length) {
      val usedInLastSegment: Int = length & MASK
      this.length = length
      this.segmentsUsed = length / SEGLEN + (if (usedInLastSegment == 0) 0
      else 1)
    }
  }

  def charAt(index: Int): Char = {
    if (index < 0 || index >= length) {
      throw new IndexOutOfBoundsException(index + "")
    }
    data(index >> BITS)(index & MASK)
  }

  def subSequence(start: Int, end: Int): CharSequence = {
    var firstSeg: Int = start >> BITS
    val lastSeg: Int = (end - 1) >> BITS
    if (firstSeg == lastSeg) {
      try new CharSlice(data(firstSeg), start & MASK, end - start)
      catch {
        case e: ArrayIndexOutOfBoundsException => {
          e.printStackTrace()
          throw e
        }

      }
    } else {
      val fsb: FastStringBuffer = new FastStringBuffer(end - start)
      val firstSegLen: Int = SEGLEN - (start & MASK)
      fsb.append(data(firstSeg), start & MASK, firstSegLen)
      var doneTo: Int = start + firstSegLen
      breakable {
        while (true) {
          {
            firstSeg += 1;
            firstSeg - 1
          }
          if (doneTo + SEGLEN < end) {
            fsb.append(data(firstSeg))
            doneTo += SEGLEN
          } else {
            fsb.append(data(firstSeg), 0, end - doneTo)
            break()
          }
        }
      }
      fsb
    }
  }

  override def toString(): String = subSequence(0, length).toString

  override def equals(other: Any): Boolean = other match {
    case other: CharSequence => toString == other.toString
    case _ => false

  }

  override def hashCode(): Int = {
    var h: Int = 0
    for (chars <- data; i <- 0 until SEGLEN) {
      h = 31 * h + chars(i)
    }
    h
  }

  def substring(start: Int, end: Int): String =
    subSequence(start, end).toString

  def write(writer: java.io.Writer): Unit = {
    writer.write(toString)
  }

}
