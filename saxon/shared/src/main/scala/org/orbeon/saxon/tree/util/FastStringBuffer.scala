////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.tree.util

import java.io.Writer
import java.util.Arrays

import org.orbeon.saxon.regex.{BMPString, GeneralUnicodeString, UnicodeString}
import org.orbeon.saxon.serialize.charcode.UTF16CharacterSet
import org.orbeon.saxon.tree.tiny.{AppendableCharSequence, CharSlice, CompressedWhitespace}


/**
  * A simple implementation of a class similar to StringBuffer. Unlike
  * StringBuffer it is not synchronized. It also offers the capability
  * to remove unused space. (This class could possibly be replaced by
  * StringBuilder in JDK 1.5, but using our own class gives more control.)
  */
object FastStringBuffer {

  val C16  : Int = 16
  val C64  : Int = 64
  val C256 : Int = 256
  val C1024: Int = 1024

  def diagnosticPrint(in: CharSequence): String = {
    val buff = new FastStringBuffer(in.length * 2)
    for (i <- 0 until in.length) {
      val c = in.charAt(i)
      if (c > 32 && c < 127) {
        buff.cat(c)
      } else {
        buff.append("\\u")
        var d = 12
        while (d >= 0) {
          buff.cat("0123456789ABCDEF".charAt((c >> d) & 0xf))
          d -= 4
        }
      }
    }
    buff.toString
  }
}

class FastStringBuffer(initialSize: Int)
    extends AppendableCharSequence
    with CharSequenceConsumer {

  private var array: Array[Char] = new Array[Char](initialSize)

  private var used: Int = 0

  def this(cs: CharSequence) = {
    this(0)
    array = Array.ofDim[Char](cs.length)
    this.cat(cs)
  }

  def cat(s: String): FastStringBuffer = {
    val len = s.length
    ensureCapacity(len)
    s.getChars(0, len, array, used)
    used += len
    this
  }

  def append(s: String): Unit =
    cat(s)

  def append(s: CharSlice): Unit = {
    val len = s.length
    ensureCapacity(len)
    s.copyTo(array, used)
    used += len
  }

  def append(s: FastStringBuffer): Unit = {
    val len = s.length
    ensureCapacity(len)
    s.getChars(0, len, array, used)
    used += len
  }

  def append(s: StringBuffer): Unit = {
    val len = s.length
    ensureCapacity(len)
    s.getChars(0, len, array, used)
    used += len
  }

  def cat(s: CharSequence): FastStringBuffer = {
    // Although we provide variants of this method for different subtypes, Java decides which to use based
    // on the static type of the operand. We want to use the right method based on the dynamic type, to avoid
    // creating objects and copying strings unnecessarily. So we do a dynamic dispatch.
    val len = s.length
    ensureCapacity(len)
    s match {
      case slice: CharSlice =>
        slice.copyTo(array, used)
      case string: String =>
        string.getChars(0, len, array, used)
      case buffer: FastStringBuffer =>
        buffer.getChars(0, len, array, used)
      case whitespace: CompressedWhitespace =>
        whitespace.uncompress(this)
        return this
      case string: BMPString =>
        this.cat(string.getCharSequence)
        return this
      case string: GeneralUnicodeString =>
        for (i <- 0 until string.uLength)
          appendWideChar(string.uCharAt(i))
        return this
      case _ =>
        s.toString.getChars(0, len, array, used)
    }
    used += len
    this
  }

  def append(s: CharSequence): Unit =
    cat(s)

  def append(srcArray: Array[Char], start: Int, length: Int): Unit = {
    ensureCapacity(length)
    System.arraycopy(srcArray, start, array, used, length)
    used += length
  }

  def append(srcArray: Array[Char]): Unit = {
    val length = srcArray.length
    ensureCapacity(length)
    System.arraycopy(srcArray, 0, array, used, length)
    used += length
  }

  override def cat(ch: Char): FastStringBuffer = {
    ensureCapacity(1)
    array({ used += 1; used - 1 }) = ch
    this
  }

  def appendWideChar(ch: Int): Unit =
    if (ch > 0xffff) {
      cat(UTF16CharacterSet.highSurrogate(ch))
      cat(UTF16CharacterSet.lowSurrogate(ch))
    } else {
      cat(ch.toChar)
    }

  def append(str: UnicodeString): Unit =
    str match {
      case bmpString: BMPString =>
        this.cat(bmpString.getCharSequence)
      case _ =>
        for (i <- 0 until str.uLength) {
          appendWideChar(str.uCharAt(i))
        }
    }

  def prependWideChar(ch: Int): Unit =
    if (ch > 0xffff) {
      prepend(UTF16CharacterSet.lowSurrogate(ch))
      prepend(UTF16CharacterSet.highSurrogate(ch))
    } else {
      prepend(ch.toChar)
    }

  /**
    * Returns the length of this character sequence.  The length is the number
    * of 16-bit <code>char</code>s in the sequence.
    *
    * @return the number of <code>char</code>s in this sequence
    */
  def length: Int = used
  def isEmpty: Boolean = used == 0

  /**
    * Returns the <code>char</code> value at the specified index.  An index ranges from zero
    * to <tt>length - 1</tt>.  The first <code>char</code> value of the sequence is at
    * index zero, the next at index one, and so on, as for array
    * indexing.
    * <p>If the <code>char</code> value specified by the index is a
    * <a href="Character.html#unicode">surrogate</a>, the surrogate
    * value is returned.</p>
    *
    * @param index the index of the <code>char</code> value to be returned
    * @return the specified <code>char</code> value
    * @throws IndexOutOfBoundsException if the <tt>index</tt> argument is negative or not less than
    *                                   <tt>length</tt>
    */
  def charAt(index: Int): Char = {
    if (index >= used)
      throw new IndexOutOfBoundsException("" + index)
    array(index)
  }

  /**
    * Returns a new <code>CharSequence</code> that is a subsequence of this sequence.
    * The subsequence starts with the <code>char</code> value at the specified index and
    * ends with the <code>char</code> value at index <tt>end - 1</tt>.  The length
    * (in <code>char</code>s) of the
    * returned sequence is <tt>end - start</tt>, so if <tt>start == end</tt>
    * then an empty sequence is returned.
    *
    * @param start the start index, inclusive
    * @param end   the end index, exclusive
    * @return the specified subsequence
    * @throws IndexOutOfBoundsException if <tt>start</tt> or <tt>end</tt> are negative,
    *                                   if <tt>end</tt> is greater than <tt>length</tt>,
    *                                   or if <tt>start</tt> is greater than <tt>end</tt>
    */
  def subSequence(start: Int, end: Int): CharSequence =
    new CharSlice(array, start, end - start)

  /**
    * Copies characters from this FastStringBuffer into the destination character
    * array.
    * <p>The first character to be copied is at index <code>srcBegin</code>;
    * the last character to be copied is at index <code>srcEnd-1</code>
    * (thus the total number of characters to be copied is
    * <code>srcEnd-srcBegin</code>). The characters are copied into the
    * subarray of <code>dst</code> starting at index <code>dstBegin</code>
    * and ending at index:</p>
    * <blockquote><pre>
    *     dstbegin + (srcEnd-srcBegin) - 1
    * </pre></blockquote>
    *
    * @param srcBegin index of the first character in the string
    *                 to copy.
    * @param srcEnd   index after the last character in the string
    *                 to copy.
    * @param dst      the destination array.
    * @param dstBegin the start offset in the destination array.
    * @throws IndexOutOfBoundsException If any of the following
    *                                   is true:
    *                                   <ul><li><code>srcBegin</code> is negative.
    *                                   <li><code>srcBegin</code> is greater than <code>srcEnd</code>
    *                                   <li><code>srcEnd</code> is greater than the length of this
    *                                   string
    *                                   <li><code>dstBegin</code> is negative
    *                                   <li><code>dstBegin+(srcEnd-srcBegin)</code> is larger than
    *                                   <code>dst.length</code></ul>
    */
  def getChars(srcBegin: Int,
               srcEnd: Int,
               dst: Array[Char],
               dstBegin: Int): Unit = {
    if (srcBegin < 0)
      throw new StringIndexOutOfBoundsException(srcBegin)
    if (srcEnd > used)
      throw new StringIndexOutOfBoundsException(srcEnd)
    if (srcBegin > srcEnd)
      throw new StringIndexOutOfBoundsException(srcEnd - srcBegin)
    System.arraycopy(array, srcBegin, dst, dstBegin, srcEnd - srcBegin)
  }

  def indexOf(ch: Char): Int =
    (0 until used).find(array(_) == ch).getOrElse(-1)

  override def toString: String = {
// has side-effects which is nasty on the debugger!
    condense()
    new String(array, 0, used)
  }

  override def equals(other: Any): Boolean = other match {
    case other: CharSequence => toString == other.toString
    case _ => false
  }

  override def hashCode: Int = {
// Same algorithm as String#hashCode, but not cached
    var h = 0
    for (i <- 0 until used)
      h = 31 * h + array(i)
    h
  }

  def toCharArray: Array[Char] =
    if (used == array.length) {
      array
    } else {
      val chars: Array[Char] = Array.ofDim[Char](used)
      System.arraycopy(array, 0, chars, 0, used)
      chars
    }

  def setCharAt(index: Int, ch: Char): Unit = {
    if (index < 0 || index > used)
      throw new IndexOutOfBoundsException("" + index)
    array(index) = ch
  }

  def insert(index: Int, ch: Char): Unit = {
    if (index < 0 || index > used)
      throw new IndexOutOfBoundsException("" + index)
    ensureCapacity(1)
    System.arraycopy(array, index, array, index + 1, used - index)
      used += 1
    array(index) = ch
  }

  def insertWideChar(index: Int, ch: Int): Unit =
    if (index < 0 || index > used)
      throw new IndexOutOfBoundsException("" + index)
    else if (ch > 0xffff) {
      ensureCapacity(2)
      System.arraycopy(array, index, array, index + 2, used - index)
      used += 2
      array(index) = UTF16CharacterSet.highSurrogate(ch)
      array(index + 1) = UTF16CharacterSet.lowSurrogate(ch)
    } else {
      ensureCapacity(1)
      System.arraycopy(array, index, array, index + 1, used - index)
      used += 1
      array(index) = ch.toChar
    }

  def removeCharAt(index: Int): Unit = {
    if (index < 0 || index > used)
      throw new IndexOutOfBoundsException("" + index)
    used -= 1
    System.arraycopy(array, index + 1, array, index, used - index)
  }

  def prepend(ch: Char): Unit = {
    val a2 = Array.ofDim[Char](array.length + 1)
    System.arraycopy(array, 0, a2, 1, used)
    a2(0) = ch
    used += 1
    array = a2
  }

  def prepend(str: CharSequence): Unit = {
    val len = str.length
    val a2 = Array.ofDim[Char](array.length + len)
    System.arraycopy(array, 0, a2, len, used)
    for (i <- 0 until len)
      a2(i) = str.charAt(i)
    used += len
    array = a2
  }

  def prependRepeated(ch: Char, repeat: Int): Unit =
    if (repeat > 0) {
      val a2 = Array.ofDim[Char](array.length + repeat)
      System.arraycopy(array, 0, a2, repeat, used)
      Arrays.fill(a2, 0, repeat, ch)
      used += repeat
      array = a2
    }

  def setLength(length: Int): Unit = {
    if (length < 0 || length > used)
      return
    used = length
  }

  def ensureCapacity(extra: Int): Unit =
    if (used + extra > array.length) {
      var newlen = array.length * 2
      if (newlen < used + extra)
        newlen = used + extra * 2
      array = Arrays.copyOf(array, newlen)
    }

  def condense(): FastStringBuffer = {
    if (array.length - used > 256 || (array.length > used * 2 && array.length - used > 20))
      array = Arrays.copyOf(array, used)
    this
  }

  def write(writer: Writer): Unit =
    writer.write(array, 0, used)
}
