////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

package org.orbeon.saxon.regex


object LatinString {
  val SINGLE_SPACE: LatinString = new LatinString(Array(0x20.toByte))
}

/**
 * An implementation of UnicodeString optimized for strings that contain
 * no characters outside the Latin-1 range (i.e. no characters whose codepoints exceed 255).
 */
class LatinString(src: CharSequence)
  extends UnicodeString {

  private var charArr: Array[Byte] = {
    val len = src.length
    val newArray = new Array[Byte](len)
    for (i <- 0 until len)
      newArray.array(i) = (src.charAt(i) & 0xff).toByte
    newArray
  }

  def this(chars: Array[Byte]) = {
    this("")
    this.charArr = chars
  }

  def uSubstring(beginIndex: Int, endIndex: Int): LatinString = {
    val s = Array.ofDim[Byte](endIndex - beginIndex)
    System.arraycopy(chars, beginIndex, s, 0, endIndex - beginIndex)
    new LatinString(s)
  }

  def uCharAt(pos: Int): Int = charArr(pos) & 0xff

  def uIndexOf(search: Int, pos: Int): Int =
    if (search > 255) {
      -1
    } else {
      for (i <- pos until charArr.length) {
        if ((charArr(i) & 0xff) == search)
          return i
      }
      -1;
    }

  def uLength(): Int = charArr.length

  def isEnd(pos: Int): Boolean = pos >= charArr.length

  override def toString: String = {
    val expanded: Array[Char] = Array.ofDim[Char](charArr.length)
    for (i <- 0 until charArr.length) {
      expanded(i) = (charArr(i) & 0xff).toChar
    }
    new String(expanded)
  }

  /**
   * Returns the length of this character sequence.  The length is the number
   * of 16-bit <code>char</code>s in the sequence.
   *
   * @return the number of <code>char</code>s in this sequence
   */
  def length: Int = charArr.length

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
  def charAt(index: Int): Char = (charArr(index) & 0xff).toChar

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
  def subSequence(start: Int, end: Int): CharSequence = uSubstring(start, end)

}
