////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.regex

import net.sf.saxon.value.StringValue

import org.jetbrains.annotations.NotNull




class GeneralUnicodeString(var charSequence: CharSequence)
    extends UnicodeString {

  private var charArr: Array[Int] = net.sf.saxon.value.StringValue.expand(charSequence)

  private var start: Int = 0

  private var end: Int = charArr.length

  def this(chars: Array[Int], start: Int, end: Int) = {
    this("")
    this.charArr = chars
    this.start = start
    this.end = end
  }

  def uSubstring(beginIndex: Int, endIndex: Int): UnicodeString = {
    if (endIndex > charArr.length) {
      throw new IndexOutOfBoundsException(
        "endIndex=" + endIndex + "; sequence size=" + charArr.length)
    }
    if (beginIndex < 0 || beginIndex > endIndex) {
      throw new IndexOutOfBoundsException(
        "beginIndex=" + beginIndex + "; endIndex=" + endIndex)
    }
    new GeneralUnicodeString(charArr, start + beginIndex, start + endIndex)
  }

  def uCharAt(pos: Int): Int = charArr(start + pos)

  def uIndexOf(search: Int, pos: Int): Int = {
    val len = uLength()
    for(i <- pos until len) {
         if(charArr(start +i) == search) {
           return i;
         }
         else {
           return -1;
         }
    }
    return -1;
  }

  def uLength(): Int = end - start

  def isEnd(pos: Int): Boolean = pos >= (end - start)

  @NotNull
  override def toString(): String = {
    obtainCharSequence()
    charSequence = charSequence.toString
    charSequence.asInstanceOf[String]
  }

  private def obtainCharSequence(): CharSequence = {
    if (charSequence == null) {
      var c: Array[Int] = charArr
      if (start != 0) {
        c = Array.ofDim[Int](end - start)
        System.arraycopy(chars, start, c, 0, end - start)
      }
      charSequence = StringValue.contract(c, end - start)
    }
    charSequence
  }

  /**
    * Returns the length of this character sequence.  The length is the number
    * of 16-bit <code>char</code>s in the sequence.
    *
    * @return the number of <code>char</code>s in this sequence
    */
  def length(): Int = obtainCharSequence().length

  /**
    * Returns the <code>char</code> value at the specified index.  An index ranges from zero
    * to <tt>length() - 1</tt>.  The first <code>char</code> value of the sequence is at
    * index zero, the next at index one, and so on, as for array
    * indexing.
    * <p>If the <code>char</code> value specified by the index is a
    * <a href="Character.html#unicode">surrogate</a>, the surrogate
    * value is returned.</p>
    *
    * @param index the index of the <code>char</code> value to be returned
    * @return the specified <code>char</code> value
    * @throws IndexOutOfBoundsException if the <tt>index</tt> argument is negative or not less than
    *                                   <tt>length()</tt>
    */
  def charAt(index: Int): Char = obtainCharSequence().charAt(index)

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
    *                                   if <tt>end</tt> is greater than <tt>length()</tt>,
    *                                   or if <tt>start</tt> is greater than <tt>end</tt>
    */
  def subSequence(start: Int, end: Int): CharSequence =
    obtainCharSequence().subSequence(start, end)

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A Unicode string which, in general, may contain non-BMP characters (that is, codepoints
  * outside the range 0-65535)
  */
