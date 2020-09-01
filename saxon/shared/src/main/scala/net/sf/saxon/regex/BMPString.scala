////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.regex




/**
  * An implementation of UnicodeString optimized for strings that contain
  * no characters outside the BMP (i.e. no characters whose codepoints exceed 65535)
  */
class BMPString /**
  * Create a BMPString
  * @param src - encapsulated CharSequence.
  *            The client must ensure that this contains no surrogate pairs, and that
  *            it is immutable
  */
(private val src: CharSequence)
    extends UnicodeString {

  def uSubstring(beginIndex: Int, endIndex: Int): UnicodeString =
    new BMPString(src.subSequence(beginIndex, endIndex))

  def uCharAt(pos: Int): Int = src.charAt(pos)

  def uIndexOf(search: Int, pos: Int): Int =
    if (search > 65535) {
      -1
    } else {
      (pos until src.length).find(src.charAt(_) == search.toChar).getOrElse(-1)
    }

  def uLength(): Int = src.length

  def isEnd(pos: Int): Boolean = pos >= src.length

  override def toString: String = src.toString

  def getCharSequence(): CharSequence = src

  /**
    * Returns the length of this character sequence.  The length is the number
    * of 16-bit <code>char</code>s in the sequence.
    *
    * @return the number of <code>char</code>s in this sequence
    */
  def length(): Int = src.length

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
  def charAt(index: Int): Char = src.charAt(index)

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
    src.subSequence(start, end)

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
