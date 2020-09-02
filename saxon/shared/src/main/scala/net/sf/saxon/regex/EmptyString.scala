////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.regex

import EmptyString._




object EmptyString {

  val THE_INSTANCE: EmptyString = new EmptyString()

}

/**
  * An implementation of UnicodeString representing a zero-length string. This
  * is a singleton class with only one instance.
  */
class EmptyString private () extends UnicodeString {

  def uSubstring(beginIndex: Int, endIndex: Int): EmptyString =
    if (beginIndex == 0 && endIndex == 0) {
      this
    } else {
      throw new IndexOutOfBoundsException()
    }

  def uCharAt(pos: Int): Int = throw new IndexOutOfBoundsException()

  def uIndexOf(search: Int, pos: Int): Int = -1

  def uLength(): Int = 0

  def isEnd(pos: Int): Boolean = pos >= 0

  override def toString: String = ""

  def length: Int = 0

  def charAt(index: Int): Char = throw new IndexOutOfBoundsException()

  def subSequence(start: Int, end: Int): CharSequence =
    if (start == 0 && end == 0) {
      ""
    } else {
      throw new IndexOutOfBoundsException()
    }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
