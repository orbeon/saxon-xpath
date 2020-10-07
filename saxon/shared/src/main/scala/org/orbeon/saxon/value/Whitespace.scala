////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.value

import org.orbeon.saxon.tree.iter.AtomicIterator
import org.orbeon.saxon.tree.tiny.CharSlice
import org.orbeon.saxon.tree.tiny.CompressedWhitespace
import org.orbeon.saxon.tree.util.FastStringBuffer
import scala.util.control.Breaks._

/**
 * This class provides helper methods and constants for handling whitespace
 */
object Whitespace {
  /**
   * The values PRESERVE, REPLACE, and COLLAPSE represent the three options for whitespace
   * normalization. They are deliberately chosen in ascending strength order; given a number
   * of whitespace facets, only the strongest needs to be carried out. The option TRIM is
   * used instead of COLLAPSE when all valid values have no interior whitespace; trimming
   * leading and trailing whitespace is then equivalent to the action of COLLAPSE, but faster.
   */
    val PRESERVE = 0
  val REPLACE = 1
  val COLLAPSE = 2
  val TRIM = 3
  /**
   * The values NONE, IGNORABLE, and ALL identify which kinds of whitespace text node
   * should be stripped when building a source tree. UNSPECIFIED indicates that no
   * particular request has been made. XSLT indicates that whitespace should be stripped
   * as defined by the xsl:strip-space and xsl:preserve-space declarations in the stylesheet
   */
  val NONE = 0
  val IGNORABLE = 1
  val ALL = 2
  val UNSPECIFIED = 3
  val XSLT = 4

  /**
   * Test whether a character is whitespace
   *
   * @param ch the character (Unicode codepoint) to be tested
   * @return true if the character is one of tab, newline, carriage return, or space
   */
  def isWhitespace(ch: Int) = ch match {
    case 9 =>
    case 10 =>
    case 13 =>
    case 32 =>
      true
    case _ =>
      false
  }

  /**
   * Apply schema-defined whitespace normalization to a string
   *
   * @param action the action to be applied: one of PRESERVE, REPLACE, or COLLAPSE
   * @param value  the value to be normalized
   * @return the value after normalization
   */
  def applyWhitespaceNormalization(action: Int, /*@NotNull*/ value: CharSequence) = action match {
    case PRESERVE =>
      value
    case REPLACE =>
      val sb = new FastStringBuffer(value.length)
      for (i <- 0 until value.length) {
        val c = value.charAt(i)
        c match {
          case '\n' =>
          case '\r' =>
          case '\t' =>
            sb.cat(' ')
          case _ =>
            sb.cat(c)
        }
      }
      sb
    case COLLAPSE =>
      collapseWhitespace(value)
    case TRIM =>
      trimWhitespace(value)
    case _ =>
      throw new IllegalArgumentException("Unknown whitespace facet value")
  }

  /**
   * Remove all whitespace characters from a string
   *
   * @param value the string from which whitespace is to be removed
   * @return the string without its whitespace. This may be the original value
   *         if it contained no whitespace
   */
  def removeAllWhitespace(value: CharSequence) = if (containsWhitespace(value)) {
    val sb = new FastStringBuffer(value.length)
    for (i <- 0 until value.length) {
      val c = value.charAt(i)
      if (c > 32 || !C0WHITE(c)) sb.cat(c)
    }
    sb
  }
  else value

  /**
   * Remove leading whitespace characters from a string
   *
   * @param value the string whose leading whitespace is to be removed
   * @return the string with leading whitespace removed. This may be the
   *         original string if there was no leading whitespace
   */
  def removeLeadingWhitespace(value: CharSequence): CharSequence = {
    val len = value.length
    // quick exit for common cases...
    if (len == 0 || value.charAt(0) > 32)
      return value
    var start = -1
    breakable {
      for (i <- 0 until len) {
        val c = value.charAt(i)
        if (c > 32 || !C0WHITE(c)) {
          start = i
          break()
        }
      }
    }
    if (start == 0)
      value
    else if (start < 0 || start == len)
      ""
    else
      value.subSequence(start, len)
  }

  /**
   * Determine if a string contains any whitespace
   *
   * @param value the string to be tested
   * @return true if the string contains a character that is XML whitespace, that is
   *         tab, newline, carriage return, or space
   */
  def containsWhitespace(value: CharSequence): Boolean = {
    var i = value.length - 1
    while ( {
      i >= 0
    }) {
      val c = value.charAt({
        i -= 1; i + 1
      })
      if (c <= 32 && C0WHITE(c)) return true
    }
    false
  }

  /**
   * Determine if a string is all-whitespace
   *
   * @param content the string to be tested
   * @return true if the supplied string contains no non-whitespace
   *         characters
   */
  def isWhite(content: CharSequence): Boolean = {
    if (content.isInstanceOf[CompressedWhitespace]) return true
    val len = content.length
    var i = 0
    while ( {
      i < len
    }) { // all valid XML 1.0 whitespace characters, and only whitespace characters, are <= 0x20
      // But XML 1.1 allows non-white characters that are also < 0x20, so we need a specific test for these
      val c = content.charAt({
        i += 1; i - 1
      })
      if (c > 32 || !C0WHITE(c)) return false
    }
    true
  }

  private val C0WHITE = Array(false, false, false, false, false, false, false, false, // 0-7
    false, true, true, false, false, true, false, false, // 8-15
    false, false, false, false, false, false, false, false, // 16-23
    false, false, false, false, false, false, false, false, // 24-31
    true // 32
  )
  /**
   * Determine if a character is whitespace
   *
   * @param c the character to be tested
   * @return true if the character is a whitespace character
   */
  def isWhite(c: Char) = c <= 32 && C0WHITE(c)

  /**
   * Normalize whitespace as defined in XML Schema. Note that this is not the same
   * as the XPath normalize-space() function, which is supported by the
   * {@link #collapseWhitespace} method
   *
   * @param in the string to be normalized
   * @return a copy of the string in which any whitespace character is replaced by
   *         a single space character
   */
  def normalizeWhitespace(in: CharSequence) = {
    val sb = new FastStringBuffer(in.length)
    for (i <- 0 until in.length) {
      val c = in.charAt(i)
      c match {
        case '\n' =>
        case '\r' =>
        case '\t' =>
          sb.cat(' ')
        case _ =>
          sb.cat(c)
      }
    }
    sb
  }

  /**
   * Collapse whitespace as defined in XML Schema. This is equivalent to the
   * XPath normalize-space() function
   *
   * @param in the string whose whitespace is to be collapsed
   * @return the string with any leading or trailing whitespace removed, and any
   *         internal sequence of whitespace characters replaced with a single space character.
   */
  def collapseWhitespace(in: CharSequence): CharSequence = {
    if (!containsWhitespace(in)) return in
    val len = in.length
    val sb = new FastStringBuffer(len)
    var inWhitespace = true
    var i = 0
    while ( {
      i < len
    }) {
      val c = in.charAt(i)
      c match {
        case '\n' =>
        case '\r' =>
        case '\t' =>
        case ' ' =>
          if (inWhitespace) {
            // remove the whitespace
          }
          else {
            sb.cat(' ')
            inWhitespace = true
          }
        case _ =>
          sb.cat(c)
          inWhitespace = false
      }
      i += 1
    }
    val nlen = sb.length
    if (nlen > 0 && sb.charAt(nlen - 1) == ' ') sb.setLength(nlen - 1)
    sb
  }

  /**
   * Remove leading and trailing whitespace. This has the same effect as collapseWhitespace,
   * but is cheaper, for use by data types that do not allow internal whitespace.
   *
   * @param in the input string whose whitespace is to be removed
   * @return the result of removing excess whitespace
   */
  def trimWhitespace(in: CharSequence): CharSequence = {
    if (in.length == 0)
      return in
    var first = 0
    var last = in.length - 1
    breakable {
      while (true) {
        val x = in.charAt(first)
        if (x > 32 || !C0WHITE(x))
          break()
        if ( { first += 1; first - 1 } >= last)
          return ""
      }
    }
    breakable {
      while (true) {
        val x = in.charAt(last)
        if (x > 32 || !C0WHITE(x))
          break()
        last -= 1
      }
    }
    if (first == 0 && last == in.length - 1)
      in
    else
      in.subSequence(first, last + 1)
  }

  /**
   * Trim leading and trailing whitespace from a string, returning a string.
   * This differs from the Java trim() method in that the only characters treated as
   * whitespace are space, \n, \r, and \t. The String#trim() method removes all C0
   * control characters (which is not the same thing under XML 1.1).
   *
   * @param s the string to be trimmed. If null is supplied, null is returned.
   * @return the string with leading and trailing whitespace removed.
   *         Returns null if and only if the supplied argument is null.
   */
  /*@Nullable*/ def trim(s: CharSequence): String = {
    if (s == null) return null
    trimWhitespace(s).toString
  }

  /**
   * An iterator that splits a string on whitespace boundaries, corresponding to the XPath 3.1 function tokenize#1
   */
  class Tokenizer extends AtomicIterator[StringValue] {
    private var input : Array[Char] = _
    private var position = 0

    def this(input: Array[Char]) {
      this()
      this.input = input
      this.position = 0
    }

    def this(input: CharSequence) {
      this()
      this.input = CharSlice.toCharArray(input)
      this.position = 0
    }

    override def next: StringValue = {
      var start = position
      val eol = input.length
      while ( {
        start < eol && isWhite(input(start))
      }) start += 1
      if (start >= eol) return null
      var end = start
      while ( {
        end < eol && !isWhite(input(end))
      }) end += 1
      position = end
      StringValue.makeStringValue(new CharSlice(input, start, end - start))
    }

    override def close() = {
      // no action
    }
  }

}

class Whitespace private() {
}