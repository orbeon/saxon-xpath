////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.regex

import java.util.{ArrayList, List}

import org.orbeon.saxon.expr.LastPositionFinder
import org.orbeon.saxon.om.SequenceIterator.Property
import org.orbeon.saxon.om.SequenceIterator.Property.Property
import org.orbeon.saxon.regex.ARegexIterator._
import org.orbeon.saxon.regex.RegexIterator.MatchHandler
import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.value.StringValue
import org.orbeon.saxon.z.{IntHashMap, IntToIntHashMap}

import scala.util.control.Breaks._


/**
 * Class ARegexIterator - provides an iterator over matched and unmatched substrings.
 * This implementation of RegexIterator uses the modified Jakarta regular expression engine.
 */
object ARegexIterator {

  def computeNestingTable(regex: UnicodeString): IntToIntHashMap = {
    // See bug 3211
    val nestingTable: IntToIntHashMap = new IntToIntHashMap(16)
    val stack: Array[Int] = Array.ofDim[Int](regex.uLength)
    var tos: Int = 0
    val captureStack: Array[Boolean] = Array.ofDim[Boolean](regex.uLength)
    var captureTos: Int = 0
    var group: Int = 1
    var inBrackets: Int = 0
    stack({
      tos += 1
      tos - 1
    }) = 0
    var i: Int = 0
    val regexLen = regex.uLength
    while (i <= regexLen) {
      val ch: Int = regex.uCharAt(i)
      if (ch == '\\') {
        {
          i += 1
          i - 1
        }
      } else if (ch == '[') {
        {
          inBrackets += 1
          inBrackets - 1
        }
      } else if (ch == ']') {
        {
          inBrackets -= 1
          inBrackets + 1
        }
      } else if (ch == '(' && inBrackets == 0) {
        val capture: Boolean = regex.uCharAt(i + 1) != '?'
        captureStack({
          captureTos += 1
          captureTos - 1
        }) = capture
        if (capture) {
          nestingTable.put(group, stack(tos - 1))
          stack({
            tos += 1
            tos - 1
          }) = {
            group += 1
            group - 1
          }
        }
      } else if (ch == ')' && inBrackets == 0) {
        val capture: Boolean = captureStack({
          captureTos -= 1
          captureTos
        })
        if (capture) {
          {
            tos -= 1
            tos + 1
          }
        }
      }
    }
    nestingTable
  }

}

class ARegexIterator(private var theString: UnicodeString,
                     private var regex: UnicodeString,
                     private var matcher: REMatcher)
  extends RegexIterator
    with LastPositionFinder {

  // the string most recently returned by the iterator
  private var current: UnicodeString = _

  // if the last string was a matching string, null; otherwise the next substring
  private var nextSubStr: UnicodeString = null

  // the position in the input string of the end of the last match or non-match
  private var prevEnd: Int = 0

  private var nestingTable: IntToIntHashMap = null

  // indicates the last match was zero length
  private var skip: Boolean = false

  def getLength: Int = {
    val another: ARegexIterator =
      new ARegexIterator(theString, regex, new REMatcher(matcher.getProgram))
    var n: Int = 0
    while (another.next() != null) {
      n += 1
      n - 1
    }
    n
  }

  def next(): StringValue = {
    if (nextSubStr == null && prevEnd >= 0) {
      // we've returned a match (or we're at the start), so find the next match
      var searchStart: Int = prevEnd
      if (skip) {
        // previous match was zero-length
        searchStart += 1
        if (searchStart >= theString.uLength) {
          if (prevEnd < theString.uLength) {
            current = theString.uSubstring(prevEnd, theString.uLength)
            nextSubStr = null
          } else {
            current = null
            prevEnd = -1
            return null
          }
        }
      }
      if (matcher.`match`(theString, searchStart)) {
        val start: Int = matcher.getParenStart(0)
        val end: Int = matcher.getParenEnd(0)
        skip = start == end
        if (prevEnd == start) {
          // there's no intervening non-matching string to return
          nextSubStr = null
          current = theString.uSubstring(start, end)
          prevEnd = end
        } else {
          // return the non-matching substring first
          current = theString.uSubstring(prevEnd, start)
          nextSubStr = theString.uSubstring(start, end)
        }
      } else {
        // there are no more regex matches, we must return the final non-matching text if any
        if (prevEnd < theString.uLength) {
          current = theString.uSubstring(prevEnd, theString.uLength)
          nextSubStr = null
        } else {
          // this really is the end...
          current = null
          prevEnd = -1
          return null
        }
        prevEnd = -1
      }
    } else {
      // we've returned a non-match, so now return the match that follows it, if there is one
      if (prevEnd >= 0) {
        current = nextSubStr
        nextSubStr = null
        prevEnd = matcher.getParenEnd(0)
      } else {
        current = null
        return null
      }
    }
    currentStringValue()
  }

  private def currentStringValue(): StringValue =
    StringValue.makeStringValue(current)

  def getRegExProperties: Set[Property] = Set(Property.LAST_POSITION_FINDER)

  def isMatching: Boolean = nextSubStr == null && prevEnd >= 0

  def getRegexGroup(number: Int): String = {
    if (!isMatching) {
      return null
    }
    if (number >= matcher.getParenCount || number < 0) return ""
    val us: UnicodeString = matcher.getParen(number)
    if (us == null) "" else us.toString
  }

  /**
   * Get the number of captured groups
   */
  override def getNumberOfGroups: Int = matcher.getParenCount

  def processMatchingSubstring(action: MatchHandler): Unit = {
    val c: Int = matcher.getParenCount - 1
    if (c == 0) {
      action.characters(current.toString)
    } else {
      // The "actions" in each list are: +N: start group N; -N: end group N.
      val actions: IntHashMap[List[Integer]] = new IntHashMap[List[Integer]](c)
      var i: Int = 1
      breakable {
        while (i <= c) {
          val start: Int = matcher.getParenStart(i) - matcher.getParenStart(0)
          if (start != -1) {
            val end: Int = matcher.getParenEnd(i) - matcher.getParenStart(0)
            if (start < end) {
              // Add the start action after all other actions on the list for the same position
              var s: List[Integer] = actions.get(start)
              if (s == null) {
                s = new ArrayList[Integer](4)
                actions.put(start, s)
              }
              s.add(i)
              // Add the end action before all other actions on the list for the same position
              var e: List[Integer] = actions.get(end)
              if (e == null) {
                e = new ArrayList[Integer](4)
                actions.put(end, e)
              }
              e.add(0, -i)
            } else {
              // So we need to go back to the original regex to determine the group nesting
              if (nestingTable == null) {
                nestingTable = computeNestingTable(regex)
              }
              val parentGroup: Int = nestingTable.get(i)
              // if present; otherwise after all existing events for this position
              var s: List[Integer] = actions.get(start)
              if (s == null) {
                s = new ArrayList[Integer](4)
                actions.put(start, s)
                s.add(i)
                s.add(-i)
              } else {
                var pos: Int = s.size
                for (e <- 0 until s.size if s.get(e) == -parentGroup) {
                  pos = e
                  break()
                }
                s.add(pos, -i)
                s.add(pos, i)
              }
            }
            // zero-length group (start==end). The problem here is that the information available
            // from Java isn't sufficient to determine the nesting of groups: match("a", "(a(b?))")
            // and match("a", "(a)(b?)") will both give the same result for group 2 (start=1, end=1).
            // insert the start and end events immediately before the end event for the parent group,
            // zero-length group (start==end). The problem here is that the information available
            // from Java isn't sufficient to determine the nesting of groups: match("a", "(a(b?))")
            // and match("a", "(a)(b?)") will both give the same result for group 2 (start=1, end=1).
            // insert the start and end events immediately before the end event for the parent group,
          }
          {
            i += 1
            i - 1
          }
        }
      }
      val buff = new FastStringBuffer(current.uLength)
      for (i <- 0 until current.uLength + 1) {
        val events: List[Integer] = actions.get(i)
        if (events != null) {
          if (buff.length > 0) {
            action.characters(buff)
            buff.setLength(0)
          }
          events.forEach { group =>
            if (group > 0) {
              action.onGroupStart(group)
            } else {
              action.onGroupEnd(-group)
            }
          }
        }
        if (i < current.uLength) {
          buff.appendWideChar(current.uCharAt(i))
        }
      }
      if (buff.length > 0) {
        action.characters(buff)
      }
    }
  }
}
