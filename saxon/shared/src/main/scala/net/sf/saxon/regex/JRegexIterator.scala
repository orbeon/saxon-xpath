////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.regex

import net.sf.saxon.expr.LastPositionFinder
import net.sf.saxon.functions.Count
import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.util.FastStringBuffer
import net.sf.saxon.value.StringValue
import net.sf.saxon.z.IntHashMap
import net.sf.saxon.z.IntToIntHashMap
import java.util.ArrayList
import java.util.EnumSet
import java.util.Iterator
import java.util.List
import java.util.regex.Matcher
import java.util.regex.Pattern

import net.sf.saxon.om.SequenceIterator.Property
import net.sf.saxon.om.SequenceIterator.Property.Property
import net.sf.saxon.regex.RegexIterator.MatchHandler

import scala.util.control.Breaks._


class JRegexIterator(private var theString: String,
                     private var pattern: Pattern)
  extends RegexIterator
    with LastPositionFinder {

  // the Matcher object that does the matching, and holds the state
  private var matcher: Matcher = pattern.matcher(theString)

  // the string most recently returned by the iterator
  private var current: String = _

  // if the last string was a matching string, null; otherwise the next substring
  private var next1: String = null

  // the position in the input string of the end of the last match or non-match
  private var prevEnd: Int = 0

  private var nestingTable: IntToIntHashMap = null

  override def getLength(): Int = {
    val another: JRegexIterator = new JRegexIterator(theString, pattern)
    Count.steppingCount(another)
  }

  def next(): StringValue = {
    if (next1 == null && prevEnd >= 0) {
      // we've returned a match (or we're at the start), so find the next match
      if (matcher.find()) {
        val start: Int = matcher.start()
        val end: Int = matcher.end()
        if (prevEnd == start) {
          // there's no intervening non-matching string to return
          next1 = null
          current = theString.substring(start, end)
          prevEnd = end
        } else {
          // return the non-matching substring first
          current = theString.substring(prevEnd, start)
          next1 = theString.substring(start, end)
        }
      } else {
        // there are no more regex matches, we must return the final non-matching text if any
        if (prevEnd < theString.length) {
          current = theString.substring(prevEnd)
          next1 = null
        } else {
          // this really is the end...
          current = null
          prevEnd = -1
          null
        }
        prevEnd = -1
      }
    } else {
      // we've returned a non-match, so now return the match that follows it, if there is one
      if (prevEnd >= 0) {
        current = next1
        next1 = null
        prevEnd = matcher.end()
      } else {
        current = null
        null
      }
    }
    StringValue.makeStringValue(current)
  }

  def getRegexProperties(): Set[Property] = Set(Property.LAST_POSITION_FINDER)

  def isMatching(): Boolean = next1 == null && prevEnd >= 0

  def getRegexGroup(number: Int): String = {
    if (!isMatching) null
    if (number > matcher.groupCount() || number < 0) ""
    val s: String = matcher.group(number)
    if (s == null) ""
    s
  }

  /**
   * Get the number of captured groups
   */
  def getNumberOfGroups(): Int = matcher.groupCount()

  def processMatchingSubstring(action: MatchHandler): Unit = {
    val c: Int = matcher.groupCount()
    if (c == 0) {
      action.characters(current)
    } else {
      // The "actions" in each list are: +N: start group N; -N: end group N.
      val actions: IntHashMap[List[Integer]] = new IntHashMap[List[Integer]](c)
      var i: Int = 1
      breakable {
        while (i <= c) {
          val start: Int = matcher.start(i) - matcher.start()
          if (start != -1) {
            val end: Int = matcher.end(i) - matcher.start()
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
                computeNestingTable()
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
                  break
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
            i += 1;
            i - 1
          }
        }
      }
      val buff: FastStringBuffer = new FastStringBuffer(current.length)
      for (i <- 0 until current.length + 1) {
        val events: List[Integer] = actions.get(i)
        if (events != null) {
          if (buff.length > 0) {
            action.characters(buff)
            buff.setLength(0)
          }
          var ii: Iterator[Integer] = events.iterator()
          while (ii.hasNext) {
            val group: Int = ii.next()
            if (group > 0) {
              action.onGroupStart(group)
            } else {
              action.onGroupEnd(-group)
            }
          }
        }
        if (i < current.length) {
          buff.cat(current.charAt(i))
        }
      }
      if (buff.length > 0) {
        action.characters(buff)
      }
    }
    // Create a map from positions in the string to lists of actions.
    // Create a map from positions in the string to lists of actions.
  }

  private def computeNestingTable(): Unit = {
    nestingTable = new IntToIntHashMap(16)
    val s: String = pattern.pattern()
    val stack: Array[Int] = Array.ofDim[Int](s.length)
    var tos: Int = 0
    var group: Int = 1
    var inBrackets: Int = 0
    stack({
      tos += 1; tos - 1
    }) = 0
    var i = 0;
    while (i < s.length) {
      val ch: Char = s.charAt(i)
      if (ch == '\'') {
        {
          i += 1; i - 1
        }
      } else if (ch == '[') {
        {
          inBrackets += 1; inBrackets - 1
        }
      } else if (ch == ']') {
        {
          inBrackets -= 1; inBrackets + 1
        }
      } else if (ch == '(' && s.charAt(i + 1) != '?' && inBrackets == 0) {
        nestingTable.put(group, stack(tos - 1))
        stack({
          tos += 1; tos - 1
        }) = {
          group += 1; group - 1
        }
      } else if (ch == ')' && inBrackets == 0) {
        {
          tos -= 1; tos + 1
        }
      }
    }
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//import java.util.regex.Matcher;
//import java.util.regex.Pattern;
/**
 * Class JRegexIterator - provides an iterator over matched and unmatched substrings.
 * This implementation of RegexIterator uses the JDK regular expression engine.
 */
