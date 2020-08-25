////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.regex

import net.sf.saxon.tree.iter.AtomicIterator

import net.sf.saxon.value.StringValue

import java.util.regex.Matcher

import java.util.regex.Pattern


class JTokenIterator(private var input: CharSequence,
                     private var pattern: Pattern)
  extends AtomicIterator[StringValue] {

  private var matcher: Matcher = pattern.matcher(input)

  /*@Nullable*/

  private var current: CharSequence = _

  private var prevEnd: Int = 0

  def next(): StringValue = {
    if (prevEnd < 0) {
      current = null
      return null
    }
    if (matcher.find()) {
      current = input.subSequence(prevEnd, matcher.start())
      prevEnd = matcher.end()
    } else {
      current = input.subSequence(prevEnd, input.length)
      prevEnd = -1
    }
    StringValue.makeStringValue(current)
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
 * A JTokenIterator is an iterator over the strings that result from tokenizing a string using
 * a regular expression, in this case a regular expression evaluated using the JDK regex engine
 */
