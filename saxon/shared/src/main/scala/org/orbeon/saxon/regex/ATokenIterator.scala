////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.regex

import org.orbeon.saxon.tree.iter.AtomicIterator
import org.orbeon.saxon.value.{AtomicValue, StringValue}




class ATokenIterator(private var input: UnicodeString,
                     private var matcher: REMatcher)
    extends AtomicIterator[AtomicValue] {

  /*@Nullable*/

  private var current: UnicodeString = _

  private var prevEnd: Int = 0

  def next(): StringValue = {
    if (prevEnd < 0) {
      current = null
      return null
    }
    if (matcher.`match`(input, prevEnd)) {
      val start: Int = matcher.getParenStart(0)
      current = input.uSubstring(prevEnd, start)
      prevEnd = matcher.getParenEnd(0)
    } else {
      current = input.uSubstring(prevEnd, input.uLength())
      prevEnd = -1
    }
    currentStringValue()
  }

  private def currentStringValue(): StringValue =
    StringValue.makeStringValue(current)

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A ATokenIterator is an iterator over the strings that result from tokenizing a string using a regular expression
  */
