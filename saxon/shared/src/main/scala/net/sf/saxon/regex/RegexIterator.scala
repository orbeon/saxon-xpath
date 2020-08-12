////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.regex

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.StringValue

import RegexIterator._




object RegexIterator {

  trait MatchHandler {

    def characters(s: CharSequence): Unit

    def onGroupStart(groupNumber: Int): Unit

    def onGroupEnd(groupNumber: Int): Unit

  }

}

/**
  * This interface defines an iterator that supports the evaluation of xsl:analyze-string.
  * It returns all the matching and non-matching substrings in an input string, and
  * provides access to their captured groups
  */
trait RegexIterator extends SequenceIterator {

  /**
    * Get the next item in the sequence. This method changes the state of the
    * iterator.
    */
  override def next(): StringValue

  def isMatching(): Boolean

  def getNumberOfGroups(): Int

  /*@Nullable*/

  def getRegexGroup(number: Int): String

  def processMatchingSubstring(action: MatchHandler): Unit

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
