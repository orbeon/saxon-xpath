////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.regex

import org.orbeon.saxon.lib.Feature
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.iter.AtomicIterator
import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.value.{AtomicValue, StringValue}

import java.{util => ju}


/**
 * Glue class to interface the Jakarta regex engine to Saxon
 * (The prefix 'A' indicates an Apache regular expression, as distinct from
 * a JDK regular expression).
 */
class ARegularExpression(
  pattern      : CharSequence,
  var rawFlags : String,
  hostLanguage : String,
  warnings     : ju.List[String],
  config       : Configuration
)
  extends RegularExpression {

  var rawPattern: UnicodeString = _
  var regex: REProgram = _
  var reFlags: REFlags = new REFlags(rawFlags, hostLanguage)

  locally {
    rawPattern = UnicodeString.makeUnicodeString(pattern)
    val comp2 = new RECompiler
    comp2.setFlags(reFlags)
    regex = comp2.compile(rawPattern)
    if (warnings != null)
      comp2.getWarnings.forEach { s =>
        warnings.add(s)
      }
    if (config != null)
      regex.setBacktrackingLimit(config.getConfigurationProperty(Feature.REGEX_BACKTRACKING_LIMIT))
  }

  /**
   * Determine whether the regular expression matches a given string in its entirety
   *
   * @param input the string to match
   * @return true if the string matches, false otherwise
   */
  def matches(input: CharSequence): Boolean = {
    if (StringValue.isEmpty(input) && regex.isNullable)
      return true
    val matcher = new REMatcher(regex)
    matcher.anchoredMatch(UnicodeString.makeUnicodeString(input))
  }

  /**
   * Determine whether the regular expression contains a match of a given string
   *
   * @param input the string to match
   * @return true if the string matches, false otherwise
   */
  def containsMatch(input: CharSequence): Boolean = {
    val matcher = new REMatcher(regex)
    matcher.`match`(UnicodeString.makeUnicodeString(input), 0)
  }

  /**
   * Use this regular expression to tokenize an input string.
   *
   * @param input the string to be tokenized
   * @return a SequenceIterator containing the resulting tokens, as objects of type StringValue
   */
  def tokenize(input: CharSequence): AtomicIterator[AtomicValue] =
    new ATokenIterator(UnicodeString.makeUnicodeString(input), new REMatcher(regex))

  /**
   * Use this regular expression to analyze an input string, in support of the XSLT
   * analyze-string instruction. The resulting RegexIterator provides both the matching and
   * non-matching substrings, and allows them to be distinguished. It also provides access
   * to matched subgroups.
   *
   * @param input the character string to be analyzed using the regular expression
   * @return an iterator over matched and unmatched substrings
   */
  def analyze(input: CharSequence): RegexIterator =
    new ARegexIterator(
      UnicodeString.makeUnicodeString(input),
      rawPattern,
      new REMatcher(regex)
    )

  /**
   * Replace all substrings of a supplied input string that match the regular expression
   * with a replacement string.
   *
   * @param input       the input string on which replacements are to be performed
   * @param replacement the replacement string in the format of the XPath replace() function
   * @return the result of performing the replacement
   * @throws XPathException
   * if the replacement string is invalid
   */
  def replace(input: CharSequence, replacement: CharSequence): CharSequence = {
    val matcher = new REMatcher(regex)
    val in      = UnicodeString.makeUnicodeString(input)
    val rep     = UnicodeString.makeUnicodeString(replacement)
    matcher.replace(in, rep)
  }

  /**
   * Replace all substrings of a supplied input string that match the regular expression
   * with a replacement string.
   *
   * @param input    the input string on which replacements are to be performed
   * @param replacer the replacement string in the format of the XPath replace() function
   * @return the result of performing the replacement
   * @throws XPathException if the replacement string is invalid
   */
  def replaceWith(
    input    : CharSequence,
    replacer : CharSequence => CharSequence
  ): CharSequence = {
    val matcher = new REMatcher(regex)
    val in      = UnicodeString.makeUnicodeString(input)
    matcher.replaceWith(in, replacer)
  }

  /**
   * Get the flags used at the time the regular expression was compiled.
   *
   * @return a string containing the flags
   */
  def getFlags: String = rawFlags
}
