////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.regex

/**
 * Class representing a set of regular expression flags (some combination of i, m, s, x, q).
 * Also contains options affecting the regular expression dialect: whether or not XPath 2.0
 * and XPath 3.0 extensions to XSD regex syntax are accepted.
 */


/**
 * Create the regular expression flags
 *
 * @param flags    a string containing zero or more of 'i', 'x', 'm', 's'
 * @param language one of "XSD10", "XSD11", "XP20", or "XP30" indicating the regular expression dialect.
 *                 Also allow combinations, e.g. "XP20/XSD11".
 */
class REFlags(val flags: String, val language: String){
  if (language == "XSD10") {
    // no action
  }
  else if (language.contains("XSD11")) {
    allowUnknownBlockNames = !language.contains("XP")
    xsd11 = true
  }
  if (language.contains("XP20")) xpath20 = true
  else if (language.contains("XP30")) {
    xpath20 = true
    xpath30 = true
  }
  val semi = flags.indexOf(';')
  val endStd = if (semi >= 0) semi
  else flags.length
  for (i <- 0 until endStd) {
    val c = flags.charAt(i)
    c match {
      case 'i' =>
        caseIndependent = true
      case 'm' =>
        multiLine = true
      case 's' =>
        singleLine = true
      case 'q' =>
        literal = true
        if (!xpath30) throw new RESyntaxException("'q' flag requires XPath 3.0 to be enabled")
      case 'x' =>
        allowWhitespace = true
      case _ =>
        throw new RESyntaxException("Unrecognized flag '" + c + "'")
    }
  }
  for (i <- semi + 1 until flags.length) {
    val c = flags.charAt(i)
    c match {
      case 'g' =>
        debug = true
      case 'k' =>
        allowUnknownBlockNames = true
      case 'K' =>
        allowUnknownBlockNames = false
    }
  }
  private var caseIndependent = false
  private var multiLine = false
  private var singleLine = false
  private var allowWhitespace = false
  private var literal = false
  private var xpath20 = false
  private var xpath30 = false
  private var xsd11 = false
  private var debug = false // flags = ";g"
  private var allowUnknownBlockNames = false //flags = ";k"
  def isCaseIndependent = caseIndependent

  def isMultiLine = multiLine

  def isSingleLine = singleLine

  def isAllowWhitespace = allowWhitespace

  def isLiteral = literal

  def isAllowsXPath20Extensions = xpath20

  def isAllowsXPath30Extensions = xpath30

  def isAllowsXSD11Syntax = xsd11

  def setDebug(debug: Boolean) = this.debug = debug

  def isDebug = debug

  def setAllowUnknownBlockNames(allow: Boolean) = this.allowUnknownBlockNames = allow

  def isAllowUnknownBlockNames = allowUnknownBlockNames
}