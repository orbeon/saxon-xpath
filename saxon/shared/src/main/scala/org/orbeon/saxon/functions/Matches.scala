////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.regex.RegularExpression

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.AtomicValue

import org.orbeon.saxon.value.BooleanValue

import org.orbeon.saxon.value.StringValue


class Matches extends RegexFunction {

  def allowRegexMatchingEmptyString(): Boolean = true

  def evalMatches(input: AtomicValue,
                  regex: AtomicValue,
                  flags: CharSequence,
                  context: XPathContext): Boolean = {
    var re: RegularExpression = null
    if (regex == null) {
      return false
    }
    try {
      var lang: String = "XP30"
      if (context.getConfiguration.getXsdVersion == Configuration.XSD11) {
        lang += "/XSD11"
      }
      re = context.getConfiguration.compileRegularExpression(
        regex.getStringValueCS,
        flags.toString,
        lang,
        null)
    } catch {
      case err: XPathException => {
        val de: XPathException = new XPathException(err)
        de.maybeSetErrorCode("FORX0002")
        de.setXPathContext(context)
        throw de
      }

    }
    re.containsMatch(input.getStringValueCS)
  }

  /**
   * Evaluate the expression
   *
   * @param context   the dynamic evaluation context
   * @param arguments the values of the arguments, supplied as Sequences
   * @return the result of the evaluation, in the form of a Sequence
   * @throws org.orbeon.saxon.trans.XPathException
   * if a dynamic error occurs during the evaluation of the expression
   */
  def call(context: XPathContext, arguments: Array[Sequence]): BooleanValue = {
    val re: RegularExpression = getRegularExpression(arguments)
    val arg: StringValue = arguments(0).head.asInstanceOf[StringValue]
    val in: CharSequence = if (arg == null) "" else arg.getStringValueCS
    val result: Boolean = re.containsMatch(in)
    BooleanValue.get(result)
  }

  override def getCompilerName(): String = "MatchesCompiler"

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * This class implements the 3-argument matches() function for regular expression matching
 */
