////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.regex.RegexIterator

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.NumericValue

import org.orbeon.saxon.value.StringValue




class RegexGroup extends SystemFunction {

  /**
    * Evaluate the expression
    *
    * @param context   the dynamic evaluation context
    * @param arguments the values of the arguments, supplied as Sequences
    * @return the result of the evaluation, in the form of a Sequence
    * @throws org.orbeon.saxon.trans.XPathException
    *          if a dynamic error occurs during the evaluation of the expression
    */
  def call(context: XPathContext, arguments: Array[Sequence]): StringValue = {
    val iter: RegexIterator = context.getCurrentRegexIterator
    if (iter == null) {
      StringValue.EMPTY_STRING
    }
    val gp0: NumericValue = arguments(0).head.asInstanceOf[NumericValue]
    val s: String = iter.getRegexGroup(gp0.longValue().toInt)
    StringValue.makeStringValue(s)
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
