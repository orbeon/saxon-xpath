////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.lib.StringCollator

import org.orbeon.saxon.lib.SubstringMatcher

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.StringValue

import SubstringBefore._




object SubstringBefore {

  private def substringBefore(arg0: StringValue,
                              arg1: StringValue,
                              collator: SubstringMatcher): StringValue = {
    val s0: String = arg0.getStringValue
    val s1: String = arg1.getStringValue
    val result: StringValue = new StringValue(collator.substringBefore(s0, s1))
    if (arg0.isKnownToContainNoSurrogates) {
      result.setContainsNoSurrogates()
    }
    result
  }

}

/**
  * Implements the fn:substring-before() function with the collation already known
  */
class SubstringBefore extends CollatingFunctionFixed {

  override def isSubstringMatchingFunction(): Boolean = true

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
    val arg1: StringValue = arguments(1).head.asInstanceOf[StringValue]
    if (arg1 == null || arg1.isZeroLength) {
      StringValue.EMPTY_STRING
    }
    val arg0: StringValue = arguments(0).head.asInstanceOf[StringValue]
    if (arg0 == null || arg0.isZeroLength) {
      StringValue.EMPTY_STRING
    }
    val collator: StringCollator = getStringCollator
    substringBefore(arg0, arg1, collator.asInstanceOf[SubstringMatcher])
  }

  override def getCompilerName(): String = "SubstringBeforeCompiler"

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
