////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.functions

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.Sequence

import net.sf.saxon.trans.XPathException




class Unordered extends SystemFunction {

  /**
    * Evaluate the expression
    *
    * @param context   the dynamic evaluation context
    * @param arguments the values of the arguments, supplied as Sequences
    * @return the result of the evaluation, in the form of a Sequence
    * @throws net.sf.saxon.trans.XPathException
    *          if a dynamic error occurs during the evaluation of the expression
    */
  def call(context: XPathContext, arguments: Array[Sequence]): Sequence =
    arguments(0)

  /**
    * Make an expression that either calls this function, or that is equivalent to a call
    * on this function
    *
    * @param arguments the supplied arguments to the function call
    * @return either a function call on this function, or an expression that delivers
    * the same result
    */
  def makeFunctionCall(arguments: Array[Expression]): Expression =
    arguments(0)

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * XPath 2.0 unordered() function
  */
