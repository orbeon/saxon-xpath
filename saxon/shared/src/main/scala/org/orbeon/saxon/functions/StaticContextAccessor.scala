////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.Literal

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.expr.parser.RetainedStaticContext

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.AtomicValue

import org.orbeon.saxon.value.StringValue

import StaticContextAccessor._




object StaticContextAccessor {

  class DefaultCollation extends StaticContextAccessor {

    override def evaluate(rsc: RetainedStaticContext): AtomicValue =
      new StringValue(rsc.getDefaultCollationName)

  }

}

abstract class StaticContextAccessor extends SystemFunction {

  def evaluate(rsc: RetainedStaticContext): AtomicValue

  /**
    * Evaluate the expression
    *
    * @param context   the dynamic evaluation context
    * @param arguments the values of the arguments, supplied as Sequences
    * @return the result of the evaluation, in the form of a Sequence
    * @throws org.orbeon.saxon.trans.XPathException
    *          if a dynamic error occurs during the evaluation of the expression
    */
  def call(context: XPathContext, arguments: Array[Sequence]): AtomicValue =
    evaluate(getRetainedStaticContext)

  def makeFunctionCall(arguments: Array[Expression]): Expression =
    Literal.makeLiteral(evaluate(getRetainedStaticContext))

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A StaticContextAccessor is a function that takes no arguments, but operates implicitly on the
  * static context. In the case of a dynamic call, the context that is used is the one at the point
  * where the function item is created.
  */
