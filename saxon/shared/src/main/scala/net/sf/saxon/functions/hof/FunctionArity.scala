////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.functions.hof

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.functions.SystemFunction

import net.sf.saxon.om.Function

import net.sf.saxon.om.Sequence

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.Int64Value

import net.sf.saxon.value.IntegerValue




class FunctionArity extends SystemFunction {

  /*@NotNull*/

  override def getIntegerBounds(): Array[IntegerValue] =
    Array(Int64Value.ZERO, Int64Value.makeIntegerValue(65535))

  /**
    * Evaluate the expression
    *
    * @param context   the dynamic evaluation context
    * @param arguments the values of the arguments, supplied as SequenceIterators
    * @return the result of the evaluation, in the form of a SequenceIterator
    * @throws net.sf.saxon.trans.XPathException
    *          if a dynamic error occurs during the evaluation of the expression
    */
  def call(context: XPathContext, arguments: Array[Sequence]): IntegerValue = {
    val f: Function = arguments(0).head().asInstanceOf[Function]
    Int64Value.makeIntegerValue(f.getArity)
  }

}

// Copyright (c) 2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class implements the function function-arity(), which is a standard function in XPath 3.0
  */
// Copyright (c) 2012-2020 Saxonica Limited
