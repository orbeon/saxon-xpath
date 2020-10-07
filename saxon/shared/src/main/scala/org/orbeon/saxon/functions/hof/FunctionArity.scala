////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions.hof

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.functions.SystemFunction

import org.orbeon.saxon.om.Function

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.Int64Value

import org.orbeon.saxon.value.IntegerValue




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
    * @throws org.orbeon.saxon.trans.XPathException
    *          if a dynamic error occurs during the evaluation of the expression
    */
  def call(context: XPathContext, arguments: Array[Sequence]): IntegerValue = {
    val f: Function = arguments(0).head.asInstanceOf[Function]
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
