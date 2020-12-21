////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.Function

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.trans.XPathException




abstract class ContextAccessorFunction extends SystemFunction {

  def bindContext(context: XPathContext): Function

  /**
    * Evaluate the expression
    *
    * @param context   the dynamic evaluation context
    * @param arguments the values of the arguments, supplied as Sequences
    * @return the result of the evaluation, in the form of a Sequence
    * @throws org.orbeon.saxon.trans.XPathException if a dynamic error occurs during the evaluation of the expression
    */
  def call(context: XPathContext, arguments: Array[Sequence]): Sequence =
    bindContext(context).call(context, arguments)

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A ContextAccessorFunction is a function that is dependent on the dynamic context. In the case
  * of dynamic function calls, the context is bound at the point where the function is created,
  * not at the point where the function is called.
  */
