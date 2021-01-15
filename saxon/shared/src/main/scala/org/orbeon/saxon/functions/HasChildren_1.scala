////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.om.{NodeInfo, Sequence}
import org.orbeon.saxon.value.BooleanValue


/**
  * This class implements the function fn:has-children($node), which is a standard function in XPath 3.0
  */
class HasChildren_1 extends SystemFunction {

  /**
    * Evaluate the function dynamically
    *
    * @param context   the dynamic evaluation context
    * @param arguments the values of the arguments, supplied as Sequences
    * @return the result of the evaluation, in the form of a Sequence
    * @throws org.orbeon.saxon.trans.XPathException
    *          if a dynamic error occurs during the evaluation of the expression
    */
  def call(context: XPathContext, arguments: Array[Sequence]): BooleanValue = {
    val arg = arguments(0).head.asInstanceOf[NodeInfo]
    if (arg == null)
      BooleanValue.FALSE
    else
      BooleanValue.get(arg.hasChildNodes)
  }
}

