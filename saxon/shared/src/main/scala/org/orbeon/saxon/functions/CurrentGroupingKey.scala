////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.{Expression, XPathContext}
import org.orbeon.saxon.om.Sequence
import org.orbeon.saxon.trans.XPathException


/**
  * Implements the XSLT function current-grouping-key()
  */
class CurrentGroupingKey extends SystemFunction {

  /**
    * Make an expression that either calls this function, or that is equivalent to a call
    * on this function
    *
    * @param arguments the supplied arguments to the function call
    * @return either a function call on this function, or an expression that delivers
    * the same result
    */
  override def makeFunctionCall(arguments: Expression*): Expression =
    new CurrentGroupingKeyCall()

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence =
    throw new XPathException(
      "Dynamic call on current-grouping-key() fails (the current group is absent)",
      "XTDE1071")
}
