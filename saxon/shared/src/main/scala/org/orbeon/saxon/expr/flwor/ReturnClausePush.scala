////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.flwor

import org.orbeon.saxon.event.Outputter

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.trans.XPathException




/**
  * The class represents the final stage in a push-mode tuple pipeline. The previous stages
  * have stored the values corresponding to the current tuple in local variables on the
  * stack; all that remains is to evaluate the return expression (with reference to these
  * variables) and send the results to the current receiver.
  */
class ReturnClausePush(outputter: Outputter,
                       private var returnExpr: Expression)
    extends TuplePush(outputter) {

  /**
    * Notify the availability of the next tuple. Before calling this method,
    * the supplier of the tuples must set all the variables corresponding
    * to the supplied tuple in the local stack frame associated with the context object
    *
    * @param context the dynamic evaluation context
    */
  override def processTuple(context: XPathContext): Unit = {
    returnExpr.process(getOutputter, context)
  }

  /**
    * Close the tuple stream, indicating that no more tuples will be supplied
    */
  override def close(): Unit = ()
// no action
// no action

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
