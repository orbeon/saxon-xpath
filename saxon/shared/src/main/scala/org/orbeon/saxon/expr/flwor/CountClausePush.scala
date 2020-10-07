////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.flwor

import org.orbeon.saxon.event.Outputter

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.Int64Value




/**
  * A tuple stream in push mode that implements a "count" clause in an XQuery 3.0 FLWOR expression
  */
class CountClausePush(outputter: Outputter,
                      var destination: TuplePush,
                      countClause: CountClause)
    extends TuplePush(outputter) {

  var slot: Int = countClause.getRangeVariable.getLocalSlotNumber

  var count: Int = 0

  /*
   * Notify the next tuple.
   */

  override def processTuple(context: XPathContext): Unit = {
    context.setLocalVariable(slot, new Int64Value())
    destination.processTuple(context)
  }

  /**
    * Close the tuple stream, indicating that no more tuples will be supplied
    */
  override def close(): Unit = {
    destination.close()
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2011-2020 Saxonica Limited
