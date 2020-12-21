////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.flwor

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.Int64Value




/**
  * A tuple stream that implements a "count" clause in an XQuery 3.0 FLWOR expression
  */
class CountClausePull(var base: TuplePull, countClause: CountClause)
    extends TuplePull {

  var slot: Int = countClause.getRangeVariable.getLocalSlotNumber

  var count: Int = 0

  /**
    * Move on to the next tuple. Before returning, this method must set all the variables corresponding
    * to the "returned" tuple in the local stack frame associated with the context object
    *
    * @param context the dynamic evaluation context
    * @return true if another tuple has been generated; false if the tuple stream is exhausted. If the
    *         method returns false, the values of the local variables corresponding to this tuple stream
    *         are undefined.
    */
  override def nextTuple(context: XPathContext): Boolean = {
    if (!base.nextTuple(context)) {
      count = 0
      context.setLocalVariable(slot, Int64Value.ZERO)
      return false
    }
    context.setLocalVariable(slot, new Int64Value())
    true
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2011-2020 Saxonica Limited
