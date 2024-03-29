////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.flwor

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.trans.XPathException


/**
 * Implements the changes to a tuple stream effected by the Let clause in a FLWOR expression
 */
class LetClausePull(var base: TuplePull, var letClause: LetClause)
  extends TuplePull {

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
      return false
    }
    val `val`: Sequence =
      letClause.getEvaluator.evaluate(letClause.getSequence, context)
    context.setLocalVariable(letClause.getRangeVariable.getLocalSlotNumber,
      `val`)
    true
  }

  /**
   * Close the tuple stream, indicating that although not all tuples have been read,
   * no further tuples are required and resources can be released
   */
  override def close(): Unit = {
    base.close()
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
