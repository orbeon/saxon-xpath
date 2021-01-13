////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.flwor

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.om.{FocusTrackingIterator, Item}
import org.orbeon.saxon.value.{EmptySequence, Int64Value}


/**
 * This class represents the tuple stream returned by a "for" clause in a FLWOR expression
 */
class ForClauseOuterPull(base: TuplePull, forClause: ForClause)
  extends ForClausePull(base, forClause) {

  /**
   * Deliver the next output tuple. Before returning, this method must set all the variables corresponding
   * to the output tuple in the local stack frame associated with the context object
   *
   * @param context the dynamic evaluation context
   * @return true if another tuple has been generated; false if the tuple stream is exhausted. If the
   *         method returns false, the values of the local variables corresponding to this tuple stream
   *         are undefined.
   */
  override def nextTuple(context: XPathContext): Boolean = {
    while (true) {
      var next: Item = null
      if (currentIteration == null) {
        if (!base.nextTuple(context))
          return false
        currentIteration = new FocusTrackingIterator(forClause.getSequence.iterate(context))
        next = currentIteration.next()
        if (next == null) {
          context.setLocalVariable(forClause.getRangeVariable.getLocalSlotNumber, EmptySequence.getInstance)
          if (forClause.getPositionVariable != null) {
            context.setLocalVariable(
              forClause.getPositionVariable.getLocalSlotNumber,
              Int64Value.ZERO)
          }
          currentIteration = null
          return true
        }
      } else {
        next = currentIteration.next()
      }
      if (next != null) {
        context.setLocalVariable(forClause.getRangeVariable.getLocalSlotNumber, next)
        if (forClause.getPositionVariable != null)
          context.setLocalVariable(
            forClause.getPositionVariable.getLocalSlotNumber,
            new Int64Value(currentIteration.position)
          )
        return true
      } else {
        currentIteration = null
      }
    }
    false // shouldn't reach here
  }

  /**
   * Close the tuple stream, indicating that although not all tuples have been read,
   * no further tuples are required and resources can be released
   */
  override def close(): Unit = {
    base.close()
    if (currentIteration != null) {
      currentIteration.close()
    }
  }

}
