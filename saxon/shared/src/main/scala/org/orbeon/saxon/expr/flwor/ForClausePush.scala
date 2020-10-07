////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.flwor

import org.orbeon.saxon.event.Outputter

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.Int64Value




/**
  * This class implements the changes to the tuple stream effected by a "for" clause in a FLWOR expression
  */
class ForClausePush(outputter: Outputter,
                     var destination: TuplePush,
                     var forClause: ForClause)
    extends TuplePush(outputter) {

  /*
   * Process the next tuple.
   */

  override def processTuple(context: XPathContext): Unit = {
    val iter: SequenceIterator = forClause.getSequence.iterate(context)
    var pos: Int = 0
    var next: Item = null
    while (({
      next = iter.next()
      next
    }) != null) {
      context.setLocalVariable(forClause.getRangeVariable.getLocalSlotNumber,
                               next)
      if (forClause.getPositionVariable != null) {
        context.setLocalVariable(
          forClause.getPositionVariable.getLocalSlotNumber,
          new Int64Value())
      }
      destination.processTuple(context)
    }
  }

  /*
   * Close the tuple stream
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
