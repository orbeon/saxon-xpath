////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.instruct

import org.orbeon.saxon.expr.{Operand, XPathContext}
import org.orbeon.saxon.om.{Item, SequenceIterator}


/**
 * Iterate over the instructions in a sequence of instructions (or an XPath comma expression),
 * concatenating the result of each instruction into a single combined sequence.
 */
class BlockIterator(private var operanda: Array[Operand],
                    private var context: XPathContext)
  extends SequenceIterator {

  private var currentChildExpr = 0
  private var currentIter: SequenceIterator = null
  private var position = 0

  def next(): Item = {

    if (position < 0)
      return null

    while (true) {
      if (currentIter == null) {
        currentIter = operanda(currentChildExpr).getChildExpression.iterate(context)
        currentChildExpr += 1
      }
      val current = currentIter.next()
      if (current != null) {
        position += 1
        return current
      }
      currentIter = null
      if (currentChildExpr >= operanda.length) {
        position = -1
        return null
      }
    }
    null
  }

  override def close(): Unit =
    if (currentIter != null)
      currentIter.close()
}
