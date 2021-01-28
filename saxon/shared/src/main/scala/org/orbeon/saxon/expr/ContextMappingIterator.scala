////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import org.orbeon.saxon.om.{FocusIterator, Item, SequenceIterator}

import scala.util.control.Breaks._


/**
  * ContextMappingIterator merges a sequence of sequences into a single flat
  * sequence. It takes as inputs an iteration, and a mapping function to be
  * applied to each Item returned by that iteration. The mapping function itself
  * returns another iteration. The result is an iteration of the concatenation of all
  * the iterations returned by the mapping function: often referred to as a flat-map operation.
 *
  * This is related to the `MappingIterator` class: it differs in that it
  * sets each item being processed as the context item
  */
class ContextMappingIterator(private var action: ContextMappingFunction,
                             private var context: XPathContext)
  extends SequenceIterator {

  private val base: FocusIterator = context.getCurrentIterator

  private var stepIterator: SequenceIterator = null

  def next(): Item = {
    var nextItem: Item = null
    // ORBEON: Optimize loop with `while` as the non-local return showed in the JavaScript profiler.
    var exitLoop = false
    while (! exitLoop) {
      if (stepIterator != null) {
        nextItem = stepIterator.next()
        if (nextItem != null)
          exitLoop = true
        else
          stepIterator = null
      }
      if (! exitLoop) {
        if (base.next() != null) {
          // Call the supplied mapping function
          stepIterator = action.map(context)
          nextItem = stepIterator.next()
          if (nextItem == null)
            stepIterator = null
          else
            exitLoop = true
        } else {
          stepIterator = null
          return null
        }
      }
    }
    nextItem
  }

  override def close(): Unit = {
    base.close()
    if (stepIterator != null)
      stepIterator.close()
  }
}
