////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.sort.ItemOrderComparer

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.trans.XPathException


class IntersectionEnumeration(private var e1: SequenceIterator,
                              private var e2: SequenceIterator,
                              private var comparer: ItemOrderComparer)
  extends SequenceIterator {

  /*@Nullable*/

  private var nextNode1: NodeInfo = next(e1)

  private var nextNode2: NodeInfo = next(e2)

  private def next(iter: SequenceIterator): NodeInfo =
    iter.next().asInstanceOf[NodeInfo]

  def next(): NodeInfo = {
    if (nextNode1 == null || nextNode2 == null) {
      return null
    }
    while (nextNode1 != null && nextNode2 != null) {
      val c: Int = comparer.compare(nextNode1, nextNode2)
      if (c < 0) {
        nextNode1 = next(e1)
      } else if (c > 0) {
        nextNode2 = next(e2)
      } else {
        // which is the same as nextNode1
        val current: NodeInfo = nextNode2
        nextNode2 = next(e2)
        nextNode1 = next(e1)
        return current
      }
      // keys are equal
      // keys are equal
    }
    null
  }

  // main merge loop: iterate whichever sequence has the lower value, returning when a pair
  // is found that match.
  // main merge loop: iterate whichever sequence has the lower value, returning when a pair
  // is found that match.

  override def close(): Unit = {
    e1.close()
    e2.close()
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * An enumeration representing a nodeset that is an intersection of two other NodeSets.
 * This implements the XPath 2.0 operator "intersect".
 */
