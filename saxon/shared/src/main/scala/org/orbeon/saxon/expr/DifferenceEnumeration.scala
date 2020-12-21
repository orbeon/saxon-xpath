////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.sort.ItemOrderComparer

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.trans.XPathException


class DifferenceEnumeration(private var p1: SequenceIterator,
                            private var p2: SequenceIterator,
                            private var comparer: ItemOrderComparer)
  extends SequenceIterator {

  /*@Nullable*/

  private var nextNode1: NodeInfo = next(p1)

  private var nextNode2: NodeInfo = next(p2)

  private def next(iter: SequenceIterator): NodeInfo = iter.next().asInstanceOf[NodeInfo]

  def next(): NodeInfo = {
    var results: NodeInfo = null
    while (true) {
      if (nextNode1 == null) {
        results = null
      }
      if (nextNode2 == null) {
        // second node-set is exhausted; return the next node from the first node-set
        results = deliver()
      }
      val c: Int = comparer.compare(nextNode1, nextNode2)
      if (c < 0) {
        // p1 is lower
        results = deliver()
      } else if (c > 0) {
        // p1 is higher
        nextNode2 = next(p2)
        if (nextNode2 == null) {
          results = deliver()
        }
      } else {
        // keys are equal
        nextNode2 = next(p2)
        nextNode1 = next(p1)
      }
    }
    results
  }

  /**
   * Deliver the next node from the first node-set, advancing the iterator to
   * look-ahead for the next item, and setting the current and position variables.
   *
   * @return the next node from the first node-set
   * @throws XPathException if a failure occurs reading from the input
   */
  private def deliver(): NodeInfo = {
    val current: NodeInfo = nextNode1
    nextNode1 = next(p1)
    current
  }

  override def close(): Unit = {
    p1.close()
    p2.close()
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * An enumeration representing a nodeset that is teh difference of two other NodeSets.
 * There is an "except" operator in XPath 2.0 to create such an expression.
 */
