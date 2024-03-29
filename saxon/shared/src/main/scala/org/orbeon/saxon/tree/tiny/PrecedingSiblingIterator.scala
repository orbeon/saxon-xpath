////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.tree.tiny

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.pattern.NodeTest

import org.orbeon.saxon.tree.iter.AxisIterator

import java.util.function.IntPredicate




class PrecedingSiblingIterator(private var document: TinyTree,
                               private var startNode: TinyNodeImpl,
                               private var test: NodeTest)
  extends AxisIterator {

  private var nextNodeNr: Int = startNode.nodeNr

  private var parentNode: TinyNodeImpl = startNode.parent

  private val matcher: IntPredicate = test.getMatcher(document)

  document.ensurePriorIndex()

  /*@Nullable*/

  def next(): NodeInfo = {
    if (nextNodeNr < 0) {
      // This check is needed because an errant caller can call next() again after hitting the end of sequence
      return null
    }
    while (true) {
      nextNodeNr = document.prior(nextNodeNr)
      if (nextNodeNr < 0) {
        return null
      }
      if (matcher.test(nextNodeNr)) {
        val next: TinyNodeImpl = document.getNode(nextNodeNr)
        next.setParentNode(parentNode)
        return next
      }
    }
    null
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class supports the preceding-sibling axis.
  * The starting node must be an element, text node, comment, or processing instruction:
  * to ensure this, construct the enumeration using NodeInfo#getEnumeration()
  */
