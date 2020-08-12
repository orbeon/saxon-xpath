////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.accum

import net.sf.saxon.om.AxisInfo

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.om.Sequence

import net.sf.saxon.pattern.AnyNodeTest

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.AxisIterator

import net.sf.saxon.tree.tiny.TinyNodeImpl

import net.sf.saxon.tree.util.Navigator

import java.util.Stack




/**
  * Accumulator data for a tree that is obtained by mapping the nodes in this tree
  * to nodes in some other tree (specifically, the subtree from which this tree was
  * originally copied with copy-accumulators=yes) and getting the accumulator value
  * from the corresponding node in the other tree.
  */
class PathMappedAccumulatorData(original: IAccumulatorData,
                                private var origin: NodeInfo)
    extends IAccumulatorData {

  private var originalData: IAccumulatorData = original

  override def getAccumulator(): Accumulator = null

  override def getValue(node: NodeInfo, postDescent: Boolean): Sequence =
    originalData.getValue(map(node), postDescent)

  private def map(node: NodeInfo): NodeInfo =
    if (origin.isInstanceOf[TinyNodeImpl] && node.isInstanceOf[TinyNodeImpl]) {
// Fast algorithm where both trees are TinyTrees: map the node numbers
      val nodeNrInSubtree: Int = node.asInstanceOf[TinyNodeImpl].getNodeNumber
      origin
        .asInstanceOf[TinyNodeImpl]
        .getTree
        .getNode(
          nodeNrInSubtree + origin.asInstanceOf[TinyNodeImpl].getNodeNumber)
    } else {
// General algorithm: get the node with a corresponding path in terms of sibling position.
      val path: Stack[Integer] = new Stack[Integer]()
      var ancestor: NodeInfo = node
      while (ancestor != null) {
        path.push(
          Navigator.getSiblingPosition(ancestor,
                                       AnyNodeTest.getInstance,
                                       java.lang.Integer.MAX_VALUE))
        ancestor = ancestor.getParent
      }
      var target: NodeInfo = origin
      while (!path.isEmpty) {
        var pos: Int = path.pop()
        val kids: AxisIterator = target.iterateAxis(AxisInfo.CHILD)
        while ({ pos -= 1; pos + 1 } > 0) {
          target = kids.next()
          assert((target != null))
        }
      }
      target
    }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
