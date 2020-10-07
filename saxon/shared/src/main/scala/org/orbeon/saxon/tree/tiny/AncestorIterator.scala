package org.orbeon.saxon.tree.tiny

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.tree.iter.AxisIterator

import java.util.function.Predicate


class AncestorIterator(private var current: NodeInfo,
                       private var test: Predicate[_ >: NodeInfo])
  extends AxisIterator {

  def next(): NodeInfo = {
    if (current == null) {
      return null
    }
    var node: NodeInfo = current.getParent
    while (node != null && !test.test(node)) node = node.getParent
    current = node
    return current
  }

}