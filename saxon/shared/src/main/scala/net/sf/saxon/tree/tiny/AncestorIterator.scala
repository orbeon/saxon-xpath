package net.sf.saxon.tree.tiny

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.tree.iter.AxisIterator

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