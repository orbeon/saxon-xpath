package net.sf.saxon.tree.linked

import net.sf.saxon.om.NodeInfo

import java.util.function.Predicate


class PrecedingEnumeration(node: NodeImpl, nodeTest: Predicate[_ >: NodeInfo])
  extends TreeEnumeration(node, nodeTest) {

  var nextAncestor: NodeImpl = node.getParent.asInstanceOf[NodeImpl]

  advance()

  override  def conforms(node: NodeImpl): Boolean = {
    if (node != null) {
      if (node == nextAncestor) {
        nextAncestor = nextAncestor.getParent
        false
      }
    }
    super.conforms(node)
  }

   def step(): Unit = {
    nextImpl = nextImpl.getPreviousInDocument
  }

}