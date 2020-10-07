package org.orbeon.saxon.tree.linked

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.om.NodeInfo

import java.util.function.Predicate


class FollowingEnumeration(var node: NodeImpl, nodeTest: Predicate[_ >: NodeInfo])
  extends TreeEnumeration(node, nodeTest) {

  private var root: NodeImpl = node.getRoot.asInstanceOf[NodeImpl]

  val `type`: Int = node.getNodeKind

  if (`type` == Type.ATTRIBUTE || `type` == Type.NAMESPACE) {
    nextImpl = node.getParent.getNextInDocument(root)
  } else {
    do {
      nextImpl = node.getNextSibling
      if (next == null) {
        node = node.getParent
      }
    } while (next == null && node != null);
  }

  while (!conforms(nextImpl)) step()

   def step(): Unit = {
    nextImpl = nextImpl.getNextInDocument(root)
  }

}
