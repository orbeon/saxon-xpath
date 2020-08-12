package net.sf.saxon.tree.linked

import net.sf.saxon.om.NodeInfo

import java.util.function.Predicate

class FollowingSiblingEnumeration(node: NodeImpl,
                                  nodeTest: Predicate[_ >: NodeInfo])
  extends TreeEnumeration(node, nodeTest) {

  advance()

   def step(): Unit = {
    nextImpl = nextImpl.getNextSibling
  }

}