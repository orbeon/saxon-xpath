package org.orbeon.saxon.tree.linked

import org.orbeon.saxon.om.NodeInfo

import java.util.function.Predicate


class PrecedingSiblingEnumeration(node: NodeImpl,
                                  nodeTest: Predicate[_ >: NodeInfo])
  extends TreeEnumeration(node, nodeTest) {

  advance()

   def step(): Unit = {
    nextImpl = nextImpl.getPreviousSibling
  }

}