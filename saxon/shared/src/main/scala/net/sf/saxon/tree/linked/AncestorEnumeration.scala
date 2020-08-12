package net.sf.saxon.tree.linked

import net.sf.saxon.om.NodeInfo

import java.util.function.Predicate

class AncestorEnumeration(node: NodeImpl,
                          nodeTest: Predicate[_ >: NodeInfo],
                          private var includeSelf: Boolean)
  extends TreeEnumeration(node, nodeTest) {

  if (!includeSelf || !conforms(node)) {
    advance()
  }

   def step(): Unit = {
    nextImpl = nextImpl.getParent
  }

}