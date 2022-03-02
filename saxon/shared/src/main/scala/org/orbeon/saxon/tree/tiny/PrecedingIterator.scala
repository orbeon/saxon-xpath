package org.orbeon.saxon.tree.tiny

import org.orbeon.saxon.model.Type
import org.orbeon.saxon.model.UType
import org.orbeon.saxon.om.NodeInfo
import org.orbeon.saxon.pattern.NodeTest
import org.orbeon.saxon.tree.iter.AxisIterator
import java.util.function.IntPredicate


class PrecedingIterator(private var doc: TinyTree,
                        private var node: TinyNodeImpl,
                        private var nodeTest: NodeTest,
                        private var includeAncestors: Boolean)
  extends AxisIterator {

  private val tree: TinyTree = doc
  private var current: TinyNodeImpl = node
  private var nextAncestorDepth: Int = doc.depth(node.nodeNr) - 1
  private val matcher: IntPredicate = nodeTest.getMatcher(doc)
  private var pending: TinyNodeImpl = null

  private val matchesTextNodes: Boolean =
    nodeTest.getUType.overlaps(UType.TEXT)

  def next(): NodeInfo = {
    if (pending != null) {
      current = pending
      pending = null
      return current
    }
    if (current == null) {
      return null
    }
    if (current.isInstanceOf[TinyTextualElement#TinyTextualElementText]) {
      current = current.getParent
    }
    var nextNodeNr: Int = current.nodeNr
    while ( {
      true
    }) {
      if (!includeAncestors) {
        {
          nextNodeNr -= 1
        }
        while (nextAncestorDepth >= 0 && tree.depth(nextNodeNr) == nextAncestorDepth) {
          if ( {
            nextAncestorDepth -= 1; nextAncestorDepth + 1
          } <= 0) {
            current = null
            return null
          }
          nextNodeNr -= 1
        }
      } else {
        if (tree.depth(nextNodeNr) == 0) {
          current = null
          return null
        } else {
          nextNodeNr -= 1
        }
      }
      if (matchesTextNodes && tree.nodeKind(nextNodeNr) == Type.TEXTUAL_ELEMENT) {
        val element: TinyTextualElement =
          tree.getNode(nextNodeNr).asInstanceOf[TinyTextualElement]
        val text: TinyTextualElement#TinyTextualElementText =
          element.textNode
        if (nodeTest.test(text)) {
          if (nodeTest.test(element)) {
            pending = element
          }
          current = text.asInstanceOf[TinyNodeImpl]
        } else if (nodeTest.test(element)) {
          current = element
        }
      } else {
        if (matcher.test(nextNodeNr)) {
          current = tree.getNode(nextNodeNr)
          return current
        }
        if (tree.depth(nextNodeNr) == 0) {
          current = null
          return null
        }
      }
    }
    null
  }

}