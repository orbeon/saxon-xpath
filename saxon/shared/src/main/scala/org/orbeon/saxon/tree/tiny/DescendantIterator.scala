package org.orbeon.saxon.tree.tiny

import org.orbeon.saxon.model.Type
import org.orbeon.saxon.om.NodeInfo
import org.orbeon.saxon.pattern.NodeTest
import org.orbeon.saxon.tree.iter.AxisIterator

import java.util.function.IntPredicate


class DescendantIterator(
  private val doc: TinyTree,
  node           : TinyNodeImpl,
  nodeTest       : NodeTest
) extends AxisIterator {

  private val tree      : TinyTree     = doc
  private var nextNodeNr: Int          = node.nodeNr
  private val startDepth: Int          = doc.depth(nextNodeNr)
  private val matcher   : IntPredicate = nodeTest.getMatcher(doc)
  private var pending   : NodeInfo     = null

  def next(): NodeInfo = {
    do {
      if (pending != null) {
        val p: NodeInfo = pending
        pending = null
        return p
      }
      nextNodeNr += 1
      try if (tree.depth(nextNodeNr) <= startDepth) {
        nextNodeNr = -1
        return null
      } catch {
        case _: ArrayIndexOutOfBoundsException =>
          nextNodeNr = -1
          return null
      }
      if (tree.nodeKind(nextNodeNr) == Type.TEXTUAL_ELEMENT)
        pending = tree.getNode(nextNodeNr).asInstanceOf[TinyTextualElement].textNode
    } while (! matcher.test(nextNodeNr));
    tree.getNode(nextNodeNr)
  }
}
