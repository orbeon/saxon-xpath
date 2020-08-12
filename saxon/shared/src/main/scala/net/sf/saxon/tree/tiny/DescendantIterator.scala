package net.sf.saxon.tree.tiny

import net.sf.saxon.model.Type

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.pattern.NodeTest

import net.sf.saxon.tree.iter.AxisIterator

import java.util.function.IntPredicate


class DescendantIterator(private val doc: TinyTree,
                         node: TinyNodeImpl,
                         nodeTest: NodeTest)
  extends AxisIterator {

  private var tree: TinyTree = doc

  private var nextNodeNr: Int = node.nodeNr

  private val startDepth: Int = doc.depth(nextNodeNr)

  private val matcher: IntPredicate = nodeTest.getMatcher(doc)

  private var pending: NodeInfo = null

  def next(): NodeInfo = {
    do {
      if (pending != null) {
        val p: NodeInfo = pending
        pending = null
        p
      }
      { nextNodeNr += 1; nextNodeNr - 1 }
      try if (tree.depth(nextNodeNr) <= startDepth) {
        nextNodeNr = -1
        null
      } catch {
        case e: ArrayIndexOutOfBoundsException => {
          nextNodeNr = -1
          null
        }

      }
      if (tree.nodeKind(nextNodeNr) == Type.TEXTUAL_ELEMENT) {
        pending =
          tree.getNode(nextNodeNr).asInstanceOf[TinyTextualElement].getTextNode
      }
    } while (!matcher.test(nextNodeNr));
    tree.getNode(nextNodeNr)
  }

}
