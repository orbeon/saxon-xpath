package net.sf.saxon.tree.tiny

import net.sf.saxon.model.Type
import net.sf.saxon.om.NodeInfo
import net.sf.saxon.pattern.NodeTest
import net.sf.saxon.tree.iter.AxisIterator
import java.util.function.IntPredicate
import scala.util.control.Breaks._

class FollowingIterator(var doc: TinyTree,
                        var startNode: TinyNodeImpl,
                        var nodeTest: NodeTest,
                        var includeDescendants: Boolean)
  extends AxisIterator {
  var tree: TinyTree = doc

  var current: NodeInfo = _

  var position: Int = 0

  val matcher: IntPredicate = nodeTest.getMatcher(doc)

  var pending: NodeInfo = _

  var test: NodeTest = nodeTest

  def next(): NodeInfo = {
    if (pending != null) {
      val p: NodeInfo = pending
      pending = null
      return p
    }
    var nodeNr: Int = 0
    if (position <= 0) {
      if (position < 0) {
        return null
      }
      nodeNr = startNode.nodeNr
      if (includeDescendants) {
        nodeNr += 1
      } else {
        breakable {
          while (true) {
            val nextSib: Int = tree.next(nodeNr)
            if (nextSib > nodeNr) {
              nodeNr = nextSib
              break()
            } else if (tree.depth(nextSib) == 0) {
              current = null
              position = -1
              return null
            } else {
              nodeNr = nextSib
            }
          }
        }
      }
    } else {
      assert(current != null)
      var here: TinyNodeImpl = null
      here = if (current.isInstanceOf[TinyTextualElement])
        current.getParent.asInstanceOf[TinyNodeImpl]
      else current.asInstanceOf[TinyNodeImpl]
      nodeNr = here.nodeNr + 1
    }
    while (true) {
      if (tree.depth(nodeNr) == 0) {
        current = null
        position = -1
        return null
      }
      if (tree.nodeKind(nodeNr) == Type.TEXTUAL_ELEMENT) {
        val e: TinyTextualElement =
          tree.getNode(nodeNr).asInstanceOf[TinyTextualElement]
        val t: NodeInfo = e.getTextNode
        if (matcher.test(nodeNr)) {
          if (test.test(t)) {
            pending = t
          }
          position += 1
          current = tree.getNode(nodeNr)
        } else if (test.test(t)) {
          position += 1
          current = t
        }
      } else if (matcher.test(nodeNr)) {
        position += 1
        current = tree.getNode(nodeNr)
        return current
      }
      nodeNr += 1
    }
    null
  }

}