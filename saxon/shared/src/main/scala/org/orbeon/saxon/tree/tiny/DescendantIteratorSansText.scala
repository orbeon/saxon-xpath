package org.orbeon.saxon.tree.tiny

import org.orbeon.saxon.om.NodeInfo
import org.orbeon.saxon.pattern.NodeTest
import org.orbeon.saxon.tree.iter.AxisIterator

import java.util.function.IntPredicate


class DescendantIteratorSansText(
  private val doc: TinyTree,
  node           : TinyNodeImpl,
  nodeTest       : NodeTest
) extends AxisIterator {

  private val tree      : TinyTree     = doc
  private var nextNodeNr: Int          = node.nodeNr
  private val startDepth: Int          = doc.depth(nextNodeNr)
  private val matcher   : IntPredicate = nodeTest.getMatcher(doc)

  def next(): NodeInfo = {
    do {
      nextNodeNr += 1
      try if (tree.depth(nextNodeNr) <= startDepth) {
        nextNodeNr = -1
        return null
      } catch {
        case _: ArrayIndexOutOfBoundsException =>
          nextNodeNr = -1
          return null
      }
    } while (! matcher.test(nextNodeNr));
    tree.getNode(nextNodeNr)
  }
}