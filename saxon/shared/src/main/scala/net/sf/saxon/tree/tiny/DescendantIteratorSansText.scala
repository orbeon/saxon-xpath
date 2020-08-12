package net.sf.saxon.tree.tiny

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.pattern.NodeTest

import net.sf.saxon.tree.iter.AxisIterator

import java.util.function.IntPredicate


class DescendantIteratorSansText(private val doc: TinyTree,
                                 node: TinyNodeImpl,
                                 nodeTest: NodeTest)
  extends AxisIterator {
  private var tree: TinyTree = doc

  private var nextNodeNr: Int = node.nodeNr

  private val startDepth: Int = doc.depth(nextNodeNr)

  private val matcher: IntPredicate = nodeTest.getMatcher(doc)

  def next(): NodeInfo = {
    do {
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
    } while (!matcher.test(nextNodeNr));
    tree.getNode(nextNodeNr)
  }

}