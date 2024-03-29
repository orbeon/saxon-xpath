package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.sort.ItemOrderComparer

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.om.SequenceIterator

import SequenceIterator.Property._

import org.orbeon.saxon.tree.iter.LookaheadIterator

import java.util.EnumSet

class UnionEnumeration(p1: SequenceIterator,
                       p2: SequenceIterator,
                       private var comparer: ItemOrderComparer)
  extends SequenceIterator
    with LookaheadIterator {

  private var e1: SequenceIterator = p1

  private var e2: SequenceIterator = p2

  private var nextNode1: NodeInfo = next(e1)

  private var nextNode2: NodeInfo = next(e2)

  private def next(iter: SequenceIterator): NodeInfo =
    iter.next().asInstanceOf[NodeInfo]

  def hasNext: Boolean = nextNode1 != null || nextNode2 != null

  def next(): NodeInfo = {
    if (nextNode1 != null && nextNode2 != null) {
      val c: Int = comparer.compare(nextNode1, nextNode2)
      if (c < 0) {
        val current: NodeInfo = nextNode1
        nextNode1 = next(e1)
        return current
      } else if (c > 0) {
        val current: NodeInfo = nextNode2
        nextNode2 = next(e2)
        return current
      } else {
        val current: NodeInfo = nextNode2
        nextNode2 = next(e2)
        nextNode1 = next(e1)
        return current
      }
    }
    if (nextNode1 != null) {
      val current: NodeInfo = nextNode1
      nextNode1 = next(e1)
      return current
    }
    if (nextNode2 != null) {
      val current: NodeInfo = nextNode2
      nextNode2 = next(e2)
      return current
    }
    null
  }

  override def close(): Unit = {
    e1.close()
    e2.close()
  }

  override def getProperties: Set[Property] = Set(LOOKAHEAD)

}
