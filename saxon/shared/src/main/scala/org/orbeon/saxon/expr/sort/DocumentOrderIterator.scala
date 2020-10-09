package org.orbeon.saxon.expr.sort

import org.orbeon.saxon.expr.LastPositionFinder

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.trans.Err

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.iter.ListIterator

import java.util.ArrayList

import java.util.List

class DocumentOrderIterator(base: SequenceIterator,
                            private var comparer: ItemOrderComparer)
  extends SequenceIterator {

  val len =
    if (base.getProperties.contains(SequenceIterator.Property.LAST_POSITION_FINDER))
      base.asInstanceOf[LastPositionFinder].getLength
    else 50
  private var sequence: List[NodeInfo] = new ArrayList(len)
  private var iterator: SequenceIterator = new ListIterator(sequence)

  private var current: NodeInfo = null

  base.forEachOrFail {
    case info: NodeInfo =>
      sequence.add(info)
    case item =>
      throw new XPathException(
        "Item in input for sorting is not a node: " + Err.depict(item),
        "XPTY0004")
  }

  if (sequence.size > 1)
    sequence.sort(comparer)

  def next(): NodeInfo = {
    while (true) {
      val next: NodeInfo = iterator.next().asInstanceOf[NodeInfo]
      if (next == null) {
        current = null
        return null
      }
      if (next != current) {
        current = next
        return current
      }
    }
    null
  }

}
