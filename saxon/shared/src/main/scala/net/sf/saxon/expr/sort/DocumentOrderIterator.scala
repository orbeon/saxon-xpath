package net.sf.saxon.expr.sort

import net.sf.saxon.expr.LastPositionFinder

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.trans.Err

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.ListIterator

import java.util.ArrayList

import java.util.List

class DocumentOrderIterator(base: SequenceIterator,
                            private var comparer: ItemOrderComparer)
  extends SequenceIterator {

  private var iterator: SequenceIterator = new ListIterator(sequence)

  private var sequence: List[NodeInfo] = new ArrayList(len)

  private var current: NodeInfo = null

  val len: Int =
    if (base.getProperties.contains(SequenceIterator.Property.LAST_POSITION_FINDER))
      base.asInstanceOf[LastPositionFinder].getLength
    else 50

  base.forEachOrFail((item) =>
    if (item.isInstanceOf[NodeInfo]) {
      sequence.add(item.asInstanceOf[NodeInfo])
    } else {
      throw new XPathException(
        "Item in input for sorting is not a node: " + Err.depict(item),
        "XPTY0004")
    })

  if (sequence.size > 1) {
    sequence.sort(comparer)
  }

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
