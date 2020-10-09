package org.orbeon.saxon.expr.sort

import java.util.{ArrayList, List}

import org.orbeon.saxon.expr.LastPositionFinder
import org.orbeon.saxon.ma.arrays.ArraySort
import org.orbeon.saxon.om.{NodeInfo, SequenceIterator}
import org.orbeon.saxon.trans.{Err, XPathException}
import org.orbeon.saxon.tree.iter.ListIterator

class DocumentOrderIterator(base: SequenceIterator,
                            private var comparer: ItemOrderComparer)
  extends SequenceIterator {

  private val len =
    if (base.getProperties.contains(SequenceIterator.Property.LAST_POSITION_FINDER))
      base.asInstanceOf[LastPositionFinder].getLength
    else
      50
  private val sequence: List[NodeInfo] = new ArrayList(len)
  private val iterator: SequenceIterator = new ListIterator(sequence)

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
    ArraySort.sortList(sequence)(comparer)

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
