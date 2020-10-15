package org.orbeon.saxon.tree.iter

import org.orbeon.saxon.expr.LastPositionFinder
import org.orbeon.saxon.om.GroundedValue
import org.orbeon.saxon.om.Item
import org.orbeon.saxon.om.NodeInfo
import org.orbeon.saxon.om.SequenceIterator
import org.orbeon.saxon.value.AtomicValue
import org.orbeon.saxon.value.SequenceExtent
import java.util.EnumSet
import java.util.List

import org.orbeon.saxon.om.SequenceIterator.Property
import org.orbeon.saxon.om.SequenceIterator.Property.Property


object ListIterator {

  class Atomic(list: List[AtomicValue])
    extends ListIterator[AtomicValue](list)
      with AtomicIterator[AtomicValue]

  class OfNodes(list: List[NodeInfo])
    extends ListIterator[NodeInfo](list)
      with AxisIterator {

    override def next(): NodeInfo = super.next()
  }
}

class ListIterator[T <: Item](var list: List[T])
  extends UnfailingIterator
    with LastPositionFinder
    with LookaheadIterator
    with GroundedIterator
    with ReversibleIterator {

  private var index: Int = 0

  def hasNext: Boolean = index < list.size

  def next(): T = {
    if (index >= list.size) {
      return null.asInstanceOf[T]
    }
    list.get({
      index += 1; index - 1
    })
  }

  def getLength: Int = list.size

  override def getProperties: Set[Property] =
    Set(Property.LOOKAHEAD,
      Property.GROUNDED,
      Property.LAST_POSITION_FINDER)

  override def materialize: GroundedValue = SequenceExtent.makeSequenceExtent(list)

  override def getResidue: GroundedValue = {
    var l2 = list
    if (index != 0)
      l2 = l2.subList(index, l2.size)
    SequenceExtent.makeSequenceExtent(l2)
  }

  def getReverseIterator: SequenceIterator = new ReverseListIterator(list)
}