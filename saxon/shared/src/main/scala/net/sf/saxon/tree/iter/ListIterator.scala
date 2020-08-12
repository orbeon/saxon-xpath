package net.sf.saxon.tree.iter

import net.sf.saxon.expr.LastPositionFinder
import net.sf.saxon.om.GroundedValue
import net.sf.saxon.om.Item
import net.sf.saxon.om.NodeInfo
import net.sf.saxon.om.SequenceIterator
import net.sf.saxon.value.AtomicValue
import net.sf.saxon.value.SequenceExtent
import java.util.EnumSet
import java.util.List

import net.sf.saxon.om.SequenceIterator.Property
import net.sf.saxon.om.SequenceIterator.Property.Property


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

class ListIterator[T <: Item]( var list: List[T])
  extends UnfailingIterator
    with LastPositionFinder
    with LookaheadIterator
    with GroundedIterator
    with ReversibleIterator {

  private var index: Int = 0

  def hasNext(): Boolean = index < list.size

  def next(): T = {
    if (index >= list.size) {
      null
    }
    list.get({ index += 1; index - 1 })
  }

  def getLength(): Int = list.size

  override def getProperties(): Set[Property] =
    Set(Property.LOOKAHEAD,
      Property.GROUNDED,
      Property.LAST_POSITION_FINDER)

  override def materialize(): GroundedValue = SequenceExtent.makeSequenceExtent(list)

  override def getResidue(): GroundedValue = {
    var l2: List[T] = list
    if (index != 0) {
      l2 = l2.subList(index, l2.size)
    }
    SequenceExtent.makeSequenceExtent(l2)
  }

  def getReverseIterator(): SequenceIterator = new ReverseListIterator(list)

}