package net.sf.saxon.tree.iter

import net.sf.saxon.expr.LastPositionFinder
import net.sf.saxon.om.Item
import net.sf.saxon.om.SequenceIterator
import java.util.EnumSet
import java.util.List

import net.sf.saxon.om.SequenceIterator.Property
import net.sf.saxon.om.SequenceIterator.Property.Property


class ReverseListIterator[T <: Item](private val items: List[T])
  extends UnfailingIterator
    with ReversibleIterator
    with LookaheadIterator
    with LastPositionFinder {

  private var index: Int = items.size - 1

  def hasNext(): Boolean = index >= 0

  def next(): T =
    if (index >= 0) {
      return items.get({ index -= 1; index + 1 })
    } else {
      return null.asInstanceOf[T]
    }

  def getLength(): Int = items.size

  override def getProperties(): Set[Property] =
    Set(Property.LAST_POSITION_FINDER)

  def getReverseIterator(): SequenceIterator = new ListIterator(items)

}