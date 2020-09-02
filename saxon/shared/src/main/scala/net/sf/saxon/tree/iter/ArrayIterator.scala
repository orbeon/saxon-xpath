package net.sf.saxon.tree.iter

import net.sf.saxon.expr.LastPositionFinder
import net.sf.saxon.om.GroundedValue
import net.sf.saxon.om.Item
import net.sf.saxon.om.NodeInfo
import net.sf.saxon.om.SequenceIterator
import net.sf.saxon.value.SequenceExtent
import java.util.Arrays
import java.util.EnumSet
import java.util.List

import net.sf.saxon.om.SequenceIterator.Property
import net.sf.saxon.om.SequenceIterator.Property.Property

object ArrayIterator {

  class OfNodes(list: Array[NodeInfo])
    extends ArrayIterator[NodeInfo](list)
      with AxisIterator

}

class ArrayIterator[T <: Item]( var items: Array[T])
  extends UnfailingIterator
    with LastPositionFinder
    with LookaheadIterator
    with GroundedIterator {

  private var index: Int = 0

   var start: Int = 0

   var end: Int = items.length

  def this(items: Array[T], start: Int, end: Int) = {
    this(items)
    this.items = items
    this.end = end
    this.start = start
    index = start
  }

  def makeSliceIterator(min: Int, max: Int): SequenceIterator = {
    val items: Array[T] = getArray
    val currentStart: Int = getStartPosition
    val currentEnd: Int = getEndPosition
    var lMin = min
    if (lMin < 1) {
      lMin = 1
    }
    var newStart: Int = currentStart + (lMin - 1)
    if (newStart < currentStart) {
      newStart = currentStart
    }
    var newEnd: Int =
      if (max == java.lang.Integer.MAX_VALUE) currentEnd
      else newStart + max - lMin + 1
    if (newEnd > currentEnd) {
      newEnd = currentEnd
    }
    if (newEnd <= newStart) {
      EmptyIterator.emptyIterator
    }
    new ArrayIterator(items, newStart, newEnd)
  }

  def hasNext: Boolean = index < end

  def next(): T = {
    if (index >= end) {
      index = end + 1
      return null.asInstanceOf[T]
    }
    items({ index += 1; index - 1 })
  }

  def getLength: Int = end - start

  def getArray: Array[T] = items

  def getStartPosition: Int = start

  def getEndPosition: Int = end

  override def materialize(): GroundedValue = {
    var seq: SequenceExtent = null
    if (start == 0 && end == items.length) {
      seq = new SequenceExtent(items.asInstanceOf[Array[Item]])
    } else {
      val sublist: List[T] = Arrays.asList(items: _*).subList(start, end)
      seq = new SequenceExtent(sublist)
    }
    seq.reduce()
  }

  override def getResidue: GroundedValue = {
    var seq: SequenceExtent = null
    if (start == 0 && index == 0 && end == items.length) {
      seq = new SequenceExtent(items.asInstanceOf[Array[Item]])
    } else {
      val sublist: List[T] =
        Arrays.asList(items: _*).subList(start + index, end)
      seq = new SequenceExtent(sublist)
    }
    seq.reduce()
  }

  override def getProperties: Set[Property] =
    Set(Property.GROUNDED,
      Property.LAST_POSITION_FINDER,
      Property.LOOKAHEAD)

}