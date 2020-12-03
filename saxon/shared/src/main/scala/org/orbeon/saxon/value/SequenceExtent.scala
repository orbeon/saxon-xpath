package org.orbeon.saxon.value

import org.orbeon.saxon.expr.parser.ExpressionTool
import org.orbeon.saxon.expr.{LastPositionFinder, StaticProperty}
import org.orbeon.saxon.om._
import org.orbeon.saxon.tree.iter.{GroundedIterator, ListIterator, ReverseListIterator, UnfailingIterator}
import org.orbeon.saxon.tree.util.FastStringBuffer

import java.{util => ju}


object SequenceExtent {

  def makeSequenceExtent(iter: SequenceIterator): GroundedValue = iter.materialize

  def fromIterator(iter: SequenceIterator): GroundedValue =
    new SequenceExtent(iter).reduce()

  def makeResidue(iter: SequenceIterator): GroundedValue = {
    if (iter.getProperties.contains(SequenceIterator.Property.GROUNDED))
      return iter.asInstanceOf[GroundedIterator].getResidue
    val extent = new SequenceExtent(iter)
    extent.reduce()
  }

  def makeSequenceExtent[T <: Item](input: ju.List[T]): GroundedValue = {
    val len = input.size
    if (len == 0)
      EmptySequence.getInstance
    else if (len == 1)
      input.get(0)
    else
      new SequenceExtent(new ju.ArrayList(input))
  }
}

class SequenceExtent extends GroundedValue {

  var value: ju.List[_ <: Item] = _

  def this(items: Array[Item]) = {
    this()
    value = ju.Arrays.asList(items: _*)
  }
  def this(list: ju.List[_ <: Item]) = {
    this()
    this.value = list
  }

  def this(iter: SequenceIterator) = {
    this()
    val len  =
      if (!iter.getProperties.contains(
        SequenceIterator.Property.LAST_POSITION_FINDER)) 20
      else
        iter.asInstanceOf[LastPositionFinder].getLength
    val list = new ju.ArrayList[Item](len)
    iter.forEachOrFail(res => list.add(res))
    value = list
  }

  def getStringValue: String = SequenceTool.getStringValue(this)

  def getStringValueCS: CharSequence = SequenceTool.getStringValue(this)

  def head: Item = itemAt(0)

  def getLength: Int = value.size

  def getCardinality: Int = value.size match {
    case 0 => StaticProperty.EMPTY
    case 1 => StaticProperty.EXACTLY_ONE
    case _ => StaticProperty.ALLOWS_ONE_OR_MORE
  }

  def itemAt(n: Int): Item =
    if (n < 0 || n >= getLength)
      null
    else
      value.get(n)

  def iterate(): ListIterator[_ <: Item] = new ListIterator(value)

  def reverseIterate(): UnfailingIterator = new ReverseListIterator(value)

  override def effectiveBooleanValue: Boolean = {
    val len = getLength
    if (len == 0) {
      false
    } else {
      val first = value.get(0)
      if (first.isInstanceOf[NodeInfo]) {
        true
      } else if (len == 1 && first.isInstanceOf[AtomicValue]) {
        first.effectiveBooleanValue
      } else {
        ExpressionTool.effectiveBooleanValue(iterate())
      }
    }
  }

  def subsequence(start: Int, length: Int): GroundedValue = {
    var startInt = start
    if (startInt < 0) startInt = 0
    if (startInt > value.size)
      EmptySequence.getInstance
    else
      new SequenceSlice(value, startInt, length).reduce()
  }

  override def toString: String = {
    val fsb = new FastStringBuffer(FastStringBuffer.C64)
    for (i <- 0 until value.size) {
      fsb.append(if (i == 0) "(" else ", ")
      fsb.append(value.get(i).toString)
    }
    fsb.cat(')')
    fsb.toString
  }

  override def reduce(): GroundedValue = {
    val len = getLength
    if (len == 0)
      EmptySequence.getInstance
    else if (len == 1)
      itemAt(0)
    else
      this
  }

  override def asIterable(): java.lang.Iterable[_ <: Item] = value

  def iterator: ju.Iterator[_ <: Item] = value.iterator
}
