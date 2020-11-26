package org.orbeon.saxon.tree.iter

import org.orbeon.saxon.expr.LastPositionFinder
import org.orbeon.saxon.om.{GroundedValue, Item, SequenceIterator}
import org.orbeon.saxon.om.SequenceIterator.Property._
import org.orbeon.saxon.value.EmptySequence


object SingletonIterator {

  def makeIterator[T <: Item](item: T): UnfailingIterator =
    if (item == null)
      EmptyIterator.emptyIterator
    else
      new SingletonIterator(item)

  def rawIterator[T <: Item](item: T): SingletonIterator[T] = {
    require(item ne null)
    new SingletonIterator(item)
  }
}

class SingletonIterator[T <: Item](value: T)
  extends SequenceIterator
    with UnfailingIterator
    with ReversibleIterator
    with LastPositionFinder
    with GroundedIterator
    with LookaheadIterator {

  private val item: T = value

  var gone: Boolean = false

  def hasNext: Boolean = !gone

  def next(): T =
    if (gone) {
      null.asInstanceOf[T]
    } else {
      gone = true
      item
    }

  def getLength: Int = 1

  def getReverseIterator: SingletonIterator[T] = new SingletonIterator(item)

  def getValue: T = item

  override def materialize: GroundedValue =
    if (item != null)
      item
    else
      EmptySequence.getInstance

  def getResidue: GroundedValue =
    if (gone) EmptySequence.getInstance else materialize

  override def getProperties: Set[Property] = Set(LOOKAHEAD, GROUNDED, LAST_POSITION_FINDER)
}
