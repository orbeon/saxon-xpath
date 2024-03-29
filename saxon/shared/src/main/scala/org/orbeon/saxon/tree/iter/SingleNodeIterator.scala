package org.orbeon.saxon.tree.iter

import org.orbeon.saxon.expr.LastPositionFinder
import org.orbeon.saxon.om._
import org.orbeon.saxon.value.EmptySequence
import java.util.EnumSet

import org.orbeon.saxon.om.SequenceIterator.Property
import org.orbeon.saxon.om.SequenceIterator.Property.Property


object SingleNodeIterator {

  def makeIterator(item: NodeInfo): AxisIterator =
    if (item == null) {
      EmptyIterator.ofNodes
    } else {
      new SingleNodeIterator(item)
    }

}

class SingleNodeIterator private (value: NodeInfo)
  extends AxisIterator
    with ReversibleIterator
    with LastPositionFinder
    with GroundedIterator
    with LookaheadIterator {

  private val item: NodeInfo = value

  private var position: Int = 0

  def hasNext: Boolean = position == 0

  def next(): NodeInfo =
    if (position == 0) {
      position = 1
      item
    } else if (position == 1) {
      position = -1
      null
    } else {
      null
    }

  def getLength: Int = 1

  def getReverseIterator: SequenceIterator = new SingleNodeIterator(item)

  def getValue: NodeInfo = item

  override def materialize: GroundedValue = new ZeroOrOne(item)

  override def getResidue: GroundedValue =
    if (item == null) EmptySequence.getInstance else new One(item)

  override def getProperties: Set[Property] =
    Set(Property.LOOKAHEAD,
      Property.LAST_POSITION_FINDER,
      Property.GROUNDED)

}
