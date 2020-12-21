package org.orbeon.saxon.expr

import org.orbeon.saxon.om.SequenceIterator.Property._
import org.orbeon.saxon.om.{Item, SequenceIterator}
import org.orbeon.saxon.tree.iter.LookaheadIterator

import scala.beans.BooleanBeanProperty


class ItemMappingIterator(var base: SequenceIterator,
                          var action: ItemMappingFunction)
  extends SequenceIterator
    with LookaheadIterator
    with LastPositionFinder {

  @BooleanBeanProperty
  var oneToOne: Boolean = false

  def this(base: SequenceIterator,
           action: ItemMappingFunction,
           oneToOne: Boolean) = {
    this(base, action)
    this.base = base
    this.action = action
    this.oneToOne = oneToOne
  }

  def getBaseIterator: SequenceIterator = base
  def getMappingFunction: ItemMappingFunction = action

  def hasNext: Boolean = // is a lookahead iterator and one-to-one is true
    base.asInstanceOf[LookaheadIterator].hasNext

  def next(): Item = {
    while (true) {
      val nextSource = base.next()
      if (nextSource == null)
        return null
      // Call the supplied mapping function
      val current = action.mapItem(nextSource)
      if (current != null)
        return current
    }
    null
  }

  override def close(): Unit =
    base.close()

  def getLength: Int = // is a last-position-finder iterator and one-to-one is true
    base.asInstanceOf[LastPositionFinder].getLength

  override def getProperties: Set[Property] = {
    val propSet: Set[Property] = Set.empty
    val resultsSet: Set[Property] = Set(LAST_POSITION_FINDER, LOOKAHEAD)
    if (oneToOne)
      base.getProperties.intersect(resultsSet)
    else
      propSet
  }
}