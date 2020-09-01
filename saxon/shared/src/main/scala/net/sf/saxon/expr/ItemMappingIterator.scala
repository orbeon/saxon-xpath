////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr

import net.sf.saxon.om.EnumSetTool

import net.sf.saxon.om.Item

import net.sf.saxon.om.SequenceIterator

import SequenceIterator.Property._

import net.sf.saxon.tree.iter.LookaheadIterator

import java.util.EnumSet

import scala.beans.{BeanProperty, BooleanBeanProperty}


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

  def hasNext(): Boolean = // is a lookahead iterator and one-to-one is true
    base.asInstanceOf[LookaheadIterator].hasNext

  def next(): Item = {
    var results: Item = null
    while (true) {
      val nextSource: Item = base.next()
      if (nextSource == null) {
        results = null
      }
      // Call the supplied mapping function
      val current: Item = action.mapItem(nextSource)
      if (current != null) {
        results = current
      }
    }
    results
  }

  // otherwise go round the loop to get the next item from the base sequence
  // otherwise go round the loop to get the next item from the base sequence

  override def close(): Unit = {
    base.close()
  }

  def getLength: Int = // is a last-position-finder iterator and one-to-one is true
    base.asInstanceOf[LastPositionFinder].getLength

  override def getProperties: Set[Property] = {
    var propSet : Set[Property] = Set()
    val resultsSet: Set[Property] = Set(LAST_POSITION_FINDER, LOOKAHEAD)
    if (oneToOne) {
      base.getProperties.intersect(resultsSet)
    } else {
      propSet
    }
  }
}