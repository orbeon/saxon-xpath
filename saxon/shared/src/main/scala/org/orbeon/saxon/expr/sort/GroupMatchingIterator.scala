package org.orbeon.saxon.expr.sort

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.LastPositionFinder

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.AtomicSequence

import org.orbeon.saxon.om.FocusIterator

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.pattern.Pattern

import SequenceIterator.Property._

import org.orbeon.saxon.tree.iter.ListIterator

import org.orbeon.saxon.tree.iter.LookaheadIterator

import java.util.EnumSet

import java.util.List

abstract class GroupMatchingIterator
  extends LookaheadIterator
    with LastPositionFinder
    with GroupIterator {

   var select: Expression = _

   var population: FocusIterator = _

   var pattern: Pattern = _

   var baseContext: XPathContext = _

   var runningContext: XPathContext = _

   var currentMembers: List[Item] = _

   var nextItem: Item = _

   var current: Item = null

   var position: Int = 0

   def advance(): Unit

  def getCurrentGroupingKey(): AtomicSequence = null

  def iterateCurrentGroup(): SequenceIterator =
    new ListIterator(currentMembers)

  def hasNext: Boolean = nextItem != null

  def next(): Item =
    if (nextItem != null) {
      current = nextItem
      position += 1
      advance()
      current
    } else {
      current = null
      position = -1
      null
    }

  override def close(): Unit = {
    population.close()
  }

  override def getProperties: Set[Property] = Set(LOOKAHEAD, LAST_POSITION_FINDER)

}
