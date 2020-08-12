package net.sf.saxon.expr.sort

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.LastPositionFinder

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.AtomicSequence

import net.sf.saxon.om.FocusIterator

import net.sf.saxon.om.Item

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.pattern.Pattern

import SequenceIterator.Property._

import net.sf.saxon.tree.iter.ListIterator

import net.sf.saxon.tree.iter.LookaheadIterator

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

  def hasNext(): Boolean = nextItem != null

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

  override def getProperties(): Set[Property] = Set(LOOKAHEAD, LAST_POSITION_FINDER)

}
