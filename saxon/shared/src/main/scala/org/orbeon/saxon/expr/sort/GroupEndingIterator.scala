package org.orbeon.saxon.expr.sort

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.functions.Count

import org.orbeon.saxon.pattern.Pattern

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.iter.LookaheadIterator

import java.util.ArrayList

import scala.util.control.Breaks._

class GroupEndingIterator(select: Expression,
                          endPattern: Pattern,
                          context: XPathContext)
  extends GroupMatchingIterator
    with GroupIterator
    with LookaheadIterator {

  var expSelect = select

  this.pattern = endPattern

  baseContext = context

  runningContext = context.newMinorContext()

  this.population = runningContext.trackFocus(expSelect.iterate(context))

  nextItem = population.next()

  override def getLength: Int = {
    val another: GroupEndingIterator =
      new GroupEndingIterator(expSelect, pattern, baseContext)
    Count.steppingCount(another)
  }

  def advance(): Unit = {
    currentMembers = new ArrayList(20)
    currentMembers.add(current)
    nextItem = current
    breakable {
      while (nextItem != null) if (pattern.matches(nextItem, runningContext)) {
        nextItem = population.next()
        if (nextItem != null) {
          break()
        }
      } else {
        nextItem = population.next()
        if (nextItem != null) {
          currentMembers.add(nextItem)
        }
      }
    }
  }

}
