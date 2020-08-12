package net.sf.saxon.expr.sort

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.functions.Count

import net.sf.saxon.pattern.Pattern

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.LookaheadIterator

import java.util.ArrayList

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

  override def getLength(): Int = {
    val another: GroupEndingIterator =
      new GroupEndingIterator(expSelect, pattern, baseContext)
    Count.steppingCount(another)
  }

   def advance(): Unit = {
    currentMembers = new ArrayList(20)
    currentMembers.add(current)
    nextItem = current
    while (nextItem != null) if (pattern.matches(nextItem, runningContext)) {
      nextItem = population.next()
      if (nextItem != null) {
        //break
      }
    } else {
      nextItem = population.next()
      if (nextItem != null) {
        currentMembers.add(nextItem)
      }
    }
  }

}
