package org.orbeon.saxon.expr.sort

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.functions.Count

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.pattern.Pattern

import org.orbeon.saxon.tree.iter.LookaheadIterator

import java.util.ArrayList
import scala.util.control.Breaks._

class GroupStartingIterator(select: Expression,
                            startPattern: Pattern,
                            context: XPathContext)
  extends GroupMatchingIterator
    with LookaheadIterator
    with GroupIterator {

  var selExpression = select

  this.pattern = startPattern

  baseContext = context

  runningContext = context.newMinorContext()

  this.population = runningContext.trackFocus(selExpression.iterate(context))

  nextItem = population.next()

  override def getLength: Int = {
    val another: GroupStartingIterator =
      new GroupStartingIterator(selExpression, pattern, baseContext)
    Count.steppingCount(another)
  }

   def advance(): Unit = {
     currentMembers = new ArrayList(10)
     currentMembers.add(current)
     breakable{ while (true) {
       val nextCandidate: Item = population.next()
       if (nextCandidate == null) {
         break()
       }
       if (pattern.matches(nextCandidate, runningContext)) {
         nextItem = nextCandidate
         return
       } else {
         currentMembers.add(nextCandidate)
       }
     }
   }
    nextItem = null
  }

}
