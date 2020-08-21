package net.sf.saxon.expr.sort

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.functions.Count

import net.sf.saxon.om.Item

import net.sf.saxon.pattern.Pattern

import net.sf.saxon.tree.iter.LookaheadIterator

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

  override def getLength(): Int = {
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
