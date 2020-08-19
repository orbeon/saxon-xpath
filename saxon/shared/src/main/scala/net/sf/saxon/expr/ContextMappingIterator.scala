////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr

import net.sf.saxon.om.FocusIterator

import net.sf.saxon.om.Item

import net.sf.saxon.om.SequenceIterator

import scala.util.control.Breaks._

class ContextMappingIterator(private var action: ContextMappingFunction,
                             private var context: XPathContext)
  extends SequenceIterator {

  private var base: FocusIterator = context.getCurrentIterator

  private var stepIterator: SequenceIterator = null

  def next(): Item = {
    var nextItem: Item = null
    breakable {
      while (true) {
        if (stepIterator != null) {
          nextItem = stepIterator.next()
          if (nextItem != null) {
            break
          } else {
            stepIterator = null
          }
        }
        if (base.next() != null) {
          // Call the supplied mapping function
          stepIterator = action.map(context)
          nextItem = stepIterator.next()
          if (nextItem == null) {
            stepIterator = null
          } else {
            break
          }
        } else {
          stepIterator = null
          null
        }
      }
    }
    nextItem
  }

  override def close(): Unit = {
    base.close()
    if (stepIterator != null) {
      stepIterator.close()
    }
  }

}
