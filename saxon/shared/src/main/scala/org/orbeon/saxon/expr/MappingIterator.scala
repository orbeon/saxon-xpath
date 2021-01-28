package org.orbeon.saxon.expr

import org.orbeon.saxon.om.{Item, SequenceIterator}


class MappingIterator(
  private var base  : SequenceIterator,
  private var action: MappingFunction
)
  extends SequenceIterator {

  private var results: SequenceIterator = null

  def next(): Item = {
    var nextItem: Item = null
    var exitLoop = false
    while (! exitLoop) {
      if (results != null) {
        nextItem = results.next()
        if (nextItem != null)
          exitLoop = true
        else
          results = null
      }
      if (! exitLoop) {
        val nextSource = base.next()
        if (nextSource != null) {
          val obj = action.map(nextSource)
          if (obj != null) {
            results = obj
            nextItem = results.next()
            if (nextItem == null)
              results = null
            else
              exitLoop = true
          }
        } else {
          results = null
          return null
        }
      }
    }
    nextItem
  }

  override def close(): Unit = {
    if (results != null)
      results.close()
    base.close()
  }
}
