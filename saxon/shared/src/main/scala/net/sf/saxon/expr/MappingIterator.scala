package net.sf.saxon.expr

import net.sf.saxon.om.Item

import net.sf.saxon.om.SequenceIterator
import scala.util.control.Breaks._

class MappingIterator(private var base: SequenceIterator,
                      private var action: MappingFunction)
  extends SequenceIterator {

  private var results: SequenceIterator = null

  def next(): Item = {
    var nextItem: Item = null
    breakable {
      while (true) {
        if (results != null) {
          nextItem = results.next()
          if (nextItem != null) {
            break
          } else {
            results = null
          }
        }
        val nextSource: Item = base.next()
        if (nextSource != null) {
          val obj: SequenceIterator = action.map(nextSource)
          if (obj != null) {
            results = obj
            nextItem = results.next()
            if (nextItem == null) {
              results = null
            } else {
              break
            }
          }
        } else {
          results = null
          null
        }
      }
    }
    nextItem
  }

  override def close(): Unit = {
    if (results != null) {
      results.close()
    }
    base.close()
  }

}
