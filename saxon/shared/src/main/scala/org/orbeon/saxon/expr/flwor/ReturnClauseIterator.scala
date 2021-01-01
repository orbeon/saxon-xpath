package org.orbeon.saxon.expr.flwor

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.SequenceIterator

import scala.util.control.Breaks._

class ReturnClauseIterator(private var base: TuplePull,
                           flwor: FLWORExpression,
                           private var context: XPathContext)
  extends SequenceIterator {

  private var action: Expression = flwor.getReturnClause

  private var results: SequenceIterator = null

  def next(): Item = {
    var nextItem: Item = null
    breakable {
      while (true) {
        if (results != null) {
          nextItem = results.next()
          if (nextItem != null) {
            break()
          } else {
            results = null
          }
        }
        if (base.nextTuple(context)) {
          results = action.iterate(context)
          nextItem = results.next()
          if (nextItem == null) {
            results = null
          } else {
            break()
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
    if (results != null) {
      results.close()
    }
    base.close()
  }

}
