package net.sf.saxon.expr.flwor

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.Item

import net.sf.saxon.om.SequenceIterator

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
            break
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
            break
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
