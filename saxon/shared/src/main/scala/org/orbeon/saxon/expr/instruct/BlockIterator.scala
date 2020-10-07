package org.orbeon.saxon.expr.instruct

import org.orbeon.saxon.expr.Operand

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.SequenceIterator

class BlockIterator(private var operanda: Array[Operand],
                    private var context: XPathContext)
  extends SequenceIterator {

  private var currentChildExpr: Int = 0

  private var currentIter: SequenceIterator = _

  private var position: Int = 0

  def next(): Item = {
    if (position < 0) {
      return null
    }
    while (true) {
      if (currentIter == null) {
        currentChildExpr += 1
        currentIter = operanda(currentChildExpr).getChildExpression.iterate(context)
      }
      val current: Item = currentIter.next()
      if (current != null) {
        position += 1
        return current
      }
      currentIter = null
      if (currentChildExpr >= operanda.length) {
        position = -1
        null
      }
    }
    null
  }

  override def close(): Unit = {
    if (currentIter != null) {
      currentIter.close()
    }
  }

}
