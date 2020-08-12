package net.sf.saxon.expr.instruct

import net.sf.saxon.expr.Operand

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.Item

import net.sf.saxon.om.SequenceIterator

class BlockIterator(private var operanda: Array[Operand],
                    private var context: XPathContext)
  extends SequenceIterator {

  private var currentChildExpr: Int = 0

  private var currentIter: SequenceIterator = _

  private var position: Int = 0

  def next(): Item = {
    if (position < 0) {
      null
    }
    while (true) {
      if (currentIter == null) {
        currentChildExpr += 1
        currentIter = operanda(currentChildExpr).getChildExpression.iterate(context)
      }
      val current: Item = currentIter.next()
      if (current != null) {
         position += 1
        current
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
