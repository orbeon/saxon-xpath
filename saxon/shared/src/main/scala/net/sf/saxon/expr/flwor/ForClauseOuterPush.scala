////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr.flwor

import net.sf.saxon.event.Outputter

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.Item

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.value.EmptySequence

import net.sf.saxon.value.Int64Value

/**
 * This class implements the changes to the tuple stream effected by a "for" clause in a FLWOR expression
 * where "allowing empty" is specified
 */
class ForClauseOuterPush(outputter: Outputter,
                          var destination: TuplePush,
                          var forClause: ForClause)
  extends TuplePush(outputter) {

  /*
   * Process the next tuple.
   */

  override def processTuple(context: XPathContext): Unit = {
    val iter: SequenceIterator = forClause.getSequence.iterate(context)
    var pos: Int = 0
    var next: Item = iter.next()
    if (next == null) {
      context.setLocalVariable(forClause.getRangeVariable.getLocalSlotNumber,
        EmptySequence.getInstance)
      if (forClause.getPositionVariable != null) {
        context.setLocalVariable(
          forClause.getPositionVariable.getLocalSlotNumber,
          Int64Value.ZERO)
      }
      destination.processTuple(context)
    } else {
      while (true) {
        context.setLocalVariable(forClause.getRangeVariable.getLocalSlotNumber,
          next)
        if (forClause.getPositionVariable != null) {
          context.setLocalVariable(
            forClause.getPositionVariable.getLocalSlotNumber,
            new Int64Value())
        }
        destination.processTuple(context)
        next = iter.next()
        if (next == null) {
        }
      }
    }
  }

  /*
   * Close the tuple stream
   */

  override def close(): Unit = {
    destination.close()
  }
}