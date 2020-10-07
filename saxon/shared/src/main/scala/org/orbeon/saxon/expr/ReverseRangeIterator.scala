package org.orbeon.saxon.expr

import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.iter.AtomicIterator
import org.orbeon.saxon.tree.iter.LookaheadIterator
import org.orbeon.saxon.tree.iter.ReversibleIterator
import org.orbeon.saxon.value.{AtomicValue, Int64Value, IntegerValue}
import java.util.EnumSet


class ReverseRangeIterator(var start: Long, var limit: Long)
  extends AtomicIterator[IntegerValue]
    with ReversibleIterator
    with LastPositionFinder
    with LookaheadIterator {

  var currentValue: Long = start + 1

  if (start - limit > java.lang.Integer.MAX_VALUE) {
    throw new XPathException("Saxon limit on sequence length exceeded (2^31)",
      "XPDY0130")
  }

  def hasNext: Boolean = currentValue > limit

  def next(): IntegerValue = {
    currentValue -= 1
    if (currentValue < limit) {
      return null
    }
    Int64Value.makeIntegerValue(currentValue)
  }

  def getLength: Int = ((start - limit) + 1).toInt

  import org.orbeon.saxon.om.SequenceIterator.Property._

  override def getProperties: Set[Property] =
    Set(LOOKAHEAD, LAST_POSITION_FINDER)

  def getReverseIterator: AtomicIterator[_ <: AtomicValue] = new RangeIterator(limit, start)

}
