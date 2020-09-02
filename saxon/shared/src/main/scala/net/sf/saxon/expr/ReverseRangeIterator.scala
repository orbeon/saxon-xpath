package net.sf.saxon.expr

import net.sf.saxon.trans.XPathException
import net.sf.saxon.tree.iter.AtomicIterator
import net.sf.saxon.tree.iter.LookaheadIterator
import net.sf.saxon.tree.iter.ReversibleIterator
import net.sf.saxon.value.{AtomicValue, Int64Value, IntegerValue}
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

  import net.sf.saxon.om.SequenceIterator.Property._

  override def getProperties: Set[Property] =
    Set(LOOKAHEAD, LAST_POSITION_FINDER)

  def getReverseIterator: AtomicIterator[_ <: AtomicValue] = new RangeIterator(limit, start)

}
