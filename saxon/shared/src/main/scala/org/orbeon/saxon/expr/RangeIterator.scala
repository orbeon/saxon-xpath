package org.orbeon.saxon.expr

import org.orbeon.saxon.om.{GroundedValue, SequenceIterator}
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.tree.iter._
import org.orbeon.saxon.value.BigIntegerValue
import org.orbeon.saxon.value.Int64Value
import org.orbeon.saxon.value.IntegerRange
import org.orbeon.saxon.value.IntegerValue
import java.util.EnumSet

object RangeIterator {

  def makeRangeIterator(start: IntegerValue,
                        end: IntegerValue): AtomicIterator[IntegerValue] =
    if (start == null || end == null) {
      EmptyIterator.ofAtomic()
    } else {
      if (start.compareTo(end) > 0) {
        return EmptyIterator.ofAtomic()
      }
      if (start.isInstanceOf[BigIntegerValue] || end
        .isInstanceOf[BigIntegerValue]) {
        new BigRangeIterator(start.asBigInteger(), end.asBigInteger())
      } else {
        val startVal: Long = start.longValue()
        val endVal: Long = end.longValue()
        if (endVal - startVal > java.lang.Integer.MAX_VALUE) {
          throw new XPathException(
            "Saxon limit on sequence length exceeded (2^31)",
            "XPDY0130")
        }
        new RangeIterator(startVal, endVal)
      }
    }

}

class RangeIterator(var start: Long, var limit: Long)
  extends AtomicIterator[IntegerValue]
    with ReversibleIterator
    with LastPositionFinder
    with LookaheadIterator
    with GroundedIterator {

  var currentValue: Long = start - 1

  def hasNext: Boolean = currentValue < limit

  def next(): IntegerValue = {
    currentValue += 1
    if (currentValue > limit) {
      return null
    }
    Int64Value.makeIntegerValue(currentValue)
  }

  def getLength: Int = ((limit - start) + 1).toInt

  import SequenceIterator.Property._

  override def getProperties: Set[Property] =
    Set(LOOKAHEAD, LAST_POSITION_FINDER, GROUNDED)

  def getReverseIterator: AtomicIterator[IntegerValue] =
    new ReverseRangeIterator(limit, start)

  override def materialize: GroundedValue = new IntegerRange(start, limit)

  override def getResidue: GroundedValue =
    new IntegerRange(currentValue, limit)

}
