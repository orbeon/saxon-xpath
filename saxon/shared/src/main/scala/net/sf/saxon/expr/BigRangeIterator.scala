////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.expr

import net.sf.saxon.trans.UncheckedXPathException

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.AtomicIterator

import net.sf.saxon.tree.iter.LookaheadIterator

import net.sf.saxon.value.IntegerValue

import java.math.BigInteger

import java.util.EnumSet
import net.sf.saxon.om.SequenceIterator.Property._


class BigRangeIterator(var start: BigInteger, var limit: BigInteger)
  extends AtomicIterator[IntegerValue]
    with LastPositionFinder
    with LookaheadIterator {

  var currentValue: BigInteger = start.subtract(BigInteger.valueOf(1))

  if (limit
    .subtract(start)
    .compareTo(BigInteger.valueOf(java.lang.Integer.MAX_VALUE)) >
    0) {
    throw new XPathException("Saxon limit on sequence length exceeded (2^31)",
      "XPDY0130")
  }

  def hasNext(): Boolean = currentValue.compareTo(limit) < 0

  /*@Nullable*/

  def next(): IntegerValue = {
    currentValue = currentValue.add(BigInteger.valueOf(1))
    if (currentValue.compareTo(limit) > 0) {
      return null
    }
    IntegerValue.makeIntegerValue(currentValue)
  }

  def getLength: Int = {
    val len: BigInteger = limit.subtract(start).add(BigInteger.valueOf(1))
    if (len.compareTo(BigInteger.valueOf(java.lang.Integer.MAX_VALUE)) >
      0) {
      throw new UncheckedXPathException(
        new XPathException("Sequence exceeds Saxon limit (32-bit integer)"))
    }
    len.intValue()
  }

  override def getProperties: Set[Property] = {
    val enumSet: Set[Property] = Set(LOOKAHEAD, LAST_POSITION_FINDER)
    enumSet
  }

}