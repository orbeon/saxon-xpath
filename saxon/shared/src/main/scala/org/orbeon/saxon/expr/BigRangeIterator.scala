////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import org.orbeon.saxon.trans.UncheckedXPathException

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.iter.AtomicIterator

import org.orbeon.saxon.tree.iter.LookaheadIterator

import org.orbeon.saxon.value.IntegerValue

import java.math.BigInteger

import java.util.EnumSet
import org.orbeon.saxon.om.SequenceIterator.Property._


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

  def hasNext: Boolean = currentValue.compareTo(limit) < 0

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