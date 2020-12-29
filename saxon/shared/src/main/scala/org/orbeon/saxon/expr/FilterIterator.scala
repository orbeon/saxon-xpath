////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.FilterIterator._
import org.orbeon.saxon.expr.parser.ExpressionTool
import org.orbeon.saxon.om.{FocusIterator, Item, NodeInfo, SequenceIterator}
import org.orbeon.saxon.value._


/**
 * A FilterIterator filters an input sequence using a filter expression. Note that a FilterIterator
 * is not used where the filter is a constant number (PositionFilter is used for this purpose instead),
 * so this class does no optimizations for numeric predicates.
 */
object FilterIterator {

  def testPredicateValue(
    iterator : SequenceIterator,
    position : Long,
    filter   : Expression
  ): Boolean = {
    val first = iterator.next()
    if (first == null)
      false
    else if (first.isInstanceOf[NodeInfo])
      true
    else {
      first match {
        case v: BooleanValue =>
          if (iterator.next() != null) {
            ExpressionTool.ebvError(
              "a sequence of two or more items starting with a boolean",
              filter
            )
          }
          v.getBooleanValue
        case v: StringValue      =>
          if (iterator.next() != null) {
            ExpressionTool.ebvError(
              "a sequence of two or more items starting with a string",
              filter
            )
          }
          v.getStringValueCS.length != 0
        case v: Int64Value      =>
          if (iterator.next() != null) {
            ExpressionTool.ebvError(
              "a sequence of two or more items starting with a numeric value",
              filter
            )
          }
          v.longValue == position
        case v: NumericValue =>
          if (iterator.next() != null) {
            ExpressionTool.ebvError(
              "a sequence of two or more items starting with a numeric value",
              filter
            )
          }
          v.compareTo(position) == 0
        case v: AtomicValue  =>
          ExpressionTool.ebvError(
            "a sequence starting with an atomic value of type " +
              v.getPrimitiveType.getDisplayName +
              " (" +
              v.toShortString +
              ")",
            filter
          )
          false
        case v               =>
          ExpressionTool.ebvError(
            "a sequence starting with " + v.getGenre.getDescription +
              " (" +
              v.toShortString +
              ")",
            filter
          )
          false
      }
    }
  }

  class NonNumeric(
    base    : SequenceIterator,
    filter  : Expression,
    context : XPathContext
  )
    extends FilterIterator(base, filter, context) {

    override def matches(): Boolean =
      filter.effectiveBooleanValue(filterContext)
  }

}

class FilterIterator(
  baseSeqIter : SequenceIterator,
  filter      : Expression,
  context     : XPathContext
)
  extends SequenceIterator {

  var filterContext: XPathContext = _
  var base: FocusIterator         = _

  // Main constructor
  locally {
    setSequence(baseSeqIter, context)
  }

  def setSequence(baseSeqIter: SequenceIterator, context: XPathContext): Unit = {
    this.filterContext = context.newMinorContext()
    this.base          = filterContext.trackFocus(baseSeqIter)
  }

  def next(): Item = getNextMatchingItem

  def getNextMatchingItem: Item = {
    var next: Item = null
    while ({
      next = base.next()
      next
    } != null)
      if (matches())
        return next
    null
  }

  def matches(): Boolean = {
    // This code is carefully designed to avoid reading more items from the
    // iteration of the filter expression than are absolutely essential.
    // The code is almost identical to the code in ExpressionTool#effectiveBooleanValue
    // except for the handling of a numeric result
    val iterator = filter.iterate(filterContext)
    testPredicateValue(iterator, base.position, filter)
  }

  override def close(): Unit =
    base.close()
}
