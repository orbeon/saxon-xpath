////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.parser.ExpressionTool

import org.orbeon.saxon.om.FocusIterator

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value._

import FilterIterator._


object FilterIterator {

  def testPredicateValue(iterator: SequenceIterator,
                         position: Long,
                         filter: Expression): Boolean = {
    val first: Item = iterator.next()
    if (first == null) {
      return false
    }
    if (first.isInstanceOf[NodeInfo]) {
      true
    } else {
      if (first.isInstanceOf[BooleanValue]) {
        if (iterator.next() != null) {
          ExpressionTool.ebvError(
            "a sequence of two or more items starting with a boolean",
            filter)
        }
        first.asInstanceOf[BooleanValue].getBooleanValue
      } else if (first.isInstanceOf[StringValue]) {
        if (iterator.next() != null) {
          ExpressionTool.ebvError(
            "a sequence of two or more items starting with a string",
            filter)
        }
        first.getStringValueCS.length != 0
      } else if (first.isInstanceOf[Int64Value]) {
        if (iterator.next() != null) {
          ExpressionTool.ebvError(
            "a sequence of two or more items starting with a numeric value",
            filter)
        }
        first.asInstanceOf[Int64Value].longValue() == position
      } else if (first.isInstanceOf[NumericValue]) {
        if (iterator.next() != null) {
          ExpressionTool.ebvError(
            "a sequence of two or more items starting with a numeric value",
            filter)
        }
        first.asInstanceOf[NumericValue].compareTo(position) ==
          0
      } else if (first.isInstanceOf[AtomicValue]) {
        ExpressionTool.ebvError(
          "a sequence starting with an atomic value of type " +
            first.asInstanceOf[AtomicValue].getPrimitiveType.getDisplayName +
            " (" +
            first.toShortString +
            ")",
          filter
        )
        false
      } else {
        ExpressionTool.ebvError(
          "a sequence starting with " + first.getGenre.getDescription +
            " (" +
            first.toShortString +
            ")",
          filter)
        false
      }
    }
  }

  class NonNumeric(base: SequenceIterator,
                   filter: Expression,
                   context: XPathContext)
    extends FilterIterator(base, filter, context) {

    override def matches(): Boolean =
      filter.effectiveBooleanValue(filterContext)

  }

}

class FilterIterator(base: SequenceIterator,
                     var filter: Expression,
                     context: XPathContext)
  extends SequenceIterator {

  var filterContext: XPathContext = context.newMinorContext()
  var base: FocusIterator = filterContext.trackFocus(base)

  def setSequence(base: SequenceIterator, context: XPathContext): Unit = {
    filterContext = context.newMinorContext()
    this.base = filterContext.trackFocus(base)
  }

  def next(): Item = getNextMatchingItem

  def getNextMatchingItem: Item = {
    var next: Item = null
    while (({
      next = base.next()
      next
    }) != null) if (matches()) {
      next
    }
    null
  }

  def matches(): Boolean = {
    val iterator: SequenceIterator = filter.iterate(filterContext)
    testPredicateValue(iterator, base.position, filter)
  }

  // This code is carefully designed to avoid reading more items from the
  // iteration of the filter expression than are absolutely essential.
  // The code is almost identical to the code in ExpressionTool#effectiveBooleanValue
  // except for the handling of a numeric result
  // This code is carefully designed to avoid reading more items from the
  // iteration of the filter expression than are absolutely essential.
  // The code is almost identical to the code in ExpressionTool#effectiveBooleanValue
  // except for the handling of a numeric result

  override def close(): Unit = {
    base.close()
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * A FilterIterator filters an input sequence using a filter expression. Note that a FilterIterator
 * is not used where the filter is a constant number (PositionFilter is used for this purpose instead),
 * so this class does no optimizations for numeric predicates.
 */
