////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.expr.parser.ContextItemStaticInfo

import org.orbeon.saxon.expr.parser.ExpressionVisitor

import org.orbeon.saxon.expr.sort.AtomicComparer

import org.orbeon.saxon.model.AtomicType

import org.orbeon.saxon.model.BuiltInAtomicType

import org.orbeon.saxon.model.ItemType

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.om.SequenceTool

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.AtomicValue

import org.orbeon.saxon.value.Int64Value

import org.orbeon.saxon.value.IntegerValue

import IndexOf._


object IndexOf {

  private class IndexIterator(private var base: SequenceIterator,
                              private var searchType: BuiltInAtomicType,
                              private var key: AtomicValue,
                              private var comparer: AtomicComparer)
    extends SequenceIterator {

    private var index: Int = 0

    /**
     * Close the iterator. This indicates to the supplier of the data that the client
     * does not require any more items to be delivered by the iterator. This may enable the
     * supplier to release resources. After calling close(), no further calls on the
     * iterator should be made; if further calls are made, the effect of such calls is undefined.
     * <p>(Currently, closing an iterator is important only when the data is being "pushed" in
     * another thread. Closing the iterator terminates that thread and means that it needs to do
     * no additional work. Indeed, failing to close the iterator may cause the push thread to hang
     * waiting for the buffer to be emptied.)</p>
     *
     * @since 9.1
     */
    override def close(): Unit = {
      base.close()
    }

    /**
     * Get the next item in the sequence. This method changes the state of the
     * iterator, in particular it affects the result of subsequent calls of
     * position and current.
     *
     * @return the next item, or null if there are no more items. Once a call
     *         on next() has returned null, no further calls should be made. The preferred
     *         action for an iterator if subsequent calls on next() are made is to return
     *         null again, and all implementations within Saxon follow this rule.
     * @throws org.orbeon.saxon.trans.XPathException if an error occurs retrieving the next item
     * @since 8.4
     */
    def next(): Int64Value = {
      var baseItem: AtomicValue = null
      while (({
        baseItem = base.next().asInstanceOf[AtomicValue]
        baseItem
      }) != null) {
        index += 1
        if (Type.isGuaranteedComparable(searchType,
          baseItem.getPrimitiveType,
          ordered = false) &&
          comparer.comparesEqual(baseItem, key)) {
          return new Int64Value(index)
        }
      }
      null
    }

  }

}

class IndexOf extends CollatingFunctionFixed {

  /**
   * For an expression that returns an integer or a sequence of integers, get
   * a lower and upper bound on the values of the integers that may be returned, from
   * static analysis. The default implementation returns null, meaning "unknown" or
   * "not applicable". Other implementations return an array of two IntegerValue objects,
   * representing the lower and upper bounds respectively. The values
   * UNBOUNDED_LOWER and UNBOUNDED_UPPER are used by convention to indicate that
   * the value may be arbitrarily large. The values MAX_STRING_LENGTH and MAX_SEQUENCE_LENGTH
   * are used to indicate values limited by the size of a string or the size of a sequence.
   *
   * @return the lower and upper bounds of integer values in the result, or null to indicate
   *         unknown or not applicable.
   */
  override def getIntegerBounds(): Array[IntegerValue] =
    Array(Int64Value.PLUS_ONE, Expression.MAX_SEQUENCE_LENGTH)

  override def supplyTypeInformation(visitor: ExpressionVisitor,
                                     contextItemType: ContextItemStaticInfo,
                                     arguments: Array[Expression]): Unit = {
    val type0: ItemType = arguments(0).getItemType
    val type1: ItemType = arguments(1).getItemType
    if (type0.isInstanceOf[AtomicType] && type1.isInstanceOf[AtomicType]) {
      preAllocateComparer(type0.asInstanceOf[AtomicType],
        type1.asInstanceOf[AtomicType],
        visitor.getStaticContext)
    }
  }

  /**
   * Evaluate the expression
   *
   * @param context   the dynamic evaluation context
   * @param arguments the values of the arguments, supplied as SequenceIterators
   * @return the result of the evaluation, in the form of a SequenceIterator
   * @throws org.orbeon.saxon.trans.XPathException
   * if a dynamic error occurs during the evaluation of the expression
   */
  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    val comparer: AtomicComparer = getAtomicComparer(context)
    val seq: SequenceIterator = arguments(0).iterate()
    val `val`: AtomicValue = arguments(1).head.asInstanceOf[AtomicValue]
    val searchType: BuiltInAtomicType = `val`.getPrimitiveType
    SequenceTool.toLazySequence(
      new IndexIterator(seq, searchType, `val`, comparer))
  }

  override def getStreamerName: String = "IndexOf"

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * The XPath 2.0 index-of() function, with the collation already known
 */
