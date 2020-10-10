////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.value

import org.orbeon.saxon.expr.RangeIterator

import org.orbeon.saxon.expr.parser.ExpressionTool

import org.orbeon.saxon.om.AtomicArray

import org.orbeon.saxon.om.AtomicSequence

import org.orbeon.saxon.om.GroundedValue

import org.orbeon.saxon.om.SequenceTool

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.tree.iter.AtomicIterator

import java.util.Iterator


class IntegerRange(var start: Long, var end: Long) extends AtomicSequence {

  if (end < start) {
    throw new IllegalArgumentException("end < start in IntegerRange")
  }

  if (end - start > java.lang.Integer.MAX_VALUE) {
    throw new IllegalArgumentException(
      "Maximum length of sequence in Saxon is " + java.lang.Integer.MAX_VALUE)
  }

  def getStart: Long = start

  def getEnd: Long = end

  /*@NotNull*/

  def iterate(): AtomicIterator[IntegerValue] = new RangeIterator(start, end)

  /*@Nullable*/

  def itemAt(n: Int): IntegerValue = {
    if (n < 0 || n > (end - start)) {
      return null
    }
    Int64Value.makeIntegerValue(start + n)
  }

  /*@NotNull*/

  def subsequence(start: Int, length: Int): GroundedValue = {
    if (length <= 0) {
      EmptySequence.getInstance
    }
    val newStart: Long = this.start + (if (start > 0) start else 0)
    var newEnd: Long = newStart + length - 1
    if (newEnd > end) {
      newEnd = end
    }
    if (newEnd >= newStart) {
      new IntegerRange(newStart, newEnd)
    } else {
      EmptySequence.getInstance
    }
  }

  def getLength: Int = (end - start + 1).toInt

  def head: IntegerValue = new Int64Value(start)

  /**
   * Get the canonical lexical representation as defined in XML Schema. This is not always the same
   * as the result of casting to a string according to the XPath rules.
   *
   * @return the canonical lexical representation if defined in XML Schema; otherwise, the result
   *         of casting to string according to the XPath 2.0 rules
   */
  def getCanonicalLexicalRepresentation(): CharSequence = getStringValueCS

  /**
   * Get a Comparable value that implements the XML Schema ordering comparison semantics for this value.
   * The default implementation is written to compare sequences of atomic values.
   * This method is overridden for AtomicValue and its subclasses.
   * <p>In the case of data types that are partially ordered, the returned Comparable extends the standard
   * semantics of the compareTo() method by returning the value {@link org.orbeon.saxon.om.SequenceTool#INDETERMINATE_ORDERING} when there
   * is no defined order relationship between two given values.</p>
   *
   * @return a Comparable that follows XML Schema comparison rules
   */
  def getSchemaComparable(): Comparable[_] =
    new AtomicArray(iterate()).getSchemaComparable

  def getStringValueCS: CharSequence = SequenceTool.getStringValue(this)

  def getStringValue: String = getStringValueCS.toString

  override def effectiveBooleanValue: Boolean =
    ExpressionTool.effectiveBooleanValue(iterate())

  /**
   * Reduce the sequence to its simplest form. If the value is an empty sequence, the result will be
   * EmptySequence.getInstance(). If the value is a single atomic value, the result will be an instance
   * of AtomicValue. If the value is a single item of any other kind, the result will be an instance
   * of SingletonItem. Otherwise, the result will typically be unchanged.
   *
   * @return the simplified sequence
   */
  override def reduce(): GroundedValue =
    if (start == end) {
      itemAt(0)
    } else {
      this
    }

  override def toString: String = "(" + start + " to " + end + ")"

  def iterator: Iterator[AtomicValue] = new Iterator[AtomicValue]() {
    var current: Long = start

    /**
     * Returns <tt>true</tt> if the iteration has more elements. (In other
     * words, returns <tt>true</tt> if <tt>next</tt> would return an element
     * rather than throwing an exception.)
     *
     * @return <tt>true</tt> if the iterator has more elements.
     */
    def hasNext: Boolean = current <= end

    /**
     * Removes from the underlying collection the last element returned by the
     * iterator (optional operation).  This method can be called only once per
     * call to <tt>next</tt>.  The behavior of an iterator is unspecified if
     * the underlying collection is modified while the iteration is in
     * progress in any way other than by calling this method.
     *
     * @throws UnsupportedOperationException if the <tt>remove</tt>
     *                                       operation is not supported by this Iterator.
     * @throws IllegalStateException         if the <tt>next</tt> method has not
     *                                       yet been called, or the <tt>remove</tt> method has already
     *                                       been called after the last call to the <tt>next</tt>
     *                                       method.
     */
    override def remove(): Unit = {
      throw new UnsupportedOperationException()
    }

    /**
     * Returns the next element in the iteration.
     *
     * @return the next element in the iteration.
     * @throws java.util.NoSuchElementException
     * iteration has no more elements.
     */
    def next(): IntegerValue = new Int64Value({
      current += 1; current - 1
    })
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * This class represents a sequence of consecutive ascending integers, for example 1 to 50.
 * The integers must be within the range of a Java long.
 */
