////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A value that is a sequence containing zero or one items.
  */

package org.orbeon.saxon.om

import org.orbeon.saxon.expr.parser.ExpressionTool
import org.orbeon.saxon.tree.iter.ConstrainedIterator
import org.orbeon.saxon.value.EmptySequence


object ZeroOrOne {
  private val EMPTY: ZeroOrOne[Item] = new ZeroOrOne[Item]
  def empty[T <: Item]: ZeroOrOne[T] = EMPTY.asInstanceOf[ZeroOrOne[T]]
}

class ZeroOrOne[T <: Item] extends GroundedValue {

  private var item: T = _

  def this(item: T)={
    this()
    this.item =item
  }

  def getStringValueCS: CharSequence =
    if (item == null) "" else item.getStringValueCS

  /*@NotNull*/

  def getStringValue: String = if (item == null) "" else item.getStringValue

  /**
    * Get the first item in the sequence.
    *
    * @return the first item in the sequence if there is one, or null if the sequence
    *         is empty
    */
  def head: T = item

  def getLength: Int = if (item == null) 0 else 1

  /*@Nullable*/

  def itemAt(n: Int): T =
    if (n == 0 && item != null) {
      item
    } else {
      null.asInstanceOf[T]
    }

  /*@NotNull*/

  def subsequence(start: Int, length: Int): GroundedValue =
    if (item != null && start <= 0 && start + length > 0)
      this
    else
      EmptySequence.getInstance

  /**
    * Return an iterator over this value.
    */
  override def iterate(): ConstrainedIterator[T] =
    new ConstrainedIterator[T]() {
      var gone: Boolean = false

      def hasNext: Boolean = item != null && !gone

      def next(): T =
        if (gone) {
          null.asInstanceOf[T]
        } else {
          gone = true
          item
        }

      def getLength: Int = if (item == null) 0 else 1

      override def materialize(): GroundedValue =
        if (item == null) EmptySequence.getInstance else item

      def getResidue: GroundedValue =
        if (gone) EmptySequence.getInstance else item

      def getReverseIterator: SequenceIterator = iterate()
    }

  override def effectiveBooleanValue(): Boolean =
    ExpressionTool.effectiveBooleanValue(item)

  /**
    * Returns a string representation of the object (used only for diagnostics).
    *
    * @return a string representation of the object.
    */
  override def toString: String = if (item == null) "null" else item.toString

  /**
    * Reduce the sequence to its simplest form. If the value is an empty sequence, the result will be
    * EmptySequence.getInstance(). If the value is a single atomic value, the result will be an instance
    * of AtomicValue. If the value is a single item of any other kind, the result will be an instance
    * of SingletonItem. Otherwise, the result will typically be unchanged.
    *
    * @return the simplified sequence
    */
  override def reduce(): GroundedValue =
    if (item == null)
      EmptySequence.getInstance
    else
      item
}

