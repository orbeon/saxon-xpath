////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

package net.sf.saxon.om

import net.sf.saxon.tree.iter.ConstrainedIterator
import net.sf.saxon.value._


object One {
  def bool(value: Boolean): One[BooleanValue] = new One(BooleanValue.get(value))
  def string(value: String): One[StringValue] = new One(new StringValue(value))
  def integer(value: Long): One[IntegerValue] = new One(new Int64Value(value))
  def dbl(value: Double): One[DoubleValue] = new One(new DoubleValue(value))
}

/**
  * A sequence containing exactly one item. The main use of this class is in declaring the expected arguments
  * of reflexive method calls, where the use of One(T) rather than T emphasizes that the value must not be null/empty,
  * and generates type-checking code to ensure that it is not empty.
  *
  * <p>To extract the wrapped item, use {@link #head()}.</p>
  */
class One[T <: Item](item: T)  extends ZeroOrOne[T](item) {

  if (item == null) {
    throw new NullPointerException()
  }

  /**
    * Return an iterator over this value.
    */
  override def iterate(): ConstrainedIterator[T] =
    new ConstrainedIterator[T]() {
      var gone: Boolean = false

      override def hasNext: Boolean = !gone

      override def next(): T =
        if (! gone) {
          gone = true
          head
        } else {
          null.asInstanceOf[T]
        }

      override def getLength: Int = 1
      override def materialize(): GroundedValue = head
      override def getResidue: GroundedValue =
        if (gone)
          EmptySequence.getInstance else head

      override def getReverseIterator: SequenceIterator = iterate()
    }

}
