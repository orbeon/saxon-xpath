////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

package net.sf.saxon.z

import scala.beans.{BeanProperty, BooleanBeanProperty}

/**
  * Set of int values. This immutable implementation of IntSet represents a dense monotonic
  * range of integers from A to B.
  *
  * @author Michael Kay
  */
class IntBlockSet(@BeanProperty var startPoint: Int,
                  @BeanProperty var endPoint: Int)
  extends IntSet {

  // Hashcode, evaluated lazily
  private var hashCodeVariable: Int = -1

  def copy: IntSet = this

  def mutableCopy: IntSet =
    new IntRangeSet(Array(startPoint), Array(endPoint))

  override def isMutable(): Boolean = false

  def size: Int = endPoint - startPoint

  def isEmpty: Boolean = size == 0

  def contains(value: Int): Boolean = value >= startPoint && value <= endPoint

  def remove(value: Int): Boolean =
    throw new UnsupportedOperationException("remove")

  def clear(): Unit = {
    throw new UnsupportedOperationException("clear")
  }

  def add(value: Int): Boolean = throw new UnsupportedOperationException("add")

  def iterator(): IntIterator = mutableCopy().iterator()

  override def toString(): String = startPoint + " - " + endPoint

  override def equals(other: Any): Boolean = mutableCopy() == other

  override def hashCode(): Int = {
    // Note, hashcodes are NOT the same as those used by IntHashSet and IntArraySet
    if (hashCodeVariable == -1) {
      hashCodeVariable = 0x836a89f1 ^ (startPoint + (endPoint << 3))
    }
    hashCodeVariable
  }

}

