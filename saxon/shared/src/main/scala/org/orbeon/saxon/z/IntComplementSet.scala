////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

package org.orbeon.saxon.z

import scala.beans.{BeanProperty, BooleanBeanProperty}


/**
  * An immutable integer set containing all int values except those in an excluded set
  */
class IntComplementSet extends IntSet {

  def this(exclusions: IntSet)={
    this()
    this.exclusions = exclusions
  }

  @BeanProperty
  var exclusions: IntSet = exclusions.copy()

  def copy(): IntSet = {
    new IntComplementSet(exclusions.copy())
  }

  def mutableCopy(): IntSet = copy()

  def clear(): Unit = {
    throw new UnsupportedOperationException(
      "IntComplementSet cannot be emptied")
  }

  def size(): Int = java.lang.Integer.MAX_VALUE - exclusions.size

  def isEmpty: Boolean = size == 0

  def contains(value: Int): Boolean = !exclusions.contains(value)

  def remove(value: Int): Boolean = {
    val b: Boolean = contains(value)
    if (b) {
      exclusions.add(value)
    }
    b
  }

  def add(value: Int): Boolean = {
    val b: Boolean = contains(value)
    if (!b) {
      exclusions.remove(value)
    }
    b
  }

  def iterator: IntIterator =
    throw new UnsupportedOperationException("Cannot enumerate an infinite set")

  override def union(other: IntSet): IntSet =
    new IntComplementSet(exclusions.except(other))

  override def intersect(other: IntSet): IntSet =
    if (other.isEmpty) {
      IntEmptySet.getInstance
    } else if (other == IntUniversalSet.getInstance) {
      copy()
    } else if (other.isInstanceOf[IntComplementSet]) {
      new IntComplementSet(
        exclusions.union(other.asInstanceOf[IntComplementSet].exclusions))
    } else {
      other.intersect(this)
    }

  override def except(other: IntSet): IntSet =
    new IntComplementSet(exclusions.union(other))

  override def containsAll(other: IntSet): Boolean =
    if (other.isInstanceOf[IntComplementSet]) {
      other.asInstanceOf[IntComplementSet].exclusions.containsAll(exclusions)
    } else if (other.isInstanceOf[IntUniversalSet]) {
      (!exclusions.isEmpty)
    } else {
      val ii: IntIterator = other.iterator
      while (ii.hasNext) {
        if (exclusions.contains(ii.next)) {
          return false
        }
      }
      true
    }

}

