////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

package net.sf.saxon.z

import IntEmptySet._

//remove if not needed
//import scala.collection.JavaConversions._

object IntEmptySet {

  private var THE_INSTANCE: IntEmptySet = new IntEmptySet()

  def getInstance: IntEmptySet = THE_INSTANCE

}

/**
  * An immutable integer set containing no integers
  */
class IntEmptySet private () extends IntSet {

  def copy(): IntSet = this

  def mutableCopy(): IntSet = new IntHashSet()

  override def isMutable(): Boolean = false

  def clear(): Unit = {
    throw new UnsupportedOperationException("IntEmptySet is immutable")
  }

  def size(): Int = 0

  def isEmpty: Boolean = true

  def contains(value: Int): Boolean = false

  def remove(value: Int): Boolean =
    throw new UnsupportedOperationException("IntEmptySet is immutable")

  def add(value: Int): Boolean =
    throw new UnsupportedOperationException("IntEmptySet is immutable")

  def iterator: IntIterator = new IntIterator {
    def hasNext: Boolean = false

    def next(): Integer = java.lang.Integer.MIN_VALUE
  }

  override def union(other: IntSet): IntSet = other.copy()

  override def intersect(other: IntSet): IntSet = this

  override def except(other: IntSet): IntSet = this

  override def containsAll(other: IntSet): Boolean = other.isEmpty

}

