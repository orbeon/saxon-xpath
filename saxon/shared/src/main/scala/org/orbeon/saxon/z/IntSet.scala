////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

package org.orbeon.saxon.z


/**
 * A set of integers represented as int values
 */
trait IntSet {

  def copy(): IntSet
  def mutableCopy(): IntSet
  def isMutable: Boolean = true
  def clear(): Unit
  def size(): Int
  def isEmpty: Boolean
  def contains(value: Int): Boolean
  def remove(value: Int): Boolean
  def add(value: Int): Boolean
  def iterator: IntIterator

  def containsAll(other: IntSet): Boolean = {
    if (other == IntUniversalSet.getInstance || other.isInstanceOf[IntComplementSet])
      return false
    val it = other.iterator
    while (it.hasNext)
      if (! contains(it.next))
        return false
    true
  }

  def union(other: IntSet): IntSet = {
    if (other == IntUniversalSet.getInstance)
      return other
    if (this.isEmpty)
      return other.copy()
    if (other.isEmpty)
      return this.copy()
    if (other.isInstanceOf[IntComplementSet])
      return other.union(this)
    val n = new IntHashSet(this.size + other.size)
    var it: IntIterator = iterator
    while (it.hasNext) {
      n.add(it.next)
    }
    it = other.iterator
    while (it.hasNext) {
      n.add(it.next)
    }
    n
  }

  def intersect(other: IntSet): IntSet = {
    if (this.isEmpty || other.isEmpty)
      return IntEmptySet.getInstance
    val n = new IntHashSet(size)
    val it = iterator
    while (it.hasNext) {
      val v = it.next
      if (other.contains(v))
        n.add(v)
    }
    n
  }

  def except(other: IntSet): IntSet = {
    val n = new IntHashSet(size)
    val it = iterator
    while (it.hasNext) {
      val v = it.next
      if (! other.contains(v))
        n.add(v)
    }
    n
  }
}
