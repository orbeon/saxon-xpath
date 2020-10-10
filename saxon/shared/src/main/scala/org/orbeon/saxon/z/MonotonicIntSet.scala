////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * Set of int values. This implementation requires that new entries are added in monotonically
 * increasing order; any attempt to add a value out of sequence, or to remove a value, results
 * is an UnsupportedOperationException
 */

package org.orbeon.saxon.z

import org.orbeon.saxon.tree.util.FastStringBuffer

import java.util.Arrays

import MonotonicIntSet._

import scala.util.control.Breaks._

object MonotonicIntSet {

  def make(in: Array[Int], size: Int): MonotonicIntSet =
    new MonotonicIntSet(in, size)

}

class MonotonicIntSet extends IntSet {

  private var contents: Array[Int] = new Array[Int](4)

  private var used: Int = 0

  def copy(): IntSet = {
    val i2: MonotonicIntSet = new MonotonicIntSet()
    i2.contents = Arrays.copyOf(contents, used)
    i2.used = used
    i2
  }

  def mutableCopy(): IntSet = copy()

  override def isMutable(): Boolean = false

  def clear(): Unit = {
    if (contents.length > used + 20) {
      contents = Array.ofDim[Int](4)
    }
    used = 0
  }

  def size(): Int = used

  def isEmpty: Boolean = used == 0

  def contains(value: Int): Boolean =
    Arrays.binarySearch(contents, 0, used, value) >= 0

  def remove(value: Int): Boolean = throw new UnsupportedOperationException()

  def add(value: Int): Boolean = {
    if (used > 0) {
      val last: Int = contents(used - 1)
      if (value == last) {
        return false
      } else if (value < last) {
        throw new UnsupportedOperationException(
          "Values must be added in monotonic order")
      }
    }
    if (used == contents.length) {
      contents = Arrays.copyOf(contents, if (used == 0) 4 else used * 2)
    }
    contents({
      used += 1;
      used - 1
    }) = value
    true
  }

  def iterator: IntIterator =
    new IntArraySet.IntArrayIterator(contents, used)

  override def union(other: IntSet): IntSet = {
    // Look for special cases: one set empty, or both sets equal
    if (size == 0) {
      return other.copy()
    } else if (other.isEmpty) {
      return copy()
    } else if (other == IntUniversalSet.getInstance) {
      return other
    } else if (other.isInstanceOf[IntComplementSet]) {
      return other.union(this)
    }
    if (equals(other)) {
      return copy()
    }
    if (other.isInstanceOf[MonotonicIntSet]) {
      // Form the union by a merge of the two sorted arrays
      val merged: Array[Int] = Array.ofDim[Int](size + other.size)
      val a: Array[Int] = contents
      val b: Array[Int] = other.asInstanceOf[MonotonicIntSet].contents
      val m: Int = used
      val n: Int = other.asInstanceOf[MonotonicIntSet].used
      var o: Int = 0
      var i: Int = 0
      var j: Int = 0
      while (true) {
        if (a(i) < b(j)) {
          merged(o) = a(i)
          o += 1
          i += 1
        } else if (b(j) < a(i)) {
          merged(o) = b(j)
          o += 1
          j += 1
        } else {
          merged(o) = a(i)
          o += 1
          i += 1
          j += 1
        }
        if (i == m) {
          System.arraycopy(b, j, merged, o, n - j)
          o += (n - j)
          return make(merged, o)
        } else if (j == n) {
          System.arraycopy(a, i, merged, o, m - i)
          o += (m - i)
          return make(merged, o)
        }
      }
    } else {
      val n = new IntHashSet(size)
      val it = iterator
      while ( {
        it.hasNext
      }) {
        val v = it.next
        if (!other.contains(v)) n.add(v)
      }
      return n
    }
    return null
  }

  def this(content: Array[Int], used: Int) = {
    this()
    this.contents = content
    this.used = used
  }

  override def toString: String = {
    val sb = new FastStringBuffer(used * 4)
    var i = 0
    while (i < used) {
      if (i == used - 1) {
        sb.append(contents(i).toString)
      } else if (contents(i) + 1 != contents(i + 1)) {
        sb.append(contents(i).toString + ",")
      } else {
        var j: Int = i + 1
        breakable {
          while (contents(j) == contents(j - 1) + 1) {
            j += 1
            if (j == used) {
              break()
            }
          }
        }
        sb.append(contents(i).toString + "-" + contents(j - 1).toString + ",")
        i = j
      }
    }
    sb.toString
  }

  override def equals(other: Any): Boolean =
    if (other.isInstanceOf[MonotonicIntSet]) {
      val s: MonotonicIntSet = other.asInstanceOf[MonotonicIntSet]
      if (used != s.used) {
        return false
      }
      for (i <- 0 until used if contents(i) != s.contents(i)) {
        return false
      }
      true
    } else
      other.isInstanceOf[IntSet] && used == other.asInstanceOf[IntSet].size &&
        containsAll(other.asInstanceOf[IntSet])

  override def hashCode: Int = {
    var h: Int = 936247625
    val it: IntIterator = iterator
    while (it.hasNext) h += it.next
    h
  }

}

