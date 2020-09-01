////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

package net.sf.saxon.z

import net.sf.saxon.tree.util.FastStringBuffer

import java.util.Arrays

import scala.beans.{BeanProperty, BooleanBeanProperty}


/**
 * Set of int values. This implementation of IntSet uses a sorted array
 * of integer ranges.
 *
 * @author Michael Kay
 */
class IntRangeSet extends IntSet {

  // The array of start points, which will always be sorted
  @BeanProperty
  var startPoints: Array[Int] = new Array[Int](4)

  // The array of end points, which will always be sorted
  @BeanProperty
  var endPoints: Array[Int] = new Array[Int](4)

  // The number of elements of the above two arrays that are actually in use
  private var used: Int = 0

  // Hashcode, evaluated lazily
  private var hashCodeVar: Int = -1

  // The number of items in the set
  var size: Int = 0

  def this(input: IntRangeSet) = {
    this()
    startPoints = Array.ofDim[Int](input.used)
    endPoints = Array.ofDim[Int](input.used)
    used = input.used
    System.arraycopy(input.startPoints, 0, startPoints, 0, used)
    System.arraycopy(input.endPoints, 0, endPoints, 0, used)
    hashCodeVar = input.hashCode
  }

  def this(startPoints: Array[Int], endPoints: Array[Int]) = {
    this()
    if (startPoints.length != endPoints.length) {
      throw new IllegalArgumentException("Array lengths differ")
    }
    this.startPoints = startPoints
    this.endPoints = endPoints
    used = startPoints.length
    for (i <- 0 until used) {
      size += (endPoints(i) - startPoints(i) + 1)
    }
  }

  def clear(): Unit = {
    startPoints = Array.ofDim[Int](4)
    endPoints = Array.ofDim[Int](4)
    used = 0
    hashCodeVar = -1
  }

  def copy(): IntSet = {
    val s: IntRangeSet = new IntRangeSet()
    s.startPoints = Array.ofDim[Int](startPoints.length)
    System.arraycopy(startPoints, 0, s.startPoints, 0, startPoints.length)
    //s.startPoints = Arrays.copyOf(startPoints, startPoints.length);
    s.endPoints = Array.ofDim[Int](endPoints.length)
    System.arraycopy(endPoints, 0, s.endPoints, 0, endPoints.length)
    //s.endPoints = Arrays.copyOf(endPoints, endPoints.length);
    s.used = used
    s.size = size
    s
  }

  def mutableCopy(): IntSet = copy()

  override def isMutable(): Boolean = false

  def isEmpty(): Boolean = size == 0

  def contains(value: Int): Boolean = {
    if (used == 0) {
      return false
    }
    if (value > endPoints(used - 1)) {
      return false
    }
    if (value < startPoints(0)) {
      return false
    }
    var i: Int = 0
    var j: Int = used
    do {
      val mid: Int = i + (j - i) / 2
      if (endPoints(mid) < value) {
        i = Math.max(mid, i + 1)
      } else if (startPoints(mid) > value) {
        j = Math.min(mid, j - 1)
      } else {
        true
      }
    } while (i != j);
    false
  }

  def remove(value: Int): Boolean =
    throw new UnsupportedOperationException("remove")

  def add(value: Int): Boolean = {
    hashCodeVar = -1
    if (used == 0) {
      ensureCapacity(1)
      startPoints(used - 1) = value
      endPoints(used - 1) = value
      size += 1;
      return true
    }
    if (value > endPoints(used - 1)) {
      if (value == endPoints(used - 1) + 1) {
        {
          endPoints(used - 1) += 1;
          endPoints(used - 1) - 1
        }
      } else {
        ensureCapacity(used + 1)
        startPoints(used - 1) = value
        endPoints(used - 1) = value
      }
      size += 1
      return true
    }
    if (value < startPoints(0)) {
      if (value == startPoints(0) - 1) {
        {
          startPoints(0) -= 1;
          startPoints(0) + 1
        }
      } else {
        ensureCapacity(used + 1)
        System.arraycopy(startPoints, 0, startPoints, 1, used - 1)
        System.arraycopy(endPoints, 0, endPoints, 1, used - 1)
        startPoints(0) = value
        endPoints(0) = value
      }
      size += 1
      return true
    }
    var i: Int = 0
    var j: Int = used
    do {
      val mid: Int = i + (j - i) / 2
      if (endPoints(mid) < value) {
        i = Math.max(mid, i + 1)
      } else if (startPoints(mid) > value) {
        j = Math.min(mid, j - 1)
      } else {
        // value is already present
        false
      }
    } while (i != j);
    if (i > 0 && endPoints(i - 1) + 1 == value) {
      i -= 1
    } else if (i < used - 1 && startPoints(i + 1) - 1 == value) {
      i += 1
    }
    if (endPoints(i) + 1 == value) {
      if (value == startPoints(i + 1) - 1) {
        // merge the two ranges
        endPoints(i) = endPoints(i + 1)
        System.arraycopy(startPoints, i + 2, startPoints, i + 1, used - i - 2)
        System.arraycopy(endPoints, i + 2, endPoints, i + 1, used - i - 2)
        used = used - 1
      } else {
        {
          endPoints(i) += 1;
          endPoints(i) - 1
        }
      }
      size += 1
      true
    } else if (startPoints(i) - 1 == value) {
      if (value == endPoints(i - 1) + 1) {
        // merge the two ranges
        endPoints(i - 1) = endPoints(i)
        System.arraycopy(startPoints, i + 1, startPoints, i, used - i - 1)
        System.arraycopy(endPoints, i + 1, endPoints, i, used - i - 1)
        used = used - 1
      } else {
        {
          startPoints(i) -= 1;
          startPoints(i) + 1
        }
      }
      size += 1
      true
    } else {
      if (value > endPoints(i)) {
        i += 1
      }
      ensureCapacity(used + 1)
      try {
        System.arraycopy(startPoints, i, startPoints, i + 1, used - i - 1)
        System.arraycopy(endPoints, i, endPoints, i + 1, used - i - 1)
      } catch {
        case err: Exception => err.printStackTrace()

      }
      startPoints(i) = value
      endPoints(i) = value
      size = size + 1
      true
    }
  }

  private def ensureCapacity(n: Int): Unit = {
    if (startPoints.length < n) {
      val s: Array[Int] = Array.ofDim[Int](startPoints.length * 2)
      val e: Array[Int] = Array.ofDim[Int](startPoints.length * 2)
      System.arraycopy(startPoints, 0, s, 0, used)
      System.arraycopy(endPoints, 0, e, 0, used)
      startPoints = s
      endPoints = e
    }
    used = n
  }

  def iterator(): IntIterator = new IntRangeSetIterator()

  override def toString: String = {
    val sb: FastStringBuffer = new FastStringBuffer(used * 8)
    for (i <- 0 until used) {
      sb.append(startPoints(i).toString + "-" + endPoints(i).toString + ",")
    }
    sb.toString
  }

  override def equals(other: Any): Boolean = other match {
    case other: IntSet =>
      if (other.isInstanceOf[IntRangeSet]) {
        var otherVar: IntRangeSet = other.asInstanceOf[IntRangeSet]
        if (used == otherVar.used && Arrays.equals(startPoints, otherVar.startPoints) && Arrays.equals(endPoints, otherVar.endPoints)) {
          return true
        } else {
          return false
        }
      } else {
        return containsAll(other)
      }
    case _ => return false

  }

  override def hashCode(): Int = {
    // Note, hashcodes are NOT the same as those used by IntHashSet and IntArraySet
    if (hashCodeVar == -1) {
      var h: Int = 0x836a89f1
      for (i <- 0 until used) {
        h ^= startPoints(i) + (endPoints(i) << 3)
      }
      hashCodeVar = h
    }
    hashCodeVar
  }

  def addRange(low: Int, high: Int): Unit = {
    if (low == high) {
      add(low)
      return
    }
    hashCodeVar = -1
    if (used == 0) {
      ensureCapacity(1)
      startPoints(used - 1) = low
      endPoints(used - 1) = high
      size += (high - low + 1)
    } else if (low > endPoints(used - 1)) {
      if (low == endPoints(used - 1) + 1) {
        endPoints(used - 1) = high
      } else {
        ensureCapacity(used + 1)
        startPoints(used - 1) = low
        endPoints(used - 1) = high
      }
      size += (high - low + 1)
    } else if (high < startPoints(0)) {
      ensureCapacity(used + 1)
      System.arraycopy(startPoints, 0, startPoints, 1, used - 1)
      System.arraycopy(endPoints, 0, endPoints, 1, used - 1)
      startPoints(0) = low
      endPoints(0) = high
    } else {
      for (i <- 1 until used
           if startPoints(i) > high && endPoints(i - 1) < low) {
        ensureCapacity(used + 1)
        System.arraycopy(startPoints, i, startPoints, i + 1, used - i - 1)
        System.arraycopy(endPoints, i, endPoints, i + 1, used - i - 1)
        startPoints(i) = low
        endPoints(i) = high
        return
      }
      // otherwise do it the hard way
      var i: Int = low
      while (i <= high) {
        add(i)
        i = i + 1
      }
    }
  }

  def getNumberOfRanges: Int = used

  private class IntRangeSetIterator extends IntIterator {

    private var i: Int = -1

    private var current: Int = java.lang.Integer.MIN_VALUE

    def hasNext(): Boolean =
      if (i < 0) {
        size > 0
      } else {
        current < endPoints(used - 1)
      }

    def next(): Integer = {
      if (i < 0) {
        i = 0
        current = startPoints(0)
        return current
      }
      if (current == endPoints(i)) {
        i = i + 1
        current = startPoints(i)
        current
      } else {
        current = current + 1;
        current
      }
    }

  }

}

