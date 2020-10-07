////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

package org.orbeon.saxon.z

import org.orbeon.saxon.tree.util.FastStringBuffer

import IntHashSet._

//remove if not needed

object IntHashSet {

  // MAX_SIZE = 2^NBIT
  private val NBIT: Int = 30

  // maximum number of keys mapped
  private val MAX_SIZE: Int = 1 << NBIT

  def containsSome(one: IntSet, two: IntSet): Boolean = {
    if (two.isInstanceOf[IntEmptySet]) {
      return false
    }
    if (two.isInstanceOf[IntUniversalSet]) {
      return ! one.isEmpty
    }
    two match {
      case set: IntComplementSet => return ! set.getExclusions.containsAll(one)
      case _ =>
    }
    val it = two.iterator
    while (it.hasNext) {
      if (one.contains(it.next))
        return true
    }
    false
  }

  def toString(it: IntIterator): String = {
    val sb: FastStringBuffer = new FastStringBuffer(100)
    while (it.hasNext) if (sb.isEmpty) {
      sb.append("" + it.next)
    } else {
      sb.append(" " + it.next)
    }
    sb.toString
  }

  def of(members: Int*): IntHashSet = {
    val is: IntHashSet = new IntHashSet(members.length)
    for (i <- members) {
      is.add(i)
    }
    is
  }

}

/**
 * Set of int values. This class is modelled on the java.net.Set interface, but it does
 * not implement this interface, because the set members are nint's rather than Objects.
 * <p>Not thread safe.</p>
 *
 * @author Dominique Devienne
 * @author Michael Kay: retrofitted to JDK 1.4, added iterator
 */
/**
 * Initializes a set with a load factor of 0,25.
 *
 * @param capacity the initial capacity.
 * @param ndv      the value to use for non-values.
 */

class IntHashSet(var capacity: Int, private val ndv: Int)
  extends IntSet {

  // 0 <= _nmax = 2^nbit <= 2^NBIT = MAX_SIZE
  private var _nmax: Int = _

  // 0 <= _size <= _nmax <= MAX_SIZE
  private var _size: Int = _

  // _nmax*_factor (_size<=_nlo, if possible)
  private var _nlo: Int = _

  //  MAX_SIZE*_factor (_size< _nhi, if possible)
  private var _nhi: Int = _

  // _shift = 1 + NBIT - nbit (see function hash() below)
  private var _shift: Int = _

  // _mask = _nmax - 1
  private var _mask: Int = _

  // array[_nmax] of values
  private var _values: Array[Int] = _

  //_factor = 0.25;
  setCapacity(capacity)

  /**
   * Initializes a set with a capacity of 8 and a load factor of 0,25.
   */
  /**
   * Initializes a set with a capacity of 8 and a load factor of 0,25.
   */
  def this() = this(8, java.lang.Integer.MIN_VALUE)

  /**
   * Initializes a set with the given capacity and a load factor of 0,25.
   *
   * @param capacity the initial capacity.
   */
  def this(capacity: Int) = this(capacity, java.lang.Integer.MIN_VALUE)

  def copy(): IntSet =
    if (_size == 0) {
      IntEmptySet.getInstance
    } else {
      val s: IntHashSet = new IntHashSet(_size, ndv)
      s._nmax = _nmax
      s._size = _size
      s._nlo = _nlo
      s._nhi = _nhi
      s._shift = _shift
      s._mask = _mask
      s._values = Array.ofDim[Int](_values.length)
      System.arraycopy(_values, 0, s._values, 0, _values.length)
      s
    }

  def mutableCopy(): IntSet = copy()

  def clear(): Unit = {
    _size = 0
    for (i <- 0 until _nmax) {
      _values(i) = ndv
    }
  }

  def size(): Int = _size

  def isEmpty: Boolean = _size == 0

  def getValues: Array[Int] = {
    var index: Int = 0
    val values: Array[Int] = Array.ofDim[Int](_size)
    for (_value <- _values if _value != ndv) {
      values({
        index += 1; index - 1
      }) = _value
    }
    values
  }

  def contains(value: Int): Boolean = (_values(indexOf(value)) != ndv)

  def remove(value: Int): Boolean = {
    // Knuth, v. 3, 527, Algorithm R.
    var i = indexOf(value)
    if (_values(i) == ndv)
      return false

    while (true) {
      _values(i) = ndv
      val j: Int = i
      var r: Int = 0
      do {
        i = (i - 1) & _mask
        if (_values(i) == ndv)
          return true
        r = hash(_values(i))
      } while ((i <= r && r < j) || (r < j && j < i) || (j < i && i <= r));
      _values(j) = _values(i)
    }
    false
  }

  def add(value: Int): Boolean = {
    if (value == ndv) {
      throw new IllegalArgumentException("Can't add the 'no data' value")
    }
    val i: Int = indexOf(value)
    if (_values(i) == ndv) {

      _values(i) = value
      // Check new size
      if (_size > MAX_SIZE) {
        throw new RuntimeException("Too many elements (> " + MAX_SIZE + ')')
      }
      if (_nlo < _size && _size <= _nhi) {
        this.capacity = _size
      }
      true
    } else {
      // leave set unchanged
      false
    }
  }

  private def hash(key: Int): Int = // The constant c = 1327217885 approximates 2^31 * (sqrt(5)-1)/2.
    ((1327217885 * key) >> _shift) & _mask

  /**
   * Gets the index of the value, if it exists, or the index at which
   * this value would be added if it does not exist yet.
   */
  private def indexOf(value: Int): Int = {
    var i: Int = hash(value)
    while (_values(i) != ndv) {
      if (_values(i) == value) {
        return i
      }
      i = (i - 1) & _mask
    }
    i
  }

  private def setCapacity(capacity: Int): Unit = {
    // Changed MHK in 8.9 to use a constant factor of 0.25, thus avoiding floating point arithmetic
    if (capacity < _size) {
      this.capacity = _size
    }
    //double factor = 0.25;
    var nbit: Int = 0
    var nmax: Int = 0
    nbit = 1
    nmax = 2
    while (nmax < capacity * 4 && nmax < MAX_SIZE) {

      nmax *= 2
    }
    // do nothing
    // do nothing
    val nold: Int = _nmax
    if (nmax == nold) {
      return
    }
    _nmax = nmax
    _nlo = nmax / 4
    _nhi = MAX_SIZE / 4
    _shift = 1 + NBIT - nbit
    _mask = nmax - 1
    _size = 0
    val values: Array[Int] = _values
    _values = Array.ofDim[Int](nmax)
    // empty all values
    java.util.Arrays.fill(_values, ndv)
    if (values != null) {
      for (i <- 0 until nold) {
        val value: Int = values(i)
        if (value != ndv) {
          //add(values[i]);

          _values(indexOf(value)) = value
        }
        // Don't use add, because the capacity is necessarily large enough,
        // and the value is necessarily unique (since in this set already)!
        // Don't use add, because the capacity is necessarily large enough,
        // and the value is necessarily unique (since in this set already)!
      }
    }
  }

  def iterator: IntIterator = new IntHashSetiterator

  override def equals(other: Any): Boolean = other match {
    case other: IntHashSet => {
      val s: IntHashSet = other
      (size == s.size && containsAll(s))
    }
    case _ => false

  }

  override def hashCode: Int = {
    // Note, hashcodes are the same as those used by IntArraySet
    var h: Int = 936247625
    val it: IntIterator = iterator
    while (it.hasNext) h += it.next
    h
  }

  override def toString: String = IntHashSet.toString(iterator)

  def diagnosticDump(): Unit = {
    System.err.println("Contents of IntHashSet")
    val sb: FastStringBuffer = new FastStringBuffer(100)
    for (i <- 0 until _values.length) {
      if (i % 10 == 0) {
        System.err.println(sb.toString)
        sb.setLength(0)
      }
      if (_values(i) == ndv) {
        sb.append("*, ")
      } else {
        sb.append(_values(i).toString + ", ")
      }
    }
    System.err.println(sb.toString)
    sb.setLength(0)
    System.err.println("size: " + _size)
    System.err.println("ndv: " + ndv)
    System.err.println("nlo: " + _nlo)
    System.err.println("nhi: " + _nhi)
    System.err.println("nmax: " + _nmax)
    System.err.println("shift: " + _shift)
    System.err.println("mask: " + _mask)
    System.err.println("Result of iterator:")
    val iter: IntIterator = iterator
    var i: Int = 0
    while (iter.hasNext) {
      if ( {
        i += 1; i - 1
      } % 10 == 0) {
        System.err.println(sb.toString)
        sb.setLength(0)
      }
      sb.append(iter.next.toString + ", ")
    }
    System.err.println(sb.toString)
    System.err.println("=====================")
  }

  private class IntHashSetiterator extends IntIterator {

    private var i: Int = 0

    def hasNext: Boolean = {
      while (i < _values.length) if (_values(i) != ndv) {
        true
      } else {
        {
          i += 1; i - 1
        }
      }
      false
    }

    def next: Integer = {
      _values(i)
      i = i + 1
      i
    }

  }

}
