////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A hash table that maps int keys to Object values.
  *
  * @author Dave Hale, Landmark Graphics
  * @author Dominique Devienne
  * @author Michael Kay: retrofitted to JDK 1.4, added iterator, modified to disallow null values
  *         Reverted to generics July 2008.
  */
package org.orbeon.saxon.z

import java.io.PrintStream
import java.util.{Iterator, NoSuchElementException}

import org.orbeon.saxon.z.IntHashMap._

object IntHashMap {

  // NMAX = 2^NBIT
  private val NBIT: Int = 30

  // maximum number of keys mapped
  private val NMAX: Int = 1 << NBIT

}
/**
  * Constructs a new map with initial capacity, and load factor.
  * <p>The capacity is the number of keys that can be mapped without resizing
  * the arrays in which keys and values are stored. For efficiency, only
  * a fraction of the elements in those arrays are used. That fraction is
  * the specified load factor. The initial length of the arrays equals the
  * smallest power of two not less than the ratio capacity/factor. The
  * capacity of the map is increased, as necessary. The maximum number
  * of keys that can be mapped is 2^30.</p>
  *
  */
class IntHashMap[T >: Null <: AnyRef](var capacity: Int, var _factor: Double) {

  setCapacity(capacity)

  /**
    * Initializes a map with a capacity of 8 and a load factor of 0,25.
    */
  /**
    * Initializes a map with a capacity of 8 and a load factor of 0,25.
    */
  def this() = this(8, 0.25)

  /**
    * Initializes a map with the given capacity and a load factor of 0,25.
    *
    * @param capacity the initial capacity.
    */
  def this(capacity: Int) = {
    this(capacity, 0.25)
  }

  /**
    * Clears the map.
    */
  def clear(): Unit = {
    _n = 0
    for (i <- 0 until _nmax) {
      _value(i) = null
    }
  }

  /**
    * Gets the value for this key.
    *
    * @param key Key
    * @return the value, null if not found.
    */
  def get(key: Int): T = _value(indexOf(key))

  /**
    * Gets the size of the map.
    *
    * @return the size (the number of entries in the map)
    */
  def size(): Int = _n

  /**
    * Removes a key from the map.
    *
    * @param key Key to remove
    * @return true if the value was removed
    */
  def remove(key: Int): Boolean = {
    // Knuth, v. 3, 527, Algorithm R.
    var i: Int = indexOf(key)
    //if (!_filled[i]) {
    if (_value(i) == null) {
      return false
    }

    while (true) {
      _value(i) = null
      val j: Int = i
      var r: Int = 0
      do {
        i = (i - 1) & _mask
        //if (!_filled[i]) {
        if (_value(i) == null) {
          return true
        }
        r = hash(_key(i))
      } while ((i <= r && r < j) || (r < j && j < i) || (j < i && i <= r));
      _key(j) = _key(i)
      _value(j) = _value(i)
    }
    false
    //_filled[j] = _filled[i];
    //_filled[j] = _filled[i];
  }

  /**
    * Adds a key-value pair to the map.
    *
    * @param key   Key
    * @param value Value
    * @return the value that was previously associated with the key, or null if there was no previous value
    */
  def put(key: Int, value: T): T = {
    if (value == null) {
      throw new NullPointerException("IntHashMap does not allow null values")
    }
    val i: Int = indexOf(key)
    val old: T = _value(i)
    if (old != null) {
      _value(i) = value
    } else {
      _key(i) = key
      _value(i) = value
      grow()
    }
    old
  }

  // 0 <= _nmax = 2^nbit <= 2^NBIT = NMAX
  private var _nmax: Int = _

  // 0 <= _n <= _nmax <= NMAX
  private var _n: Int = _

  // _nmax*_factor (_n<=_nlo, if possible)
  private var _nlo: Int = _

  //  NMAX*_factor (_n< _nhi, if possible)
  private var _nhi: Int = _

  // _shift = 1 + NBIT - nbit (see function hash() below)
  private var _shift: Int = _

  // _mask = _nmax - 1
  private var _mask: Int = _

  // array[_nmax] of keys
  private var _key: Array[Int] = _

  // array[_nmax] of values
  private var _value: Array[T] = _

  private def hash(key: Int): Int = // The constant c = 1327217885 approximates 2^31 * (sqrt(5)-1)/2.
    ((1327217885 * key) >> _shift) & _mask

  private def indexOf(key: Int): Int = {
    var i: Int = hash(key)
    //while (_filled[i]) {
    while (_value(i) != null) {
      if (_key(i) == key) {
        return i
      }
      i = (i - 1) & _mask
    }
    i
  }

  private def grow(): Unit = {
    _n += 1
    if (_n > NMAX) {
      throw new RuntimeException("number of keys mapped exceeds " + NMAX)
    }
    if (_nlo < _n && _n <= _nhi) {
      capacity = _n
    }
  }

  private def setCapacity(capacity: Int): Unit = {
    if (capacity < _n) {
      this.capacity = _n
    }
    val factor: Double =
      if ((_factor < 0.01)) 0.01 else if ((_factor > 0.99)) 0.99 else _factor
    var nbit: Int = 0
    var nmax: Int = 0
    nbit = 1
    nmax = 2
    while (nmax * factor < capacity && nmax < NMAX) {
      nbit += 1
      nmax *= 2
    }
    // no-op
    // no-op
    val nold: Int = _nmax
    if (nmax == nold) {
      return
    }
    _nmax = nmax
    _nlo = (nmax * factor).toInt
    _nhi = (NMAX * factor).toInt
    _shift = 1 + NBIT - nbit
    _mask = nmax - 1
    val key: Array[Int] = _key
    val value: Array[T] = _value
    //boolean[] filled = _filled;
    _n = 0
    _key = Array.ofDim[Int](nmax)
    // semantically equivalent to _value = new V[nmax]
    _value = Array.ofDim[Any](nmax).asInstanceOf[Array[T]]
    //_filled = new boolean[nmax];
    if (key != null) {
      for (i <- 0 until nold if value(i) != null) {
        put(key(i), value(i))
      }
    }
  }

  def keyIterator: IntIterator = new IntHashMapKeyIterator

  def valueiterator: Iterator[T] = new IntHashMapValueIterator

  def valueSet(): java.lang.Iterable[T] = new java.lang.Iterable[T]() {
    def iterator: Iterator[T] = valueiterator
  }

  def copy(): IntHashMap[T] = {
    val n: IntHashMap[T] = new IntHashMap[T](size)
    val it: IntIterator = keyIterator
    while (it.hasNext) {
      val k: Int = it.next
      n.put(k, get(k))
    }
    n
  }

  def display(ps: PrintStream): Unit = {
    val iter: IntIterator = new IntHashMapKeyIterator
    while (iter.hasNext) {
      val key: Int = iter.next
      val value: T = get(key)
      ps.println(key.toString + " -> " + value.toString)
    }
  }

  /**
    * Iterator over keys
    */
  private class IntHashMapKeyIterator extends IntIterator {

    private var i: Int = 0

    def hasNext: Boolean = {
      while (i < _key.length) if (_value(i) != null) {
        return true
      } else {
        { i += 1; i - 1 }
      }
      false
    }

    def next(): Int = _key({
      i += 1; i - 1
    })

  }

  /**
    * Iterator over values
    */
  private class IntHashMapValueIterator extends Iterator[T] {

    private var i: Int = 0

    def hasNext: Boolean = {
      while (i < _key.length) if (_value(i) != null) {
        return true
      } else {
        { i += 1; i - 1 }
      }
      false
    }

    def next(): T = {
      val temp: T = _value({ i += 1; i - 1 })
      if (temp == null) {
        throw new NoSuchElementException()
      }
      temp
    }

    /**
      * Removes from the underlying collection the last element returned by the
      * iterator (optional operation).
      *
      * @throws UnsupportedOperationException if the <tt>remove</tt>
      *                                       operation is not supported by this Iterator.
      */
    override def remove(): Unit = {
      throw new UnsupportedOperationException("remove")
    }
  }

  def keySet(): IntSet = new IntSet() {
    def clear(): Unit = {
      throw new UnsupportedOperationException("Immutable set")
    }

    def copy(): IntSet = {
      val s: IntHashSet = new IntHashSet()
      val ii: IntIterator = iterator
      while (ii.hasNext) s.add(ii.next)
      s
    }

    def mutableCopy(): IntSet = copy()

    override def isMutable(): Boolean = false

    def size(): Int = _n

    def isEmpty: Boolean = _n == 0

    def contains(key: Int): Boolean = _value(indexOf(key)) != null

    def remove(value: Int): Boolean =
      throw new UnsupportedOperationException("Immutable set")

    def add(value: Int): Boolean =
      throw new UnsupportedOperationException("Immutable set")

    def iterator: IntIterator = new IntHashMapKeyIterator

    override def union(other: IntSet): IntSet = copy().union(other)

    override def intersect(other: IntSet): IntSet = copy().intersect(other)

    override def except(other: IntSet): IntSet = copy().except(other)

    override def containsAll(other: IntSet): Boolean = copy().containsAll(other)

    override def toString: String = IntHashSet.toString(iterator)
  }
}
