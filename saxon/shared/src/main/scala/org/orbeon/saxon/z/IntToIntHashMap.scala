package org.orbeon.saxon.z

import org.orbeon.saxon.tree.util.FastStringBuffer
import org.orbeon.saxon.z.IntToIntHashMap._

import scala.util.control.Breaks._

object IntToIntHashMap {

  private val NBIT: Int = 30

  private val NMAX: Int = 1 << NBIT

}

class IntToIntHashMap(var capacity: Int, private var _factor: Double)
  extends IntToIntMap {

  this.capacity = capacity

  def this() = this(8, 0.25)

  def this(capacity: Int) = this(capacity, 0.25)

  def clear(): Unit = {
    _n = 0
    for (i <- 0 until _nmax) {
      _filled(i) = false
    }
  }

  def find(key: Int): Boolean = _filled(indexOf(key))

  def get(key: Int): Int = {
    val i: Int = indexOf(key)
    if (_filled(i)) _value(i) else defaultValue
  }

  def size(): Int = _n

  def remove(key: Int): Boolean = {
    var i: Int = indexOf(key)
    if (!_filled(i)) {
      return false
    }

    while (true) {
      _filled(i) = false
      val j: Int = i
      var r: Int = 0
      do {
        i = (i - 1) & _mask
        if (!_filled(i)) {
          return true
        }
        r = hash(_key(i))
      } while ((i <= r && r < j) || (r < j && j < i) || (j < i && i <= r));
      _key(j) = _key(i)
      _value(j) = _value(i)
      _filled(j) = _filled(i)
    }
    false
  }

  def put(key: Int, value: Int): Unit = {
    val i: Int = indexOf(key)
    if (_filled(i)) {
      _value(i) = value
    } else {
      _key(i) = key
      _value(i) = value
      _filled(i) = true
      grow()
    }
  }

  def keyIterator: IntIterator = new IntToIntHashMapKeyIterator

  private var defaultValue: Int = java.lang.Integer.MAX_VALUE

  override def setDefaultValue(defaultVal: Int): Unit = defaultValue = defaultVal

  override def getDefaultValue(): Int = defaultValue

  private var _nmax: Int = _

  private var _n: Int = _

  private var _nlo: Int = _

  private var _nhi: Int = _

  private var _shift: Int = _

  private var _mask: Int = _

  private var _key: Array[Int] = _

  private var _value: Array[Int] = _

  private var _filled: Array[Boolean] = _

  private def hash(key: Int): Int = ((1327217885 * key) >> _shift) & _mask

  private def indexOf(key: Int): Int = {
    var i: Int = hash(key)
    while (_filled(i)) {
      if (_key(i) == key) {
        return i
      }
      i = (i - 1) & _mask
    }
    i
  }

  private def grow(): Unit = {

    if (_n > NMAX) {
      throw new RuntimeException("number of keys mapped exceeds " + NMAX)
    }
    if (_nlo < _n && _n <= _nhi) {
      this.capacity = _n
    }
  }

  private def setCapacity(capacity: Int): Unit = {
    var cap = capacity
    if (cap < _n) {
      cap = _n
    }
    val factor: Double =
      if ((_factor < 0.01)) 0.01 else if ((_factor > 0.99)) 0.99 else _factor
    var nbit: Int = 0
    var nmax: Int = 0
    nbit = 1
    nmax = 2
    while (nmax * factor < capacity && nmax < NMAX) {

      nmax *= 2
    }
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
    val value: Array[Int] = _value
    val filled: Array[Boolean] = _filled
    _n = 0
    _key = Array.ofDim[Int](nmax)
    _value = Array.ofDim[Int](nmax)
    _filled = Array.ofDim[Boolean](nmax)
    if (key != null) {
      for (i <- 0 until nold if filled(i)) {
        put(key(i), value(i))
      }
    }
  }

  override def toString: String = {
    val buffer: FastStringBuffer = new FastStringBuffer(256)
    buffer.append("{")
    val keys: IntIterator = keyIterator
    var count: Int = 0
    breakable {
      while (keys.hasNext) {
        val k: Int = keys.next
        val v: Int = get(k)
        buffer.append(" " + k + ":" + v + ",")
        if ( {
          count += 1;
          count - 1
        } >= 100) {
          buffer.append("....")
          break()
        }
      }
    }
    buffer.setCharAt(buffer.length - 1, '}')
    buffer.toString
  }

  private class IntToIntHashMapKeyIterator extends IntIterator {

    private var i: Int = 0

    def hasNext: Boolean = {
      while (i < _key.length) if (_filled(i)) {
        true
      } else {
        i += 1
      }
      false
    }

    def next(): Int = {
      i += 1
      _key(i)
    }

  }

}
