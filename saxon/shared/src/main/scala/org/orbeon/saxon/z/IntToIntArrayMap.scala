////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

package org.orbeon.saxon.z

import scala.beans.BeanProperty


/**
 * An implementation of {@link IntToIntMap} that relies on serial searching, and
 * is therefore optimized for very small map sizes
 */
class IntToIntArrayMap extends IntToIntMap {

  private var keys: Array[Int] = new Array[Int](8)

  private var values: Array[Int] = new Array[Int](8)

  private var used: Int = 0

  @BeanProperty
  var defaultValue: Int = java.lang.Integer.MIN_VALUE

  def this(capacity: Int) = {
    this()
    if (capacity <= 0) {
      throw new IllegalArgumentException("capacity <= 0")
    }
    keys = Array.ofDim[Int](capacity)
    values = Array.ofDim[Int](capacity)
  }

  /**
   * Clear the map.
   */
  def clear(): Unit = {
    used = 0
  }

  /**
   * Finds a key in the map.
   *
   * @param key Key
   * @return true if the key is mapped
   */
  def find(key: Int): Boolean =
    (0 until used).find(keys(_) == key).map(_ => true).getOrElse(false)

  /**
   * Gets the value for this key.
   *
   * @param key Key
   * @return the value, or the default value if not found.
   */
  def get(key: Int): Int =
    (0 until used).find(keys(_) == key).map(values(_)).getOrElse(defaultValue)

  /*@NotNull*/

  def keyIterator: IntIterator = new KeyIterator

  /**
   * Adds a key-value pair to the map.
   *
   * @param key   Key
   * @param value Value
   */
  def put(key: Int, value: Int): Unit = {
    for (i <- 0 until used if keys(i) == key) {
      values(i) = value
      return
    }
    if (used >= keys.length) {
      val k2: Array[Int] = Array.ofDim[Int](used * 2)
      System.arraycopy(keys, 0, k2, 0, used)
      keys = k2
      val v2: Array[Int] = Array.ofDim[Int](used * 2)
      System.arraycopy(values, 0, v2, 0, used)
      values = v2
    }
    keys(used) = key
    values({ used += 1; used - 1 }) = value
  }

  /**
   * Removes a key from the map.
   *
   * @param key Key to remove
   * @return true if the value was removed
   */
  def remove(key: Int): Boolean = {
    for (i <- 0 until used if keys(i) == key) {
      values(i) = defaultValue
      return true
    }
    false
  }

  /**
   * Gets the size of the map.
   *
   * @return the size
   */
  def size(): Int = used

  @SerialVersionUID(1720894017771245276L)
  private class KeyIterator extends IntIterator {

    private var i: Int = 0

    def hasNext: Boolean = i < used

    def next(): Int = keys({ i += 1; i  })

  }

}
