////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr

import java.util.Iterator

import java.util.List




/**
  * An iterator that combines the results of a sequence of iterators
  */
class MultiIterator[T](private var array: List[Iterator[T]])
    extends Iterator[T] {

  private var current: Int = 0

  def hasNext: Boolean = {
    while (true) {
      if (current >= array.size) {
        return false
      }
      if (array.get(current).hasNext) {
        return true
      }
      current += 1
    }
    false
  }
  /**
    * Returns the next element in the iteration.
    *
    * @return the next element in the iteration.
    * @throws java.util.NoSuchElementException
    *          iteration has no more elements.
    */
  def next(): T = array.get(current).next()

  /**
    * Removes from the underlying collection the last element returned by the
    * iterator (optional operation).  This method can be called only once per
    * call to <tt>next</tt>.  The behavior of an iterator is unspecified if
    * the underlying collection is modified while the iteration is in
    * progress in any way other than by calling this method.
    *
    * @throws UnsupportedOperationException if the <tt>remove</tt>
    *                                       operation is not supported by this Iterator.
    * @throws IllegalStateException         if the <tt>next</tt> method has not
    *                                       yet been called, or the <tt>remove</tt> method has already
    *                                       been called after the last call to the <tt>next</tt>
    *                                       method.
    */
  override def remove(): Unit = {
    throw new UnsupportedOperationException
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
