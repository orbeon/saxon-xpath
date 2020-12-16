////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.tree.jiter

import java.util.NoSuchElementException
import java.{util => ju}


/**
  * An iterator over a pair of objects (typically sub-expressions of an expression)
  */
class PairIterator[T](private var one: T, private var two: T)
    extends ju.Iterator[T] {

  private var pos: Int = 0

  def hasNext: Boolean = pos < 2

  /**
    * Returns the next element in the iteration.
    *
    * @return the next element in the iteration.
    * @throws NoSuchElementException iteration has no more elements.
    */
  def next(): T = {
    pos += 1; pos - 1
  } match {
    case 0 => one
    case 1 => two
    case _ => throw new NoSuchElementException
  }

  override def remove(): Unit = throw new UnsupportedOperationException("remove")
}

