////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

package org.orbeon.saxon.tree.jiter

import java.util
import java.util.NoSuchElementException


/**
 * An iterator over a single object (typically a sub-expression of an expression)
 */
class MonoIterator[T](var thing: T) extends util.Iterator[T] {

  // true if the single object has already been returned
  private var gone: Boolean = false

  def hasNext: Boolean = !gone

  def next(): T =
    if (gone) {
      throw new NoSuchElementException()
    } else {
      gone = true
      thing
    }

}
