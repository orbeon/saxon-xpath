////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.tree.jiter

import java.util.Iterator
import java.util.function.Supplier

import scala.collection




class ConcatenatingIterator[E](var first: collection.Iterator[_ <: E],
                               var second: Supplier[collection.Iterator[_ <: E]])
    extends collection.Iterator[E] {

  var active: collection.Iterator[_ <: E] = first

  override def hasNext: Boolean =
    if (active.hasNext) {
      true
    } else if (active == first) {
      first = null
      active = second.get
      active.hasNext
    } else {
      false
    }

  /*@Nullable*/

  def next(): E = active.next()

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * An iterator over nodes, that concatenates the nodes returned by two supplied iterators.
  */
