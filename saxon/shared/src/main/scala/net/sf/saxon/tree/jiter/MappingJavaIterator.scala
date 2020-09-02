////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.tree.jiter

import java.util.Iterator




class MappingJavaIterator[S, T](
    in: Iterator[S],
    private var mapper: java.util.function.Function[S, T])
    extends Iterator[T] {

  private var input: Iterator[S] = in

  def hasNext: Boolean = input.hasNext

  def next(): T = {
    while (true) {
      val next: T = mapper.apply(input.next())
      if (next != null) {
        return next
      }
    }
    null.asInstanceOf[T]
  }

  override def remove(): Unit = {
    input.remove()
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

