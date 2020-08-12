////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.om

import net.sf.saxon.trans.XPathException




/**
  * A sequence that wraps an iterator, without being materialized. It can only be used once.
  */
class LazySequence(var iterator: SequenceIterator) extends Sequence {

  var used: Boolean = false

  def head(): Item = iterate().next()

  def iterate(): SequenceIterator = synchronized {
    if (used) {
      throw new IllegalStateException("A LazySequence can only be read once")
    } else {
      used = true
      iterator
    }
  }

  override def makeRepeatable(): Sequence = materialize()

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
