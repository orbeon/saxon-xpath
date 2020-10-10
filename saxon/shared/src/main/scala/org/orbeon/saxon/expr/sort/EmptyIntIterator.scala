////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.sort

import org.orbeon.saxon.z.IntIterator

import EmptyIntIterator._


object EmptyIntIterator {

  var THE_INSTANCE: EmptyIntIterator = new EmptyIntIterator

  /*@NotNull*/

  def getInstance: EmptyIntIterator = THE_INSTANCE

}

/**
 * An iterator over a zero-length sequence of integers
 */
class EmptyIntIterator extends IntIterator {

  def hasNext: Boolean = false

  def next(): Int = 0

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
