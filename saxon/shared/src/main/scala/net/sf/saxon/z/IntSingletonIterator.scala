////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * An iterator over a single integer
  */

package net.sf.saxon.z

//remove if not needed
//import scala.collection.JavaConversions._

class IntSingletonIterator(private var value: Int) extends IntIterator {

  var gone: Boolean = false

  def hasNext(): Boolean = !gone

  def next(): Integer = {
    gone = true
    value
  }

}

