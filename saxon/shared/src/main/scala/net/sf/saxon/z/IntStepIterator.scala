////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * An iterator over a sequence of integers with regular steps, e.g. 2, 4, 6, 8...
  */

package net.sf.saxon.z

//remove if not needed
//import scala.collection.JavaConversions._

class IntStepIterator(start: Int,
                      private var step: Int,
                      private var limit: Int)
  extends IntIterator {

  private var current: Int = start

  def hasNext: Boolean = if (step > 0) current <= limit else current >= limit

  def next(): Integer = {
    val n: Int = current
    current += step
    n
  }

}

