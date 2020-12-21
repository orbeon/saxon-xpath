////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.trans




/**
  * Utility class for collecting and reporting timing information, used only under diagnostic control
  */
class Timer {

  private var start: Long = System.nanoTime()

  private var prev: Long = _

  def report(label: String): Unit = {
    val time: Long = System.nanoTime()
    System.err.println(label + " " + (time - prev) / 1e6 + "ms")
    prev = time
  }

  def reportCumulative(label: String): Unit = {
    val time: Long = System.nanoTime()
    System.err.println(label + " " + (time - start) / 1e6 + "ms")
    prev = time
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
