////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.tree.util


/**
  * Interface that accepts a string in the form of a sequence of CharSequences,
  * which are conceptually concatenated (though in some implementations, the final
  * string may never be materialized in memory)
  */
trait CharSequenceConsumer {
  def open(): Unit = ()
  def cat(chars: CharSequence): CharSequenceConsumer
  def cat(c: Char): CharSequenceConsumer = cat("" + c)
  def close(): Unit = ()
}
