////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.tree.tiny




/**
  * Defines a CharSequence to which characters can be appended
  */
trait AppendableCharSequence extends CharSequence {

  /**
    * Append characters to this CharSequence
    *
    * @param chars the characters to be appended
    * @return the concatenated results
    */
  def cat(chars: CharSequence): AppendableCharSequence

  def cat(c: Char): AppendableCharSequence

  def setLength(length: Int): Unit

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
