////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.serialize.charcode

import ASCIICharacterSet._




object ASCIICharacterSet {

  val theInstance: ASCIICharacterSet = new ASCIICharacterSet()

  def getInstance: ASCIICharacterSet = theInstance

}

class ASCIICharacterSet private () extends CharacterSet {

  def inCharset(c: Int): Boolean = c <= 0x7f

  /*@NotNull*/

  def getCanonicalName(): String = "US-ASCII"

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class defines properties of the US-ASCII character set
  */
