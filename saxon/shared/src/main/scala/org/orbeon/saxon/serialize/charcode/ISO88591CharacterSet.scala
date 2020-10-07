////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.serialize.charcode

import ISO88591CharacterSet._




object ISO88591CharacterSet {

  private var theInstance: ISO88591CharacterSet = new ISO88591CharacterSet()

  def getInstance: ISO88591CharacterSet = theInstance

}

class ISO88591CharacterSet private () extends CharacterSet {

  def inCharset(c: Int): Boolean = c <= 0xff

  /*@NotNull*/

  def getCanonicalName(): String = "ISO-8859-1"

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class defines properties of the ISO-8859-1 character set
  */
