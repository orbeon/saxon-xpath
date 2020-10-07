////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.serialize

import java.io.Writer

import HexCharacterReferenceGenerator._




object HexCharacterReferenceGenerator {

  val THE_INSTANCE: HexCharacterReferenceGenerator =
    new HexCharacterReferenceGenerator()

}

class HexCharacterReferenceGenerator private ()
    extends CharacterReferenceGenerator {

  def outputCharacterReference(charval: Int, writer: Writer): Unit = {
    writer.write("&#x")
    writer.write(java.lang.Integer.toHexString(charval))
    writer.write(';')
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A class that represents a character as a hexadecimal character reference
  * and writes the result to a supplied Writer
  */
