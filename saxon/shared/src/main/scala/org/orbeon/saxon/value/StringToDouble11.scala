////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.value

import org.orbeon.saxon.model.StringToDouble


object StringToDouble11 {
  val getInstance: StringToDouble11 = new StringToDouble11
}

/**
  * Convert a string to a double using the rules of XML Schema 1.1
  */
class StringToDouble11 extends StringToDouble {
  override def signedPositiveInfinity: Double =
    java.lang.Double.POSITIVE_INFINITY
}

