////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.value

import net.sf.saxon.model.StringToDouble

import StringToDouble11._




object StringToDouble11 {

  private var THE_INSTANCE: StringToDouble11 = new StringToDouble11()

  /*@NotNull*/

  def getInstance: StringToDouble11 = THE_INSTANCE

}

/**
  * Convert a string to a double using the rules of XML Schema 1.1
  */
class StringToDouble11  () extends StringToDouble {

   override def signedPositiveInfinity(): Double =
    java.lang.Double.POSITIVE_INFINITY.toDouble

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
