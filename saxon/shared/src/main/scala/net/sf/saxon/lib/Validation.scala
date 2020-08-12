////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.lib




object Validation {

  val INVALID: Int = -1

  val STRICT: Int = 1

  val LAX: Int = 2

  val PRESERVE: Int = 3

  val STRIP: Int = 4

// synonym provided for the XQuery API
  val SKIP: Int = 4

  val DEFAULT: Int = 0

  val BY_TYPE: Int = 8

  def getCode(value: String): Int =
    if (value.==("strict")) {
      STRICT
    } else if (value.==("lax")) {
      LAX
    } else if (value.==("preserve")) {
      PRESERVE
    } else if (value.==("strip")) {
      STRIP
    } else {
      INVALID
    }

  /*@NotNull*/

  def toString(value: Int): String = value match {
    case STRICT => "strict"
    case LAX => "lax"
    case PRESERVE => "preserve"
    case STRIP => // for XQuery
      "skip"
    case BY_TYPE => "by type"
    case _ => "invalid"

  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class contains constants and static methods to manipulate the validation
  * property of a type.
  */
