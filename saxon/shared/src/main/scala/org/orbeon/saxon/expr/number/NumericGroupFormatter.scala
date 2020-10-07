////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.number

import org.orbeon.saxon.regex.UnicodeString

import org.orbeon.saxon.tree.util.FastStringBuffer




abstract class NumericGroupFormatter {

   var adjustedPicture: UnicodeString = _

  def getAdjustedPicture: UnicodeString = adjustedPicture

  def format(value: FastStringBuffer): String

  /*@Nullable*/

  def getSeparator: String

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A NumericGroupFormatter is responsible for insertion of grouping separators
  * into a formatted number (for example, reformatting "1234" as "1,234").
  */
