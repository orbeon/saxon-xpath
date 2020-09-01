////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.lib

import net.sf.saxon.expr.number.NumericGroupFormatter

import net.sf.saxon.regex.UnicodeString

import java.util.Locale




trait Numberer {

  def setCountry(country: String): Unit

  def getCountry: String

  /**
    * Whether this numberer has had its locale defaulted, i.e. it's not using the language requested
    * This can be used for situations, such as in fn:format-date(), where indication of use of defaults is required.
    * Override for subclasses where this can happen
    * @return the locale used, null if it wasn't defaulted
    */
  def defaultedLocale(): Locale

  def format(number: Long,
             picture: UnicodeString,
             groupSize: Int,
             groupSeparator: String,
             letterValue: String,
             ordinal: String): String

  def format(number: Long,
             picture: UnicodeString,
             numGrpFormatter: NumericGroupFormatter,
             letterValue: String,
             ordinal: String): String

  def monthName(month: Int, minWidth: Int, maxWidth: Int): String

  def dayName(day: Int, minWidth: Int, maxWidth: Int): String

  def halfDayName(minutes: Int, minWidth: Int, maxWidth: Int): String

  def getOrdinalSuffixForDateTime(component: String): String

  def getEraName(year: Int): String

  def getCalendarName(code: String): String

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Interface Numberer supports number formatting. There is a separate
  * implementation for each language, e.g. Numberer_en for English.
  * This supports the xsl:number element
  *
  * @author Michael H. Kay
  */
