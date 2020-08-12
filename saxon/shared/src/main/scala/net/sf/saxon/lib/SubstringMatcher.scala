////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.lib




/**
  * This interface is implemented by a collation that is capable of supporting
  * the XPath functions that require matching of a substring: namely contains(),
  * starts-with, ends-with, substring-before, and substring-after. For sorting
  * and comparing strings, a collation needs to implement only the {@link StringCollator}
  * interface; for matching of substrings, it must also implement this interface.
  */
trait SubstringMatcher extends StringCollator {

  def contains(s1: String, s2: String): Boolean

  def startsWith(s1: String, s2: String): Boolean

  def endsWith(s1: String, s2: String): Boolean

  def substringBefore(s1: String, s2: String): String

  def substringAfter(s1: String, s2: String): String

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
