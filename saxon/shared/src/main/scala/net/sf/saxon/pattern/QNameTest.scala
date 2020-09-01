////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Interface for tests against a QName. This is implemented by a subset of NodeTests, specifically
  * those that are only concerned with testing a name. It is used for matching error
  * codes against the codes specified in a try/catch clause, and also to match component names
  * in xsl:accept and xsl:expose.
  *
  * The various implementations of QNameTest typically match a node kind as well as node name. This
  * interface, however, is concerned only with matching of QNames, and the ability of an implementation
  * to match a node kind as well is redundant.
  */

package net.sf.saxon.pattern

import net.sf.saxon.om.StructuredQName


trait QNameTest {
  def matches(qname: StructuredQName): Boolean
  def exportQNameTest: String
  def generateJavaScriptNameTest(targetVersion: Int): String
}
