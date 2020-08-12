////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.pattern

import net.sf.saxon.expr.StaticContext

import net.sf.saxon.trans.XPathException




/**
  * Interface to a parser of XSLT patterns. There were originally subclasses for XSLT 2.0 and
  * XSLT 3.0 patterns
  */
trait PatternParser {

  def parsePattern(pattern: String, env: StaticContext): Pattern

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
