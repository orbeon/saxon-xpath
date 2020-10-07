////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Abstract class that supports lookup of a lexical QName to get the expanded QName.
  */

package org.orbeon.saxon.om

import java.util.Iterator


trait NamespaceResolver {
  /*@Nullable*/
  def getURIForPrefix(prefix: String, useDefault: Boolean): String
  def iteratePrefixes: Iterator[String]
}
