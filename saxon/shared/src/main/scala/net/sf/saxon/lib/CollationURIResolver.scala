////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.lib

import net.sf.saxon.utils.Configuration

import net.sf.saxon.trans.XPathException




/**
  * A CollationURIResolver accepts a collation name as input, and returns
  * a collation (represented by a {@link StringCollator} as output. A CollationURIResolver
  * can be registered with the Configuration (or with a TransformerFactory)
  * to resolve all collation URIs used in a stylesheet or query.
  */
trait CollationURIResolver {

  def resolve(collationURI: String, config: Configuration): StringCollator

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
