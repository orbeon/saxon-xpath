////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.query

import org.orbeon.saxon.trans.XPathException




/**
  * A QueryLibrary represents an independently compiled set of query modules that does not include a
  * main module. Such a library can be compiled once, and then linked to different main modules without
  * recompilation. The library contains one top-level module (itself a library module) together with the tree
  * of modules that it imports; it is identified by the module URI of the top-level module.
  * <p>This is an abstract class; the concrete implementation is in Saxon-EE.</p>
  */
abstract class QueryLibrary(sqc: StaticQueryContext) extends QueryModule(sqc) {

  def link(importer: QueryModule): Unit

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
