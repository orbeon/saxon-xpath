////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.lib

import net.sf.saxon.trans.XPathException

import javax.xml.transform.stream.StreamSource




trait ModuleURIResolver {

  def resolve(moduleURI: String,
              baseURI: String,
              locations: Array[String]): Array[StreamSource]

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A ModuleURIResolver is used when resolving references to
  * query modules. It takes as input a URI that identifies the module to be loaded, and a set of
  * location hints, and returns one or more StreamSource objects containing the queries
  * to be imported.
  *
  * @author Michael H. Kay
  */
