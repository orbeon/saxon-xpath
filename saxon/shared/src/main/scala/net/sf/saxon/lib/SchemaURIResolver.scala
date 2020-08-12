////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.lib

import net.sf.saxon.utils.Configuration

import net.sf.saxon.trans.XPathException

import javax.xml.transform.Source




trait SchemaURIResolver {

  def setConfiguration(config: Configuration): Unit

  def resolve(targetNamespace: String,
              baseURI: String,
              locations: Array[String]): Array[Source]

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A SchemaURIResolver is used when resolving references to
  * schema documents. It takes as input the target namespace of the schema to be loaded, and a set of
  * location hints as input, and returns one or more Source obects containing the schema documents
  * to be imported.
  *
  * @author Michael H. Kay
  */
