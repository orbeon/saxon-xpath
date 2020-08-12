////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.lib

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.SpaceStrippingRule

import net.sf.saxon.trans.XPathException

import java.util.Iterator




/**
  * This interface defines a ResourceCollection. This is a counterpart to the JAXP
  * URIResolver, but is used to map the URI of collection into a sequence of Resource objects.
  * It is used to support the fn:collection() and fn:uri-collection() functions.
  * @since 9.7
  */
trait ResourceCollection {

  def getCollectionURI(): String

  def getResourceURIs(context: XPathContext): Iterator[String]

  def getResources(context: XPathContext): Iterator[_ <: Resource]

  def isStable(context: XPathContext): Boolean

  def stripWhitespace(rules: SpaceStrippingRule): Boolean

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
