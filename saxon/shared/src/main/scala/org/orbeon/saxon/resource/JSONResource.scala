////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.resource

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.lib.Resource

import org.orbeon.saxon.lib.ResourceFactory

import org.orbeon.saxon.ma.json.ParseJsonFn

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.BooleanValue

import org.orbeon.saxon.value.StringValue

import java.util.HashMap

import java.util.Map

import JSONResource._




object JSONResource {

  val FACTORY: ResourceFactory = (config, details) => new JSONResource(details)

}

class JSONResource(
    private var details: AbstractResourceCollection.InputDetails)
    extends Resource {

  private var href: String = details.resourceUri

  private var jsonStr: String = _

  if (details.encoding == null) {
    details.encoding = "UTF-8"
  }

  def getResourceURI(): String = href

  def getItem(context: XPathContext): Item = {
    if (jsonStr == null) {
      jsonStr = details.obtainCharacterContent()
    }
    val options: Map[String, Sequence] = new HashMap[String, Sequence]()
    options.put("liberal", BooleanValue.FALSE)
    options.put("duplicates", new StringValue("use-first"))
    options.put("escape", BooleanValue.FALSE)
    ParseJsonFn.parse(jsonStr, options, context)
  }

  def getContentType(): String = "application/json"

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A Resource (that is, an item in a collection) holding JSON content
  */
