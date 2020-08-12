////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.resource

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.lib.Resource

import net.sf.saxon.lib.ResourceFactory

import net.sf.saxon.ma.json.ParseJsonFn

import net.sf.saxon.om.Item

import net.sf.saxon.om.Sequence

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.BooleanValue

import net.sf.saxon.value.StringValue

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
