////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.resource

import net.sf.saxon.expr.Callable

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.functions.CallableFunction

import net.sf.saxon.lib.Resource

import net.sf.saxon.ma.map.DictionaryMap

import net.sf.saxon.model.FunctionItemType

import net.sf.saxon.model.SpecificFunctionType

import net.sf.saxon.om.GroundedValue

import net.sf.saxon.om.Item

import net.sf.saxon.value.SequenceType

import net.sf.saxon.value.StringValue

import java.util.Map

import scala.beans.{BeanProperty, BooleanBeanProperty}




/**
  * Created by mike on 28/10/15.
  */
class MetadataResource(@BeanProperty var resourceURI: String,
                       private var content: Resource,
                       private var properties: Map[String, GroundedValue])
    extends Resource {

  def getContentType(): String = content.getContentType

  def getItem(context: XPathContext): Item = {
// Create a map for the result
    val map: DictionaryMap = new DictionaryMap()
    properties.forEach{ (key, value) =>
      map.initialPut(key, value)
    }
// Add the resourceURI of the resource as the "name" property
    map.initialPut("name", StringValue.makeStringValue(resourceURI))
// Add a fetch() function, which can be used to fetch the resource
    val fetcher: Callable = (context1, arguments) => content.getItem(context1)
    val fetcherType: FunctionItemType = new SpecificFunctionType(
      Array.ofDim[SequenceType](0),
      SequenceType.SINGLE_ITEM)
    val fetcherFunction: CallableFunction =
      new CallableFunction(0, fetcher, fetcherType)
    map.initialPut("fetch", fetcherFunction)
    map
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
