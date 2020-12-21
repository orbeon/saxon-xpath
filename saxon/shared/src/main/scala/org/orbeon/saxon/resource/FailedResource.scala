////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.resource

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.lib.Resource

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.trans.XPathException

import scala.beans.{BeanProperty, BooleanBeanProperty}




/**
  * FailedResource represents an item in a collection that could not be processed because of some kind of error
  */
class FailedResource(private var uri: String,
                     @BeanProperty var error: XPathException)
    extends Resource {

  /**
    * Get the media type (MIME type) of the resource if known
    *
    * @return always null for this kind of resource
    */
  def getContentType(): String = null

  /**
    * Get a URI that identifies this resource
    *
    * @return a URI identifying this resource
    */
  def getResourceURI(): String = uri

  def getItem(context: XPathContext): Item = throw error

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
