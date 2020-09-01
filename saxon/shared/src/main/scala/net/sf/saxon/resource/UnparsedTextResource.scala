////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.resource

import net.sf.saxon.utils.Configuration

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.lib.Resource

import net.sf.saxon.lib.ResourceFactory

import net.sf.saxon.lib.StandardUnparsedTextResolver

import net.sf.saxon.om.Item

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.StringValue

import java.io.ByteArrayInputStream

import java.io.IOException

import java.io.InputStream

import java.io.UnsupportedEncodingException

import java.net.URL

import java.net.URLConnection

import UnparsedTextResource._

import scala.beans.{BeanProperty, BooleanBeanProperty}




object UnparsedTextResource {

  val FACTORY: ResourceFactory = new ResourceFactory() {
    def makeResource(
        config: Configuration,
        details: AbstractResourceCollection.InputDetails): Resource =
      new UnparsedTextResource(details)
  }

}

/**
  * This class implements th interface Resource. We handle unparded text here.
  * The Resource objects belong to a collection
  * It is used to support the fn:collection() and fn:uri-collection() functions.
  *
  * @since 9.7
  */
class UnparsedTextResource(details: AbstractResourceCollection.InputDetails)
    extends Resource {

  private var contentType: String = details.contentType

  @BeanProperty
  var encoding: String = details.encoding

  private var href: String = details.resourceUri

  private var unparsedText: String = null

  if (details.characterContent != null) {
    unparsedText = details.characterContent
  } else if (details.binaryContent != null) {
    if (details.encoding == null) {
      val is: InputStream = new ByteArrayInputStream(details.binaryContent)
      details.encoding =
        StandardUnparsedTextResolver.inferStreamEncoding(is, null)
      is.close()
    }
    this.unparsedText = new String(details.binaryContent, details.encoding)
  }

  def getResourceURI(): String = href

  def getContent: String = {
    if (unparsedText == null) {
      val url: URL = new URL(href)
      val connection: URLConnection = url.openConnection()
      val stream: InputStream = connection.getInputStream
      var builder: StringBuilder = null
      var enc: String = encoding
      if (enc == null) {
        enc = StandardUnparsedTextResolver.inferStreamEncoding(stream, null)
      }
      builder = CatalogCollection.makeStringBuilderFromStream(stream, enc)
      unparsedText = builder.toString
    }
    unparsedText
  }

  def getItem(context: XPathContext): Item = new StringValue(getContent)

  def getContentType(): String =
    if (contentType == null) "text/plain" else contentType

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
