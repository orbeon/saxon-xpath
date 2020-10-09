////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.resource

import java.io.{ByteArrayInputStream, InputStream}

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.lib.{Resource, ResourceFactory, StandardUnparsedTextResolver}
import org.orbeon.saxon.om.Item
import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.value.StringValue

import scala.beans.BeanProperty


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

  private val contentType: String = details.contentType

  @BeanProperty
  var encoding: String = details.encoding

  private val href: String = details.resourceUri

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

  def getResourceURI: String = href

  def getContent: String = {
    if (unparsedText == null) {
      // ORBEON: JVM only
      ???
//      val url: URL = new URL(href)
//      val connection: URLConnection = url.openConnection()
//      val stream: InputStream = connection.getInputStream
//      var builder: StringBuilder = null
//      var enc: String = encoding
//      if (enc == null) {
//        enc = StandardUnparsedTextResolver.inferStreamEncoding(stream, null)
//      }
//      builder = CatalogCollection.makeStringBuilderFromStream(stream, enc)
//      unparsedText = builder.toString
    }
    unparsedText
  }

  def getItem(context: XPathContext): Item = new StringValue(getContent)

  def getContentType: String =
    if (contentType == null) "text/plain" else contentType
}
