////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.resource

import org.orbeon.saxon.functions.URIQueryParameters

import org.orbeon.saxon.lib.ParseOptions

import org.orbeon.saxon.lib.Resource

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.Base64BinaryValue

import java.net.URI

import java.nio.charset.StandardCharsets




object DataURIScheme {

  def decode(uri: URI): Resource = {
    assert(uri.getScheme.==("data"))
    val path: String = uri.getSchemeSpecificPart
    val comma: Int = path.indexOf(',')
    if (comma < 0) {
      throw new IllegalArgumentException("Missing comma in data URI")
    }
    val header: String = path.substring(0, comma)
    val content: String = path.substring(comma + 1)
    val isBase64: Boolean = header.endsWith(";base64")
    val contentType: String =
      header.substring(0, if (isBase64) comma - 7 else comma)
    if (isBase64) {
      val octets: Array[Byte] = Base64BinaryValue.decode(content)
      val resource: BinaryResource =
        new BinaryResource(uri.toString, contentType, octets)
      resource.setData(octets)
      resource
    } else {
      var encoding: String = getEncoding(contentType)
      if (encoding == null) {
        encoding = "US-ASCII"
      }
      val utf8content: Array[Byte] = content.getBytes(StandardCharsets.UTF_8)
      val details: AbstractResourceCollection.InputDetails =
        new AbstractResourceCollection.InputDetails()
      details.resourceUri = uri.toString
      details.contentType = getMediaType(contentType)
      details.encoding = encoding
      details.binaryContent = utf8content
      details.onError = URIQueryParameters.ON_ERROR_FAIL
      details.parseOptions = new ParseOptions()
      new UnparsedTextResource(details)
    }
  }

  private def getMediaType(contentType: String): String = {
    val semicolon: Int = contentType.indexOf(';')
    if (semicolon < 0) {
      contentType
    } else {
      contentType.substring(0, semicolon)
    }
  }

  private def getEncoding(contentType: String): String = {
    val parts: Array[String] = contentType.split(";")
    for (part <- parts if part.startsWith("charset=")) {
      part.substring(8)
    }
    null
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class handles URIs using the data: URI scheme defined in RFC 2397
  */
