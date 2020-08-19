////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.resource

import BinaryResource._
import net.sf.saxon.expr.XPathContext

import net.sf.saxon.lib.Resource

import net.sf.saxon.lib.ResourceFactory

import net.sf.saxon.om.Item

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.Base64BinaryValue

import java.io.BufferedInputStream

import java.io.ByteArrayOutputStream

import java.io.IOException

import java.io.InputStream

import java.net.URI

import java.net.URISyntaxException

import java.net.URL

import java.net.URLConnection

import BinaryResource._

import scala.beans.{BeanProperty, BooleanBeanProperty}
import scala.util.control.Breaks._

object BinaryResource {

  val FACTORY: ResourceFactory = (config, details) =>
    new BinaryResource(details)

  def readBinaryFromStream(in: InputStream, path: String): Array[Byte] = {
    val buffer: ByteArrayOutputStream = new ByteArrayOutputStream()
    var nRead: Int = 0
    val data: Array[Byte] = Array.ofDim[Byte](16384)
    while ((nRead = in.read(data, 0, data.length)) != -1) buffer.write(data,
      0,
      nRead)
    buffer.flush()
    buffer.toByteArray()
  }

}

class BinaryResource(in: AbstractResourceCollection.InputDetails)
  extends Resource {

  private var href: String = in.resourceUri

  @BeanProperty
  var contentType: String = in.contentType

  @BeanProperty
  var data: Array[Byte] = in.binaryContent

  private var connection: URLConnection = null

  def this(href: String, contentType: String, content: Array[Byte]) = {
    this(null)
    this.contentType = contentType
    this.href = href
    this.data = content
  }

  def getResourceURI(): String = href

  private def readBinaryFromConn(con: URLConnection): Array[Byte] = {
    var raw: InputStream = null
    this.connection = con
    raw = connection.getInputStream
    val contentLength: Int = connection.getContentLength
    val in: InputStream = new BufferedInputStream(raw)
    if (contentLength < 0) {
      // bug 4475
      val result: Array[Byte] =
        readBinaryFromStream(in, connection.getURL.getPath)
      in.close()
      result
    } else {
      val data: Array[Byte] = Array.ofDim[Byte](contentLength)
      var bytesRead: Int = 0
      var offset: Int = 0
      breakable {
        while (offset < contentLength) {
          bytesRead = in.read(data, offset, data.length - offset)
          if (bytesRead == -1) {
            break
          }
          offset += bytesRead
        }
      }
      in.close()
      if (offset != contentLength) {
        throw new XPathException(
          "Only read " + offset + " bytes; Expected " + contentLength +
            " bytes")
      }
      data
    }
  }

  def getItem(context: XPathContext): Item =
    if (data != null) {
      new Base64BinaryValue(data)
    } else if (connection != null) {
      data = readBinaryFromConn(connection)
      new Base64BinaryValue(data)
    } else {
      val url: URL = new URI(href).toURL()
      connection = url.openConnection()
      data = readBinaryFromConn(connection)
      new Base64BinaryValue(data)
    }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
