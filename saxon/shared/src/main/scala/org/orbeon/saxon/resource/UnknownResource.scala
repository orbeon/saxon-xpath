package org.orbeon.saxon.resource

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.lib.Resource
import org.orbeon.saxon.lib.ResourceFactory
import org.orbeon.saxon.om.Item
import org.orbeon.saxon.trans.XPathException
import java.io.BufferedInputStream
import java.io.ByteArrayInputStream
import java.io.IOException
import java.io.InputStream
import java.net.URLConnection

import org.orbeon.saxon.utils.Configuration

object UnknownResource {
  val FACTORY: ResourceFactory = (_, _) => new UnknownResource
}

class UnknownResource extends Resource {

  var config: Configuration = _
  var details: AbstractResourceCollection.InputDetails = _

  def this(config: Configuration, details: AbstractResourceCollection.InputDetails) {
    this()
    this.config = config
    this.details = details
  }

  def getResourceURI(): String = details.resourceUri

  def getItem(context: XPathContext): Item = {
    var stream =
      if (details.binaryContent != null)
        new ByteArrayInputStream(details.binaryContent)
      else
        details.getInputStream
    if (stream == null) {
      throw new XPathException("Unable to dereference resource URI " + details.resourceUri)
    }
    var mediaType: String = null
    try {
      if (!stream.markSupported()) {
        stream = new BufferedInputStream(stream)
      }
      mediaType = URLConnection.guessContentTypeFromStream(stream)
    } catch {
      case e: IOException => mediaType = null

    }
    if (mediaType == null) {
      mediaType = config.getMediaTypeForFileExtension("")
    }
    if (mediaType == null || mediaType.==("application/unknown")) {
      mediaType = "application/binary"
    }
    details.contentType = mediaType
    details.binaryContent =
      BinaryResource.readBinaryFromStream(stream, details.resourceUri)
    val delegee =
      config.getResourceFactoryForMediaType(mediaType)
    val actual = delegee.makeResource(config, details)
    actual.getItem(context)
  }

  def getContentType(): String = "application/xml"

}