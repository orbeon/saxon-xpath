package net.sf.saxon.resource

import net.sf.saxon.expr.XPathContext
import net.sf.saxon.lib.Resource
import net.sf.saxon.lib.ResourceFactory
import net.sf.saxon.om.Item
import net.sf.saxon.trans.XPathException
import java.io.BufferedInputStream
import java.io.ByteArrayInputStream
import java.io.IOException
import java.io.InputStream
import java.net.URLConnection

import net.sf.saxon.utils.Configuration

object UnknownResource {

  val FACTORY: ResourceFactory = new UnknownResource().asInstanceOf[ResourceFactory]

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
    var stream: InputStream = null
    stream =
      if (details.binaryContent != null)
        new ByteArrayInputStream(details.binaryContent)
      else details.getInputStream
    if (stream == null) {
      throw new XPathException(
        "Unable to dereference resource URI " + details.resourceUri)
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
    val delegee: ResourceFactory =
      config.getResourceFactoryForMediaType(mediaType)
    val actual: Resource = delegee.makeResource(config, details)
    actual.getItem(context)
  }

  def getContentType(): String = "application/xml"

}
