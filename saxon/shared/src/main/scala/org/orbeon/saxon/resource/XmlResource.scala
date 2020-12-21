package org.orbeon.saxon.resource

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.functions.URIQueryParameters
import org.orbeon.saxon.lib.ParseOptions
import org.orbeon.saxon.lib.Resource
import org.orbeon.saxon.lib.ResourceFactory
import org.orbeon.saxon.om.Item
import org.orbeon.saxon.om.NodeInfo
import org.orbeon.saxon.trans.XPathException
import javax.xml.transform.stream.StreamSource
import java.io.ByteArrayInputStream
import java.io.IOException
import java.io.InputStream
import java.io.StringReader

import org.orbeon.saxon.utils.Configuration

object XmlResource {

  val FACTORY: ResourceFactory = new ResourceFactory() {
    def makeResource(config: Configuration,
                      details: AbstractResourceCollection.InputDetails): Resource =
      new XmlResource(config, details)
  }

}

class XmlResource(private var doc: NodeInfo) extends Resource {

  private var config: Configuration = doc.getConfiguration

  private var details: AbstractResourceCollection.InputDetails = _

  def this(config: Configuration, doc: NodeInfo) = {
    this(doc)
    this.config = config
    this.doc = doc
    if (config != doc.getConfiguration) {
      throw new IllegalArgumentException(
        "Supplied node belongs to wrong configuration")
    }
  }

  def this(config: Configuration,
           details: AbstractResourceCollection.InputDetails) = {
    this(null)
    this.config = config
    this.details = details
  }

  def getResourceURI(): String =
    if (doc == null) {
      details.resourceUri
    } else {
      doc.getSystemId
    }

  def getItem(context: XPathContext): Item = {
    if (doc == null) {
      val resourceURI: String = details.resourceUri
      var options: ParseOptions = details.parseOptions
      if (options == null) {
        options = config.getParseOptions
      }
      var source: StreamSource = null
      if (details.characterContent != null) {
        source = new StreamSource(new StringReader(details.characterContent),
          resourceURI)
      } else if (details.binaryContent != null) {
        source = new StreamSource(
          new ByteArrayInputStream(details.binaryContent),
          resourceURI)
      } else {
        val stream: InputStream = details.getInputStream
        source = new StreamSource(stream, resourceURI)
      }
      try doc = config.buildDocumentTree(source, options).getRootNode
      catch {
        case e: XPathException => {
          if (details.onError == URIQueryParameters.ON_ERROR_FAIL) {
            val e2: XPathException = new XPathException(
              "collection(): failed to parse XML file " + source.getSystemId +
                ": " +
                e.getMessage,
              e.getErrorCodeLocalPart)
            throw e2
          } else if (details.onError == URIQueryParameters.ON_ERROR_WARNING) {
            context.getController.warning(
              "collection(): failed to parse XML file " + source.getSystemId +
                ": " +
                e.getMessage,
              e.getErrorCodeLocalPart,
              null)
          }
          doc = null
        }

      } finally if (source != null && source.getInputStream != null) {
        try source.getInputStream.close()
        catch {
          case e: IOException => {}

        }
      }
    }
    doc
  }

  def getContentType(): String = "application/xml"

}
