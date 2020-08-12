package net.sf.saxon.serialize

import net.sf.saxon.event.ProxyReceiver
import net.sf.saxon.event.Receiver
import net.sf.saxon.model.SchemaType
import net.sf.saxon.om.AttributeMap
import net.sf.saxon.om.Item
import net.sf.saxon.om.NamespaceMap
import net.sf.saxon.om.NodeName
import net.sf.saxon.s9api.Destination
import net.sf.saxon.s9api.Location
import net.sf.saxon.s9api.SaxonApiException
import net.sf.saxon.trans.XPathException
import net.sf.saxon.trans.XsltController
import net.sf.saxon.utils.Controller

class PrincipalOutputGatekeeper(private var controller: XsltController,
                                next: Receiver) extends ProxyReceiver(next) {

  private var usedAsPrimaryResult: Boolean = false

  private var usedAsSecondaryResult: Boolean = false

  private var isOpen: Boolean = false

  private var closed: Boolean = false

  override def open(): Unit = {
    if (closed) {
      val uri: String =
        if (getSystemId == Controller.ANONYMOUS_PRINCIPAL_OUTPUT_URI)
          "(no URI supplied)"
        else getSystemId
      val err: XPathException = new XPathException(
        "Cannot write more than one result document to the principal output destination: " +
          uri)
      err.setErrorCode("XTDE1490")
      throw err
    }
    super.open()
    isOpen = true
  }

  override def startDocument(properties: Int): Unit = {
    synchronized {
      if (!isOpen) {
        open()
      }
      nextReceiver.startDocument(properties)
    }
  }

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    useAsPrimary()
    nextReceiver.startElement(elemName,
      `type`,
      attributes,
      namespaces,
      location,
      properties)
  }

  override def characters(chars: CharSequence,
                          locationId: Location,
                          properties: Int): Unit = {
    synchronized {
      useAsPrimary()
      nextReceiver.characters(chars, locationId, properties)
    }
  }

  override def processingInstruction(target: String,
                                     data: CharSequence,
                                     locationId: Location,
                                     properties: Int): Unit = {
    useAsPrimary()
    nextReceiver.processingInstruction(target, data, locationId, properties)
  }

  override def comment(chars: CharSequence,
                       locationId: Location,
                       properties: Int): Unit = {
    useAsPrimary()
    nextReceiver.comment(chars, locationId, properties)
  }

  override def append(item: Item,
                      locationId: Location,
                      copyNamespaces: Int): Unit = {
    useAsPrimary()
    nextReceiver.append(item, locationId, copyNamespaces)
  }

  private def useAsPrimary(): Unit = {
    synchronized {
      if (closed) {
        val err: XPathException = new XPathException(
          "Cannot write to the principal output destination as it has already been closed: " +
            identifySystemId())
        err.setErrorCode("XTDE1490")
        throw err
      }
      if (usedAsSecondaryResult) {
        val err: XPathException = new XPathException(
          "Cannot write to the principal output destination as it has already been used by xsl:result-document: " +
            identifySystemId())
        err.setErrorCode("XTDE1490")
        throw err
      }
      usedAsPrimaryResult = true
    }
  }

  def useAsSecondary(): Unit = {
    synchronized {
      if (usedAsPrimaryResult) {
        val err: XPathException = new XPathException(
          "Cannot use xsl:result-document to write to a destination already used for the principal output: " +
            identifySystemId())
        err.setErrorCode("XTDE1490")
        throw err
      }
      if (usedAsSecondaryResult) {
        val err: XPathException = new XPathException(
          "Cannot write more than one xsl:result-document to the principal output destination: " +
            identifySystemId())
        err.setErrorCode("XTDE1490")
        throw err
      }
      usedAsSecondaryResult = true
    }
  }

  def makeReceiver(params: SerializationProperties): Receiver = {
    try {
      val dest: Destination = controller.getPrincipalDestination
      if (dest != null) {
        dest.getReceiver(controller.makePipelineConfiguration, params)
      }
    } catch {
      case e: SaxonApiException => null

    }
    null
  }

  private def identifySystemId(): String = {
    val uri: String = getSystemId
    if (uri == null) "(no URI supplied)" else uri
  }

  override def close(): Unit = {
    closed = true
    if (usedAsPrimaryResult) {
      nextReceiver.close()
    }
  }

}
