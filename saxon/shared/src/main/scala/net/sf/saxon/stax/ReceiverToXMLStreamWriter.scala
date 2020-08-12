package net.sf.saxon.stax

import net.sf.saxon.event.PipelineConfiguration
import net.sf.saxon.event.Receiver
import net.sf.saxon.model.SchemaType
import net.sf.saxon.om._
import net.sf.saxon.s9api.Location
import javax.xml.stream.XMLStreamWriter
import net.sf.saxon.utils.Configuration

import scala.jdk.CollectionConverters._

class ReceiverToXMLStreamWriter(private var writer: XMLStreamWriter)
  extends Receiver {

   var pipe: PipelineConfiguration = _

   var config: Configuration = _

   var systemId: String = _

   var baseURI: String = _

  def getXMLStreamWriter(): XMLStreamWriter = writer

  def setPipelineConfiguration(pipe: PipelineConfiguration): Unit = {
    this.pipe = pipe
    config = pipe.getConfiguration
  }

  def getPipelineConfiguration(): PipelineConfiguration = pipe

  def setSystemId(systemId: String): Unit = {
    this.systemId = systemId
  }

  def getSystemId(): String = systemId

  def open(): Unit = {}

  def startDocument(properties: Int): Unit = {
    writer.writeStartDocument()
  }

  def endDocument(): Unit = {
    writer.writeEndDocument()
  }

  def setUnparsedEntity(name: String,
                        systemID: String,
                        publicID: String): Unit = {}

  def startElement(elemName: NodeName,
                   `type`: SchemaType,
                   attributes: AttributeMap,
                   namespaces: NamespaceMap,
                   location: Location,
                   properties: Int): Unit = {
    val local: String = elemName.getLocalPart
    val uri: String = elemName.getURI
    val prefix: String = elemName.getPrefix
    if (prefix.==("") && uri.==("")) {
      writer.writeStartElement(local)
    } else if (prefix.==("")) {
      writer.writeStartElement(prefix, local, uri)
    } else {
      writer.writeStartElement(prefix, local, uri)
    }
    for (ns <- namespaces.asScala) {
      writer.writeNamespace(ns.getPrefix, ns.getURI)
    }
    for (att <- attributes) {
      val attName: NodeName = att.getNodeName
      val attLocal: String = attName.getLocalPart
      val attUri: String = attName.getURI
      val attPrefix: String = attName.getPrefix
      val value: String = att.getValue
      if (attPrefix.==("") && attUri.==("")) {
        writer.writeAttribute(attLocal, value)
      } else if (attPrefix.==("") & attUri.!=("")) {
        writer.writeAttribute(uri, local, value)
      } else {
        writer.writeAttribute(prefix, uri, local, value)
      }
    }
  }

  def endElement(): Unit = {
    writer.writeEndElement()
  }

  def characters(chars: CharSequence,
                 locationId: Location,
                 properties: Int): Unit = {
    writer.writeCharacters(chars.toString)
  }

  def processingInstruction(name: String,
                            data: CharSequence,
                            locationId: Location,
                            properties: Int): Unit = {
    writer.writeProcessingInstruction(name, data.toString)
  }

  def comment(content: CharSequence,
              locationId: Location,
              properties: Int): Unit = {
    writer.writeComment(content.toString)
  }

  def close(): Unit = {
    writer.close()
  }

  override def usesTypeAnnotations(): Boolean = false

}
