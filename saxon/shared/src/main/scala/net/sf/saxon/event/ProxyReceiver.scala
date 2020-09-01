package net.sf.saxon.event

import net.sf.saxon.model.SchemaType

import net.sf.saxon.om._

import net.sf.saxon.s9api.Location

class ProxyReceiver(receiver: Receiver) extends SequenceReceiver(receiver.getPipelineConfiguration) {
  var nextReceiver = receiver

  setPipelineConfiguration(nextReceiver.getPipelineConfiguration)

  def setUnderlyingReceiver(receiver: Receiver) = nextReceiver = receiver

 override def setSystemId(systemId: String): Unit = {
    if (systemId != this.systemId) {
      this.systemId = systemId
      nextReceiver.setSystemId(systemId)
    }
  }

  def getNextReceiver: Receiver = nextReceiver

  override def setPipelineConfiguration(pipe: PipelineConfiguration): Unit = {
    if (pipelineConfiguration != pipe) {
      pipelineConfiguration = pipe
      if (nextReceiver.getPipelineConfiguration != pipe) {
        nextReceiver.setPipelineConfiguration(pipe)
      }
    }
  }

  override def getNamePool(): NamePool =
    pipelineConfiguration.getConfiguration.getNamePool

  override def open(): Unit = {
    nextReceiver.open()
  }

  def close(): Unit = {
    nextReceiver.close()
  }

  def startDocument(properties: Int): Unit = {
    nextReceiver.startDocument(properties)
  }

  def endDocument(): Unit = {
    nextReceiver.endDocument()
  }

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    nextReceiver.startElement(elemName,
      `type`,
      attributes,
      namespaces,
      location,
      properties)
  }

  def endElement(): Unit = {
    nextReceiver.endElement()
  }

  def characters(chars: CharSequence,
                 locationId: Location,
                 properties: Int): Unit = {
    nextReceiver.characters(chars, locationId, properties)
  }

  def processingInstruction(target: String,
                            data: CharSequence,
                            locationId: Location,
                            properties: Int): Unit = {
    nextReceiver.processingInstruction(target, data, locationId, properties)
  }

  def comment(chars: CharSequence,
              locationId: Location,
              properties: Int): Unit = {
    nextReceiver.comment(chars, locationId, properties)
  }

  override def setUnparsedEntity(name: String, uri: String, publicId: String): Unit = {
    nextReceiver.setUnparsedEntity(name, uri, publicId)
  }

  override def append(item: Item, locationId: Location, properties: Int): Unit = {
    nextReceiver.append(item, locationId, properties)
  }

  override def usesTypeAnnotations(): Boolean = nextReceiver.usesTypeAnnotations()

}
