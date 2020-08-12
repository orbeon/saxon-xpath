package net.sf.saxon.event

import net.sf.saxon.model.SchemaType

import net.sf.saxon.om.AttributeMap

import net.sf.saxon.om.Item

import net.sf.saxon.om.NamespaceMap

import net.sf.saxon.om.NodeName

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

import java.util.Arrays

import scala.beans.{BeanProperty, BooleanBeanProperty}

class TreeReceiver(@BeanProperty var nextReceiver: Receiver) extends SequenceReceiver(nextReceiver.getPipelineConfiguration) {

  private var level: Int = 0

  private var isDocumentLevel: Array[Boolean] = new Array[Boolean](20)

  previousAtomic = false

  this.pipelineConfiguration = nextReceiver.getPipelineConfiguration

  override def setSystemId(systemId: String): Unit = {
    if (systemId != null && systemId != this.systemId) {
      this.systemId = systemId
      if (nextReceiver != null) {
        nextReceiver.setSystemId(systemId)
      }
    }
  }

  override def setPipelineConfiguration(pipe: PipelineConfiguration): Unit = {
    if (pipelineConfiguration != pipe) {
      pipelineConfiguration = pipe
      if (nextReceiver != null) {
        nextReceiver.setPipelineConfiguration(pipe)
      }
    }
  }

  override def open(): Unit = {
    if (nextReceiver == null) {
      throw new IllegalStateException(
        "TreeReceiver.open(): no underlying receiver provided")
    }
    nextReceiver.open()
    previousAtomic = false
  }

  def close(): Unit = {
    if (nextReceiver != null) {
      nextReceiver.close()
    }
    previousAtomic = false
  }

  def startDocument(properties: Int): Unit = {
    if (level == 0) {
      nextReceiver.startDocument(properties)
    }
    if (isDocumentLevel.length - 1 < level) {
      isDocumentLevel = Arrays.copyOf(isDocumentLevel, level * 2)
    }
    isDocumentLevel({ level += 1; level - 1 }) = true
  }

  def endDocument(): Unit = {
    { level -= 1; level + 1 }
    if (level == 0) {
      nextReceiver.endDocument()
    }
  }

  def startElement(elemName: NodeName,
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
    previousAtomic = false
    if (isDocumentLevel.length - 1 < level) {
      isDocumentLevel = Arrays.copyOf(isDocumentLevel, level * 2)
    }
    isDocumentLevel({ level += 1; level - 1 }) = false
  }

  def endElement(): Unit = {
    nextReceiver.endElement()
    previousAtomic = false
    level -= 1
  }

  def characters(chars: CharSequence,
                 locationId: Location,
                 properties: Int): Unit = {
    if (chars.length > 0) {
      nextReceiver.characters(chars, locationId, properties)
    }
    previousAtomic = false
  }

  def processingInstruction(target: String,
                            data: CharSequence,
                            locationId: Location,
                            properties: Int): Unit = {
    nextReceiver.processingInstruction(target, data, locationId, properties)
    previousAtomic = false
  }

  def comment(chars: CharSequence,
              locationId: Location,
              properties: Int): Unit = {
    nextReceiver.comment(chars, locationId, properties)
    previousAtomic = false
  }

  override def setUnparsedEntity(name: String, uri: String, publicId: String): Unit = {
    nextReceiver.setUnparsedEntity(name, uri, publicId)
  }

  override def append(item: Item, locationId: Location, copyNamespaces: Int): Unit = {
    decompose(item, locationId, copyNamespaces)
  }

  override def usesTypeAnnotations(): Boolean = nextReceiver.usesTypeAnnotations()

}
