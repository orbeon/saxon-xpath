package net.sf.saxon.event

import net.sf.saxon.model.SchemaType

import net.sf.saxon.model.SimpleType

import net.sf.saxon.om._

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

import java.util.ArrayList

import java.util.List

import scala.jdk.CollectionConverters._

class OutputterEventBuffer extends Outputter {

  private var buffer: List[OutputterEvent] = new ArrayList()

  def setBuffer(buffer: List[OutputterEvent]): Unit = {
    this.buffer = buffer
  }

  override def startDocument(properties: Int): Unit = {
    buffer.add(new OutputterEvent.StartDocument(properties))
  }

  override def endDocument(): Unit = {
    buffer.add(new OutputterEvent.EndDocument())
  }

  override def startElement(elemName: NodeName,
                            typeCode: SchemaType,
                            location: Location,
                            properties: Int): Unit = {
    buffer.add(
      new OutputterEvent.StartElement(elemName,
        typeCode,
        location,
        properties))
  }

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    buffer.add(
      new OutputterEvent.StartElement(elemName, `type`, location, properties))
    for (att <- attributes) {
      buffer.add(
        new OutputterEvent.Attribute(att.getNodeName,
          att.getType,
          att.getValue,
          att.getLocation,
          att.getProperties))
    }
    for (binding <- namespaces.asScala) {
      buffer.add(
        new OutputterEvent.Namespace(binding.getPrefix,
          binding.getURI,
          properties))
    }
    buffer.add(new OutputterEvent.StartContent())
  }

  override def attribute(name: NodeName,
                         `type`: SimpleType,
                         value: CharSequence,
                         location: Location,
                         properties: Int): Unit = {
    buffer.add(
      new OutputterEvent.Attribute(name,
        `type`,
        value.toString,
        location,
        properties))
  }

  override def namespace(prefix: String, uri: String, properties: Int): Unit = {
    buffer.add(new OutputterEvent.Namespace(prefix, uri, properties))
  }

  override def startContent(): Unit = {
    buffer.add(new OutputterEvent.StartContent())
  }

  override def endElement(): Unit = {
    buffer.add(new OutputterEvent.EndElement())
  }

  override def characters(chars: CharSequence,
                          location: Location,
                          properties: Int): Unit = {
    buffer.add(new OutputterEvent.Text(chars, location, properties))
  }

  override def processingInstruction(name: String,
                                     data: CharSequence,
                                     location: Location,
                                     properties: Int): Unit = {
    buffer.add(
      new OutputterEvent.ProcessingInstruction(name,
        data,
        location,
        properties))
  }

  override def comment(content: CharSequence,
                       location: Location,
                       properties: Int): Unit = {
    buffer.add(new OutputterEvent.Comment(content, location, properties))
  }

  override def append(item: Item, location: Location, properties: Int): Unit = {
    buffer.add(new OutputterEvent.Append(item, location, properties))
  }

  override def close(): Unit = {}

  def replay(out: Outputter): Unit = {
    for (event <- buffer.asScala) {
      event.replay(out)
    }
  }

  def isEmpty(): Boolean = buffer.isEmpty

  def reset(): Unit = {
    buffer.clear()
  }

}
