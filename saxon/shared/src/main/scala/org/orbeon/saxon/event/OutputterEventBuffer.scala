package org.orbeon.saxon.event

import java.{util => ju}

import org.orbeon.saxon.model.{SchemaType, SimpleType}
import org.orbeon.saxon.om._
import org.orbeon.saxon.s9api.Location

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

class OutputterEventBuffer extends Outputter {

  private var buffer: ju.List[OutputterEvent] = new ju.ArrayList

  def setBuffer(buffer: ju.List[OutputterEvent]): Unit =
    this.buffer = buffer

  def startDocument(properties: Int): Unit =
    buffer.add(new OutputterEvent.StartDocument(properties))

  def endDocument(): Unit =
    buffer.add(new OutputterEvent.EndDocument())

  def startElement(elemName: NodeName,
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
    for (att <- attributes.iterator.asScala) {
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

  def attribute(name: NodeName,
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

  def namespace(prefix: String, uri: String, properties: Int): Unit =
    buffer.add(new OutputterEvent.Namespace(prefix, uri, properties))

  override def startContent(): Unit =
    buffer.add(new OutputterEvent.StartContent())

  def endElement(): Unit =
    buffer.add(new OutputterEvent.EndElement())

  def characters(chars: CharSequence,
                          location: Location,
                          properties: Int): Unit =
    buffer.add(new OutputterEvent.Text(chars, location, properties))

  def processingInstruction(name: String,
                                     data: CharSequence,
                                     location: Location,
                                     properties: Int): Unit = {
    buffer.add(
      new OutputterEvent.ProcessingInstruction(name,
        data,
        location,
        properties))
  }

  def comment(content: CharSequence,
                       location: Location,
                       properties: Int): Unit =
    buffer.add(new OutputterEvent.Comment(content, location, properties))

  override def append(item: Item, location: Location, properties: Int): Unit =
    buffer.add(new OutputterEvent.Append(item, location, properties))

  override def close(): Unit = ()

  def replay(out: Outputter): Unit =
    for (event <- buffer.asScala)
      event.replay(out)

  def isEmpty: Boolean = buffer.isEmpty

  def reset(): Unit =
    buffer.clear()

}
