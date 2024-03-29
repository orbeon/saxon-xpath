package org.orbeon.saxon.event

import org.orbeon.saxon.model.SchemaType

import org.orbeon.saxon.om.AttributeMap

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.NamespaceMap

import org.orbeon.saxon.om.NodeName

import org.orbeon.saxon.s9api.Location

//import scala.collection.compat._
import scala.jdk.CollectionConverters._

import java.util.ArrayList

import java.util.List

class EventBuffer(pipe: PipelineConfiguration) extends SequenceReceiver(pipe) {

  private var buffer: List[Event] = new ArrayList()

  override def startDocument(properties: Int): Unit = {
    buffer.add(new Event.StartDocument(properties))
  }

  override def endDocument(): Unit = {
    buffer.add(new Event.EndDocument())
  }

  override def startElement(elemName: NodeName,
                            typeCode: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    buffer.add(
      new Event.StartElement(elemName,
        typeCode,
        attributes,
        namespaces,
        location,
        properties))
  }

  override def endElement(): Unit = {
    buffer.add(new Event.EndElement())
  }

  override def characters(chars: CharSequence,
                          location: Location,
                          properties: Int): Unit = {
    buffer.add(new Event.Text(chars, location, properties))
  }

  override def processingInstruction(name: String,
                                     data: CharSequence,
                                     location: Location,
                                     properties: Int): Unit = {
    buffer.add(
      new Event.ProcessingInstruction(name, data, location, properties))
  }

  override def comment(content: CharSequence,
                       location: Location,
                       properties: Int): Unit = {
    buffer.add(new Event.Comment(content, location, properties))
  }

  override def append(item: Item, location: Location, properties: Int): Unit = {
    buffer.add(new Event.Append(item, location, properties))
  }

  override def close(): Unit = ()

  def replay(out: Receiver): Unit = {
    for (event <- buffer.asScala) {
      event.replay(out)
    }
  }

}
