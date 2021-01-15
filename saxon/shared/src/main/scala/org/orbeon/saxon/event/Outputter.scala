package org.orbeon.saxon.event

import org.orbeon.saxon.expr.parser.Loc
import org.orbeon.saxon.model.{SchemaType, SimpleType}
import org.orbeon.saxon.om._
import org.orbeon.saxon.s9api.Location
import org.orbeon.saxon.tree.util.{CharSequenceConsumer, FastStringBuffer}
import org.orbeon.saxon.utils.Configuration
import org.orbeon.saxon.value.StringValue
//import scala.collection.compat._
import scala.jdk.CollectionConverters._


abstract class Outputter extends Receiver {

   var pipelineConfiguration: PipelineConfiguration = _
   var systemId: String = null

  def setPipelineConfiguration(pipe: PipelineConfiguration): Unit =
    this.pipelineConfiguration = pipe

  def getPipelineConfiguration: PipelineConfiguration = pipelineConfiguration

  def getConfiguration: Configuration =
    pipelineConfiguration.getConfiguration

  def setSystemId(systemId: String): Unit =
    this.systemId = systemId

  def getSystemId: String = systemId

  def open(): Unit = ()

  def startDocument(properties: Int): Unit

  def endDocument(): Unit

  def setUnparsedEntity(
    name     : String,
    systemID : String,
    publicID : String
  ): Unit = ()

  def startElement(
    elemName   : NodeName,
    typeCode   : SchemaType,
    location   : Location,
    properties : Int
  ): Unit

  override def startElement(
    elemName   : NodeName,
    `type`     : SchemaType,
    attributes : AttributeMap,
    namespaces : NamespaceMap,
    location   : Location,
    properties : Int
  ): Unit = {
    startElement(elemName, `type`, location, properties)
    for (ns <- namespaces.asScala)
      namespace(ns.getPrefix, ns.getURI, properties)
    for (att <- attributes.iterator.asScala)
      attribute(
        att.getNodeName,
        att.getType,
        att.getValue,
        att.getLocation,
        att.getProperties
      )
    startContent()
  }

  def namespace(prefix: String, namespaceUri: String, properties: Int): Unit

  def namespaces(bindings: NamespaceBindingSet, properties: Int): Unit =
    for (nb <- bindings.asScala)
      namespace(nb.getPrefix, nb.getURI, properties)

  def attribute(
    attName    : NodeName,
    typeCode   : SimpleType,
    value      : CharSequence,
    location   : Location,
    properties : Int
  ): Unit

  def startContent(): Unit = ()

  def endElement(): Unit

  def characters(
    chars      : CharSequence,
    location   : Location,
    properties : Int
  ): Unit

  def processingInstruction(
    name       : String,
    data       : CharSequence,
    location   : Location,
    properties : Int
  ): Unit

  def comment(content: CharSequence, location: Location, properties: Int): Unit

  override def append(item: Item, locationId: Location, properties: Int): Unit =
    throw new UnsupportedOperationException

  override def append(item: Item): Unit =
    append(item, Loc.NONE, ReceiverOption.ALL_NAMESPACES)

  def getStringReceiver(asTextNode: Boolean): CharSequenceConsumer =
    new CharSequenceConsumer() {
      val buffer = new FastStringBuffer(256)

      def cat(chars: CharSequence): CharSequenceConsumer =
        buffer.cat(chars)

      override def cat(c: Char): CharSequenceConsumer = buffer.cat(c)

      override def close(): Unit =
        if (asTextNode)
          Outputter.this.characters(buffer, Loc.NONE, ReceiverOption.NONE)
        else
          Outputter.this.append(new StringValue(buffer.condense()))
    }

  def close(): Unit = ()

  override def usesTypeAnnotations: Boolean = false
}
