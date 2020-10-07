package org.orbeon.saxon.serialize

import org.orbeon.saxon.event.EventBuffer

import org.orbeon.saxon.event.ProxyReceiver

import org.orbeon.saxon.event.Receiver

import org.orbeon.saxon.event.ReceiverOption

import org.orbeon.saxon.lib.NamespaceConstant

import org.orbeon.saxon.lib.SaxonOutputKeys

import org.orbeon.saxon.lib.SerializerFactory

import org.orbeon.saxon.model.SchemaType

import org.orbeon.saxon.om._

import org.orbeon.saxon.s9api.Location

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.Whitespace

import javax.xml.transform.OutputKeys

import javax.xml.transform.Result

import java.util.Properties

class UncommittedSerializer(private var finalResult: Result,
                            next: Receiver,
                            params: SerializationProperties)
  extends ProxyReceiver(next) {

  private var committed: Boolean = false

  private var pending: EventBuffer = null

  private var properties: SerializationProperties = params

  override def open(): Unit = {
    committed = false
  }

  override def close(): Unit = {
    if (!committed) {
      switchToMethod("xml")
    }
    getNextReceiver.close()
  }

  override def characters(chars: CharSequence,
                          locationId: Location,
                          properties: Int): Unit = {
    if (committed) {
      getNextReceiver.characters(chars, locationId, properties)
    } else {
      if (pending == null) {
        pending = new EventBuffer(getPipelineConfiguration)
      }
      pending.characters(chars, locationId, properties)
      if (!Whitespace.isWhite(chars)) {
        switchToMethod("xml")
      }
    }
  }

  override def processingInstruction(target: String,
                                     data: CharSequence,
                                     locationId: Location,
                                     properties: Int): Unit = {
    if (committed) {
      getNextReceiver.processingInstruction(target,
        data,
        locationId,
        properties)
    } else {
      if (pending == null) {
        pending = new EventBuffer(getPipelineConfiguration)
      }
      pending.processingInstruction(target, data, locationId, properties)
    }
  }

  override def comment(chars: CharSequence,
                       locationId: Location,
                       properties: Int): Unit = {
    if (committed) {
      getNextReceiver.comment(chars, locationId, properties)
    } else {
      if (pending == null) {
        pending = new EventBuffer(getPipelineConfiguration)
      }
      pending.comment(chars, locationId, properties)
    }
  }

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    if (!committed) {
      val name: String = elemName.getLocalPart
      val uri: String = elemName.getURI
      if (name.equalsIgnoreCase("html") && uri.isEmpty) {
        switchToMethod("html")
      } else if (name.==("html") && uri == NamespaceConstant.XHTML) {
        val version: String = this.properties.getProperties
          .getProperty(SaxonOutputKeys.STYLESHEET_VERSION)
        if ("10" == version) {
          switchToMethod("xml")
        } else {
          switchToMethod("xhtml")
        }
      } else {
        switchToMethod("xml")
      }
    }
    getNextReceiver.startElement(elemName,
      `type`,
      attributes,
      namespaces,
      location,
      properties)
  }

  override def startDocument(properties: Int): Unit = {
    if (committed) {
      getNextReceiver.startDocument(properties)
    } else {
      if (pending == null) {
        pending = new EventBuffer(getPipelineConfiguration)
      }
      pending.startDocument(properties)
    }
  }

  override def endDocument(): Unit = {
    if (!committed) {
      switchToMethod("xml")
    }
    getNextReceiver.endDocument()
  }

  private def switchToMethod(method: String): Unit = {
    val newProperties: Properties = new Properties(properties.getProperties)
    newProperties.setProperty(OutputKeys.METHOD, method)
    val sf: SerializerFactory = getConfiguration.getSerializerFactory
    val newParams: SerializationProperties = new SerializationProperties(
      newProperties,
      properties.getCharacterMapIndex)
    newParams.setValidationFactory(properties.getValidationFactory)
    val target: Receiver =
      sf.getReceiver(finalResult, newParams, getPipelineConfiguration)
    committed = true
    target.open()
    if (pending != null) {
      pending.replay(target)
      pending = null
    }
    this.setUnderlyingReceiver(target)
  }

  override def append(item: Item,
                      locationId: Location,
                      copyNamespaces: Int): Unit = {
    if (item.isInstanceOf[NodeInfo]) {
      item
        .asInstanceOf[NodeInfo]
        .copy(this, CopyOptions.ALL_NAMESPACES, locationId)
    } else {
      if (!committed) {
        switchToMethod("xml")
      }
      getNextReceiver.append(item)
    }
  }

}
