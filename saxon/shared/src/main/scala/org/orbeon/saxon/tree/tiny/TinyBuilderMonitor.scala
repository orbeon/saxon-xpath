package org.orbeon.saxon.tree.tiny

import org.orbeon.saxon.event.BuilderMonitor

import org.orbeon.saxon.model.SchemaType

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.om.AttributeMap

import org.orbeon.saxon.om.NamespaceMap

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.om.NodeName

import org.orbeon.saxon.s9api.Location

import org.orbeon.saxon.trans.XPathException


class TinyBuilderMonitor(private var builder: TinyBuilder)
  extends BuilderMonitor(builder) {

  private var mark: Int = -1

  private var markedNodeNr: Int = -1

  private var markedAttribute: Int = -1

  private var markedNamespace: Int = -1

  def markNextNode(nodeKind: Int): Unit = {
    mark = nodeKind
  }

  override def startDocument(properties: Int): Unit = {
    if (mark == Type.DOCUMENT) {
      markedNodeNr = builder.getTree.getNumberOfNodes
    }
    mark = -1
    super.startDocument(properties)
  }

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    if (mark == Type.ELEMENT) {
      markedNodeNr = builder.getTree.getNumberOfNodes
    }
    mark = -1
    super.startElement(elemName,
      `type`,
      attributes,
      namespaces,
      location,
      properties)
  }

  override def characters(chars: CharSequence,
                          locationId: Location,
                          properties: Int): Unit = {
    if (mark == Type.TEXT) {
      markedNodeNr = builder.getTree.getNumberOfNodes
    }
    mark = -1
    super.characters(chars, locationId, properties)
  }

  override def comment(chars: CharSequence,
                       locationId: Location,
                       properties: Int): Unit = {
    if (mark == Type.COMMENT) {
      markedNodeNr = builder.getTree.getNumberOfNodes
    }
    mark = -1
    super.comment(chars, locationId, properties)
  }

  override def processingInstruction(target: String,
                                     data: CharSequence,
                                     locationId: Location,
                                     properties: Int): Unit = {
    if (mark == Type.PROCESSING_INSTRUCTION) {
      markedNodeNr = builder.getTree.getNumberOfNodes
    }
    mark = -1
    super.processingInstruction(target, data, locationId, properties)
  }

  def getMarkedNode(): NodeInfo =
    if (markedNodeNr != -1) {
      builder.getTree.getNode(markedNodeNr)
    } else if (markedAttribute != -1) {
      builder.getTree.getAttributeNode(markedAttribute)
    } else {
      null
    }

}