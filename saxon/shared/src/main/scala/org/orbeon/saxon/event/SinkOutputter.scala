package org.orbeon.saxon.event

import org.orbeon.saxon.model.SchemaType

import org.orbeon.saxon.model.SimpleType

import org.orbeon.saxon.om.NodeName

import org.orbeon.saxon.s9api.Location

import org.orbeon.saxon.trans.XPathException




class SinkOutputter extends Outputter {

  override def startDocument(properties: Int): Unit = ()

  override def endDocument(): Unit = ()

  override def startElement(elemName: NodeName,
                            typeCode: SchemaType,
                            location: Location,
                            properties: Int): Unit = ()

  override def namespace(prefix: String,
                         namespaceUri: String,
                         properties: Int): Unit = ()

  override def attribute(attName: NodeName,
                         typeCode: SimpleType,
                         value: CharSequence,
                         location: Location,
                         properties: Int): Unit = ()

  override def endElement(): Unit = ()

  override def characters(chars: CharSequence,
                          location: Location,
                          properties: Int): Unit = ()

  override def processingInstruction(name: String,
                                     data: CharSequence,
                                     location: Location,
                                     properties: Int): Unit = ()

  override def comment(content: CharSequence,
                       location: Location,
                       properties: Int): Unit = ()

}

/**
  * An Outputter that swallows (discards) all input supplied to it
  */
// Copyright (c) 2009-2020 Saxonica Limited
