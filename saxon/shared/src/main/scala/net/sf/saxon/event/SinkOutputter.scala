package net.sf.saxon.event

import net.sf.saxon.model.SchemaType

import net.sf.saxon.model.SimpleType

import net.sf.saxon.om.NodeName

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException




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
