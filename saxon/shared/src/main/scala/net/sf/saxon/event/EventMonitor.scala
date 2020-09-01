////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.event

import net.sf.saxon.model.SchemaType

import net.sf.saxon.model.SimpleType

import net.sf.saxon.om.Item

import net.sf.saxon.om.NodeName

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException




class EventMonitor(private var next: Outputter) extends Outputter {

  private var written: Boolean = false

  override def startDocument(properties: Int): Unit = {
    written = true
    next.startDocument(properties)
  }

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            location: Location,
                            properties: Int): Unit = {
    written = true
    next.startElement(elemName, `type`, location, properties)
  }

  override def endElement(): Unit = {
    written = true
    next.endElement()
  }

  override def attribute(name: NodeName,
                         `type`: SimpleType,
                         value: CharSequence,
                         location: Location,
                         properties: Int): Unit = {
    written = true
    next.attribute(name, `type`, value, location, properties)
  }

  override def namespace(prefix: String, uri: String, properties: Int): Unit = {
    written = true
    next.namespace(prefix, uri, properties)
  }

  override def startContent(): Unit = {
    written = true
    next.startContent()
  }

  override def characters(chars: CharSequence,
                          location: Location,
                          properties: Int): Unit = {
    written = true
    next.characters(chars, location, properties)
  }

  override def processingInstruction(name: String,
                                     data: CharSequence,
                                     location: Location,
                                     properties: Int): Unit = {
    written = true
    next.processingInstruction(name, data, location, properties)
  }

  override def comment(content: CharSequence,
                       location: Location,
                       properties: Int): Unit = {
    written = true
    next.comment(content, location, properties)
  }

  override def append(item: Item, location: Location, properties: Int): Unit = {
    written = true
    next.append(item, location, properties)
  }

  override def endDocument(): Unit = {
    written = true
    next.endDocument()
  }

  def hasBeenWrittenTo: Boolean = written

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * An <tt>EventMonitor</tt> is a filter that passes all events down the pipeline unchanged,
  * keeping a note of whether any data has passed through the filter. At any stage it is possible
  * to ask whether any data has been written.
  * @since 9.9
  */
