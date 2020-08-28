////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.event

import net.sf.saxon.model.SchemaType

import net.sf.saxon.om.AttributeMap

import net.sf.saxon.om.Item

import net.sf.saxon.om.NamespaceMap

import net.sf.saxon.om.NodeName

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException




class Sink(pipe: PipelineConfiguration) extends SequenceReceiver(pipe) {

  override def open(): Unit = ()

  def close(): Unit = ()

  def startDocument(properties: Int): Unit = ()

  def endDocument(): Unit = ()

  def startElement(elemName: NodeName,
                   `type`: SchemaType,
                   attributes: AttributeMap,
                   namespaces: NamespaceMap,
                   location: Location,
                   properties: Int): Unit = ()

  def endElement(): Unit = ()

  def characters(chars: CharSequence,
                 locationId: Location,
                 properties: Int): Unit = ()

  def processingInstruction(target: String,
                            data: CharSequence,
                            locationId: Location,
                            properties: Int): Unit = ()

  def comment(chars: CharSequence,
              locationId: Location,
              properties: Int): Unit = ()

  override def append(item: Item, locationId: Location, copyNamespaces: Int): Unit = ()

  override def setUnparsedEntity(name: String, uri: String, publicId: String): Unit = ()

  override def usesTypeAnnotations(): Boolean = false

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A Sink is a Receiver that discards all information passed to it
  */
