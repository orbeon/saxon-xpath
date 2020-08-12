////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.event

import net.sf.saxon.model.SchemaType

import net.sf.saxon.om.AttributeMap

import net.sf.saxon.om.Item

import net.sf.saxon.om.NamespaceMap

import net.sf.saxon.om.NodeName

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException




class TeeOutputter(private var seq1: Receiver, private var seq2: Receiver)
    extends SequenceReceiver(seq1.getPipelineConfiguration) {

   def setFirstDestination(seq1: Receiver): Unit = {
    this.seq1 = seq1
  }

   def setSecondDestination(seq2: Receiver): Unit = {
    this.seq2 = seq2
  }

   def getFirstDestination(): Receiver = seq1

   def getSecondDestination(): Receiver = seq2

  override def setUnparsedEntity(name: String,
                                 systemID: String,
                                 publicID: String): Unit = {
    seq1.setUnparsedEntity(name, systemID, publicID)
    seq2.setUnparsedEntity(name, systemID, publicID)
  }

  override def append(item: Item, locationId: Location, properties: Int): Unit = {
    seq1.append(item, locationId, properties)
    seq2.append(item, locationId, properties)
  }

  override def open(): Unit = {
    super.open()
    seq1.open()
    seq2.open()
  }

  def startDocument(properties: Int): Unit = {
    seq1.startDocument(properties)
    seq2.startDocument(properties)
  }

  def endDocument(): Unit = {
    seq1.endDocument()
    seq2.endDocument()
  }

  def startElement(elemName: NodeName,
                   `type`: SchemaType,
                   attributes: AttributeMap,
                   namespaces: NamespaceMap,
                   location: Location,
                   properties: Int): Unit = {
    seq1.startElement(elemName,
                      `type`,
                      attributes,
                      namespaces,
                      location,
                      properties)
    seq2.startElement(elemName,
                      `type`,
                      attributes,
                      namespaces,
                      location,
                      properties)
  }

  def endElement(): Unit = {
    seq1.endElement()
    seq2.endElement()
  }

  def characters(chars: CharSequence,
                 locationId: Location,
                 properties: Int): Unit = {
    seq1.characters(chars, locationId, properties)
    seq2.characters(chars, locationId, properties)
  }

  def processingInstruction(name: String,
                            data: CharSequence,
                            locationId: Location,
                            properties: Int): Unit = {
    seq1.processingInstruction(name, data, locationId, properties)
    seq2.processingInstruction(name, data, locationId, properties)
  }

  def comment(content: CharSequence,
              locationId: Location,
              properties: Int): Unit = {
    seq1.comment(content, locationId, properties)
    seq2.comment(content, locationId, properties)
  }

  def close(): Unit = {
    seq1.close()
    seq2.close()
  }

  override def usesTypeAnnotations(): Boolean =
    seq1.usesTypeAnnotations() || seq2.usesTypeAnnotations()

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * TeeOutputter: a SequenceReceiver that duplicates received events to two different destinations
  */
