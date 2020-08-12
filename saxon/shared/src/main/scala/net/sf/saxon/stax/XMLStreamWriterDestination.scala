////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.stax

import net.sf.saxon.event.NamespaceReducer

import net.sf.saxon.event.PipelineConfiguration

import net.sf.saxon.event.Receiver

import net.sf.saxon.s9api.AbstractDestination

import net.sf.saxon.s9api.Destination

import net.sf.saxon.s9api.SaxonApiException

import net.sf.saxon.serialize.SerializationProperties

import javax.xml.stream.XMLStreamException

import javax.xml.stream.XMLStreamWriter




/**
  * XMLStreamWriterDestination is a s9api {@link Destination} that writes output to a supplied XMLStreamWriter
  */
class XMLStreamWriterDestination(private var writer: XMLStreamWriter)
    extends AbstractDestination {

  def getXMLStreamWriter(): XMLStreamWriter = writer

  def getReceiver(pipe: PipelineConfiguration,
                  params: SerializationProperties): Receiver = {
    val r: Receiver = new ReceiverToXMLStreamWriter(writer)
    r.setPipelineConfiguration(pipe)
    r
  }

  def close(): Unit = {
    writer.close()
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
