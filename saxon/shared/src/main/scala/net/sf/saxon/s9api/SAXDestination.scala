////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.s9api

import net.sf.saxon.event.ContentHandlerProxy

import net.sf.saxon.event.PipelineConfiguration

import net.sf.saxon.event.Receiver

import net.sf.saxon.serialize.SerializationProperties

import org.xml.sax.ContentHandler




class SAXDestination(private var contentHandler: ContentHandler)
    extends AbstractDestination {

  /*@NotNull*/

  def getReceiver(pipe: PipelineConfiguration,
                  params: SerializationProperties): Receiver = {
    val chp: ContentHandlerProxy = new ContentHandlerProxy()
    chp.setUnderlyingContentHandler(contentHandler)
    chp.setPipelineConfiguration(pipe)
    params.makeSequenceNormalizer(chp)
  }

  def close(): Unit = {}
// no action
// no action

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class represents a {@link Destination} (for example, the destination of the output of a transformation)
  * in which events representing the XML document are sent to a user-supplied SAX2 {@link ContentHandler}, as
  * if the {@code ContentHandler} were receiving the document directly from an XML parser.
  * <p>If the supplied {@code ContentHandler} implements the {@link org.xml.sax.ext.LexicalHandler} interface,
  * then comment nodes will be notified to the handler; if not, comments will be silently ignored.</p>
  * <p>Namespace-related information is reported to the {@code ContentHandler} following the conventions of
  * an {@link org.xml.sax.XMLReader} configured with default values for the features
  * {@code http://xml.org/sax/features/namespaces} and {@code http://xml.org/sax/features/namespace-prefixes}.</p>
  * <p>If and only if the supplied {@code ContentHandler} implements the {@link javax.xml.transform.sax.TransformerHandler}
  * interface, then unparsed entities may be notified to the {@link org.xml.sax.DTDHandler#unparsedEntityDecl(String, String, String, String)}
  * method.</p>
  */
