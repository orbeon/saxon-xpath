////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.s9api

import org.orbeon.saxon.event.Outputter

import org.orbeon.saxon.event.PipelineConfiguration

import org.orbeon.saxon.event.Receiver

import org.orbeon.saxon.serialize.SerializationProperties

import java.net.URI

import java.util.function.Consumer




/**
  * A <tt>Destination</tt> represents a place where XDM values can be sent. It is used, for example,
  * to define the output of a transformation or query.
  * <p>
  * A {@code Destination} is either a tree destination or a raw destination. A <b>tree destination</b>
  * performs <i>sequence normalization</i> on the stream of events passed to it, to construct
  * a tree rooted at a single XDM document node, as defined in the W3C serialization specification
  * (even if the destination is not actually a serializer). A <b>raw destination</b> omits this step.
  * Examples of tree destinations are those designed to accept XML: {@link DOMDestination},
  * {@link SAXDestination}, {@link XdmDestination}, {@link SchemaValidator}.
  * <p>
  * The {@link Serializer} acts as a tree destination when the output methods XML, HTML, XHTML, or TEXT
  * are used, but as a raw destination when the output method is JSON or ADAPTIVE.
  * <p>
  * The interface {@code Destination} has some similarities with the JAXP
  * {@link javax.xml.transform.Result} class. It differs, however, in that implementations
  * of this interface can be written by users or third parties to define new kinds of
  * destination, and any such implementation can be supplied to the Saxon methods that
  * take a {@code Destination} as an argument.
  * <p>
  * Implementing a new {@code Destination} will generally require access
  * to implementation-level classes and methods within the Saxon product. The only method that
  * needs to be supplied is {@link #getReceiver}, which returns an instance of
  * {@link Outputter}, and unless you use an existing implementation of
  * {@code Receiver}, you will need to handle internal Saxon concepts such as name codes
  * and name pools.
  * <p>
  * In general a Destination is not thread-safe (cannot be used from more than one thread),
  * and is not serially reusable. So a Destination should only be used once. A Destination
  * supplied to Saxon may be modified by Saxon.
  * <p>
  * The {@link #close} method is called by the system when
  * it finishes writing the document, and this should cause all resources held by the Destination
  * to be released.
  */
trait Destination {

  def setDestinationBaseURI(baseURI: URI): Unit

  def getDestinationBaseURI: URI

  def getReceiver(pipe: PipelineConfiguration,
                  params: SerializationProperties): Receiver

  def onClose(listener: Action): Unit

  def closeAndNotify(): Unit

  def close(): Unit

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
