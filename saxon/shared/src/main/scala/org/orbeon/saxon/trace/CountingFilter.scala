////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.trace

import org.orbeon.saxon.event.ProxyReceiver

import org.orbeon.saxon.event.Receiver

import org.orbeon.saxon.event.ReceiverOption

import org.orbeon.saxon.event.SequenceReceiver

import org.orbeon.saxon.model.SchemaType

import org.orbeon.saxon.om.AttributeMap

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.NamespaceMap

import org.orbeon.saxon.om.NodeName

import org.orbeon.saxon.s9api.Location

import org.orbeon.saxon.trans.XPathException

import java.io.PrintStream

import CountingFilter._

import scala.beans.{BeanProperty, BooleanBeanProperty}




object CountingFilter {

  private var nextid: Int = 0

}

/**
  * A filter that can be inserted into a Receiver pipeline to count the events that pass through.
  * This class is not normally used in Saxon, but is available for diagnostics when needed. Note
  * that the counters are only maintained if {@link Instrumentation#ACTIVE} is set to true. The counters
  * can be displayed by calling {@link Instrumentation#report()}.
  */
class CountingFilter(nextReceiver: Receiver)
    extends ProxyReceiver(nextReceiver) {

  @BeanProperty
  var id: Int = { nextid += 1; nextid - 1 }

  def this(nextReceiver: Receiver, diagnosticOutput: PrintStream) = {
    this(nextReceiver)
    id = nextid
    nextid += 1
  }

  private def count(counter: String): Unit = {
    Instrumentation.count("Filter " + id + " " + counter)
  }

  override def append(item: Item, locationId: Location, copyNamespaces: Int): Unit = {
    count("append")
    if (nextReceiver.isInstanceOf[SequenceReceiver]) {
      nextReceiver
        .asInstanceOf[SequenceReceiver]
        .append(item, locationId, copyNamespaces)
    } else {
      super.append(item, locationId, copyNamespaces)
    }
  }

  override def characters(chars: CharSequence,
                          locationId: Location,
                          properties: Int): Unit = {
    count("characters")
    nextReceiver.characters(chars, locationId, properties)
  }

  override def close(): Unit = {
    count("close")
    nextReceiver.close()
  }

  override def comment(chars: CharSequence,
                       locationId: Location,
                       properties: Int): Unit = {
    count("comment")
    nextReceiver.comment(chars, locationId, properties)
  }

  override def endDocument(): Unit = {
    count("endDocument")
    nextReceiver.endDocument()
  }

  override def endElement(): Unit = {
    count("endElement")
    nextReceiver.endElement()
  }

  override def open(): Unit = {
    count("open")
    nextReceiver.open()
  }

  override def processingInstruction(target: String,
                                     data: CharSequence,
                                     locationId: Location,
                                     properties: Int): Unit = {
    count("processingInstruction")
    nextReceiver.processingInstruction(target, data, locationId, properties)
  }

  override def startDocument(properties: Int): Unit = {
    count("startDocument")
    nextReceiver.startDocument(properties)
  }

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    count("startElement")
    nextReceiver.startElement(elemName,
                              `type`,
                              attributes,
                              namespaces,
                              location,
                              properties)
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
