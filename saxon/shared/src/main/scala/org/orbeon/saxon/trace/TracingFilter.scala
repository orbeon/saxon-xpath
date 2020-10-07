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

import org.orbeon.saxon.tree.util.FastStringBuffer

import org.orbeon.saxon.value.Whitespace

import java.io.PrintStream

import TracingFilter._

import scala.beans.{BeanProperty, BooleanBeanProperty}




object TracingFilter {

  private var nextid: Int = 0

}

/**
  * A filter that can be inserted into a Receiver pipeline to trace the events that pass through.
  * This class is not normally used in Saxon, but is available for diagnostics when needed.
  */
class TracingFilter(nextReceiver: Receiver)
    extends ProxyReceiver(nextReceiver) {

  @BeanProperty
  var id: Int = { nextid += 1; nextid - 1 }

  private var indent: String = ""

  private var out: PrintStream = System.err

  private var closed: Boolean = false

  def this(nextReceiver: Receiver, diagnosticOutput: PrintStream) = {
    this(???) /* TODO: Scala does not allow multiple super constructor calls
     * Change this code to call a constructor of the current class instead.
     * For your convenience, here is the invalid super constructor call:
     * }super(nextReceiver)
     */

    id = { nextid += 1; nextid - 1 }
    out = diagnosticOutput
  }

  override def append(item: Item, locationId: Location, copyNamespaces: Int): Unit = {
    out.println("RCVR " + id + indent + " APPEND " + item.getClass.getName)
    if (nextReceiver.isInstanceOf[SequenceReceiver]) {
      nextReceiver.append(item, locationId, copyNamespaces)
    } else {
      super.append(item, locationId, copyNamespaces)
    }
  }

  override def characters(chars: CharSequence,
                          locationId: Location,
                          properties: Int): Unit = {
    out.println(
      "RCVR " + id + indent + " CHARACTERS " +
        (if (Whitespace.isWhite(chars)) "(whitespace)" else ""))
    val sb: FastStringBuffer = new FastStringBuffer(chars.length * 4)
    sb.cat(chars).append(":")
    for (i <- 0 until chars.length) {
      sb.append(chars.charAt(i).toInt.toString + " ")
    }
    out.println("    \"" + sb + '\"')
    nextReceiver.characters(chars, locationId, properties)
  }

  override def close(): Unit = {
    out.println("RCVR " + id + indent + " CLOSE")
    nextReceiver.close()
  }

  override def comment(chars: CharSequence,
                       locationId: Location,
                       properties: Int): Unit = {
    out.println("RCVR " + id + indent + " COMMENT")
    nextReceiver.comment(chars, locationId, properties)
  }

  override def endDocument(): Unit = {
    out.println("RCVR " + id + indent + " END DOCUMENT")
    nextReceiver.endDocument()
  }

  override def endElement(): Unit = {
    if (indent.isEmpty) {
      throw new XPathException("RCVR " + id + " Unmatched end tag")
    }
    indent = indent.substring(2)
    out.println("RCVR " + id + indent + " END ELEMENT")
    nextReceiver.endElement()
  }

  override def open(): Unit = {
    out.println("RCVR " + id + indent + " OPEN")
    nextReceiver.open()
  }

  override def processingInstruction(target: String,
                                     data: CharSequence,
                                     locationId: Location,
                                     properties: Int): Unit = {
    out.println("RCVR " + id + indent + " PROCESSING INSTRUCTION")
    nextReceiver.processingInstruction(target, data, locationId, properties)
  }

  override def startDocument(properties: Int): Unit = {
    out.println("RCVR " + id + indent + " START DOCUMENT")
    nextReceiver.startDocument(properties)
  }

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    out.println(
      "RCVR " + id + indent + " START ELEMENT " + elemName.getDisplayName)
    indent = indent + "  "
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
