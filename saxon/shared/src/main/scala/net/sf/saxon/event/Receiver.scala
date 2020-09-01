////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.event

import net.sf.saxon.expr.parser.Loc

import net.sf.saxon.model.SchemaType

import net.sf.saxon.om.AttributeMap

import net.sf.saxon.om.Item

import net.sf.saxon.om.NamespaceMap

import net.sf.saxon.om.NodeName

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

import javax.xml.transform.Result




trait Receiver extends Result {

  def setPipelineConfiguration(pipe: PipelineConfiguration): Unit

  /*@NotNull*/

  def getPipelineConfiguration: PipelineConfiguration

  def setSystemId(systemId: String): Unit

  def open(): Unit

  def startDocument(properties: Int): Unit

  def endDocument(): Unit

  def setUnparsedEntity(name: String, systemID: String, publicID: String): Unit

  def startElement(elemName: NodeName,
                   `type`: SchemaType,
                   attributes: AttributeMap,
                   namespaces: NamespaceMap,
                   location: Location,
                   properties: Int): Unit

  def endElement(): Unit

  def characters(chars: CharSequence,
                 location: Location,
                 properties: Int): Unit

  def processingInstruction(name: String,
                            data: CharSequence,
                            location: Location,
                            properties: Int): Unit

  def comment(content: CharSequence, location: Location, properties: Int): Unit

  def append(item: Item, locationId: Location, properties: Int): Unit = {
    throw new UnsupportedOperationException()
  }

  def append(item: Item): Unit = {
    append(item, Loc.NONE, ReceiverOption.ALL_NAMESPACES)
  }

  def close(): Unit

  def usesTypeAnnotations(): Boolean = false

  def handlesAppend(): Boolean = false

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Receiver: This interface represents a recipient of XML tree-walking (push) events. It is
  * based on SAX2's ContentHandler, but adapted to handle additional events. Namespaces and
  * Attributes are handled by separate events following the startElement event. Schema types
  * can be defined for elements and attributes.
  * <p>The Receiver interface is an important internal interface within Saxon, and provides a powerful
  * mechanism for integrating Saxon with other applications. It has been designed with extensibility
  * and stability in mind. However, it should be considered as an interface designed primarily for
  * internal use, and not as a completely stable part of the public Saxon API.</p>
  *
  * @since 8.0. Extended in 9.9 to support additional methods with default implementations.
  * Changed in 10.0 to accept all the attributes and namespaces as part of the startElement event
  * (thus eliminating the need for a separate startContent event).
  */
