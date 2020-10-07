////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.event

import org.orbeon.saxon.model.SchemaType

import org.orbeon.saxon.om.AttributeMap

import org.orbeon.saxon.om.NamespaceMap

import org.orbeon.saxon.om.NodeName

import org.orbeon.saxon.s9api.Location

import org.orbeon.saxon.trans.Err

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.Whitespace




class DocumentValidator(next: Receiver, private var errorCode: String)
    extends ProxyReceiver(next) {

  private var foundElement: Boolean = false

  private var level: Int = 0

  override def setPipelineConfiguration(config: PipelineConfiguration): Unit = {
    super.setPipelineConfiguration(config)
  }

  override def startElement(elemName: NodeName,
                   `type`: SchemaType,
                   attributes: AttributeMap,
                   namespaces: NamespaceMap,
                   location: Location,
                   properties: Int): Unit = {
    if (foundElement && level == 0) {
      throw new XPathException(
        "A valid document must have only one child element",
        errorCode)
    }
    foundElement = true
    level += 1
    nextReceiver.startElement(elemName,
                              `type`,
                              attributes,
                              namespaces,
                              location,
                              properties)
  }

  override def characters(chars: CharSequence,
                 locationId: Location,
                 properties: Int): Unit = {
    if (level == 0) {
      if (Whitespace.isWhite(chars)) {
// ignore whitespace outside the outermost element
        return
      }
      throw new XPathException(
        "A valid document must contain no text outside the outermost element (found \"" +
          Err.truncate30(chars) +
          "\")",
        errorCode)
    }
    nextReceiver.characters(chars, locationId, properties)
  }

 override def endElement(): Unit = {
    level -= 1
    nextReceiver.endElement()
  }

  override def endDocument(): Unit = {
    if (level == 0) {
      if (!foundElement) {
        throw new XPathException("A valid document must have a child element",
                                 errorCode)
      }
      foundElement = false
      nextReceiver.endDocument()
      level = -1
    }
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * DocumentValidator checks that a document is well-formed: specifically, that it contains a single element
  * node child and no text node children.
  */
// Copyright (c) 2004-2020 Saxonica Limited
