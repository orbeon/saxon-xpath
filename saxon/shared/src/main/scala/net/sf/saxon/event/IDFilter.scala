////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.event

import net.sf.saxon.model.SchemaType

import net.sf.saxon.model.SimpleType

import net.sf.saxon.om._

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

import java.util.HashSet


class IDFilter(next: Receiver, id: String) extends ProxyReceiver(next) {

  private var requiredId: String = id

  private var activeDepth: Int = 0

  private var matched: Boolean = false

  private var nonIDs: HashSet[SimpleType] = _

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    matched = false
    if (activeDepth == 0) {
      for (att <- attributes
           if (att.getNodeName == StandardNames.XML_ID_NAME) ||
             ReceiverOption.contains(att.getProperties, ReceiverOption.IS_ID)
           if att.getValue == requiredId) {
        matched = true
      }
      if (matched) {
        activeDepth = 1
        // this remembers the details
        super.startElement(elemName,
          `type`,
          attributes,
          namespaces,
          location,
          properties)
      }
    } else {
      activeDepth += 1
      // this remembers the details
      super.startElement(elemName,
        `type`,
        attributes,
        namespaces,
        location,
        properties)
    }
  }

  override def endElement(): Unit = {
    if (activeDepth > 0) {
      nextReceiver.endElement()
      activeDepth -= 1
    }
  }

  override def characters(chars: CharSequence,
                          locationId: Location,
                          properties: Int): Unit = {
    if (activeDepth > 0) {
      super.characters(chars, locationId, properties)
    }
  }

  override def processingInstruction(target: String,
                                     data: CharSequence,
                                     locationId: Location,
                                     properties: Int): Unit = {
    if (activeDepth > 0) {
      super.processingInstruction(target, data, locationId, properties)
    }
  }

  override def comment(chars: CharSequence,
                       locationId: Location,
                       properties: Int): Unit = {
    if (activeDepth > 0) {
      super.comment(chars, locationId, properties)
    }
  }

  override def usesTypeAnnotations(): Boolean = true

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * IDFilter is a ProxyReceiver that extracts the subtree of a document rooted at the
 * element with a given ID value. Namespace declarations outside this subtree are
 * treated as if they were present on the identified element.
 *
 * <p>Note, this class only looks for ID attributes, not for ID elements.</p>
 */
