////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.event

import net.sf.saxon.model.ComplexType

import net.sf.saxon.model.SchemaType

import net.sf.saxon.model.Untyped

import net.sf.saxon.om.AttributeMap

import net.sf.saxon.om.NamespaceMap

import net.sf.saxon.om.NodeName

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.Whitespace

import java.util.Arrays


class IgnorableWhitespaceStripper(next: Receiver) extends ProxyReceiver(next) {

  private var stripStack: Array[Boolean] = new Array[Boolean](100)

  private var top: Int = 0

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    nextReceiver.startElement(elemName,
      `type`,
      attributes,
      namespaces,
      location,
      properties)
    var strip: Boolean = false
    if (`type` != Untyped.getInstance) {
      // if the element has element-only content, whitespace stripping is enabled
      if (`type`.isComplexType && !`type`
        .asInstanceOf[ComplexType]
        .isSimpleContent &&
        !`type`.asInstanceOf[ComplexType].isMixedContent) {
        strip = true
      }
    }
    top += 1
    if (top >= stripStack.length) {
      stripStack = Arrays.copyOf(stripStack, top * 2)
    }
    stripStack(top) = strip
  }

  // put "strip" value on top of stack
  // put "strip" value on top of stack

  override def endElement(): Unit = {
    nextReceiver.endElement()
    top -= 1
  }

  override def characters(chars: CharSequence,
                          locationId: Location,
                          properties: Int): Unit = {
    if (chars.length > 0 && (!stripStack(top) || !Whitespace.isWhite(chars))) {
      nextReceiver.characters(chars, locationId, properties)
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
 * The IgnorableWhitespaceStripper removes whitespace text nodes belonging to elements
 * whose schema-defined type defines element-only content
 */
// Copyright (c) 2005-2020 Saxonica Limited
