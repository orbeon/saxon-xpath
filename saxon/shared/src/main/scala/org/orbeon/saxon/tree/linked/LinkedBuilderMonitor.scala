////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.tree.linked

import org.orbeon.saxon.event.BuilderMonitor

import org.orbeon.saxon.model.SchemaType

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.om.AttributeMap

import org.orbeon.saxon.om.NamespaceMap

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.om.NodeName

import org.orbeon.saxon.s9api.Location

import org.orbeon.saxon.trans.XPathException

import scala.beans.{BeanProperty, BooleanBeanProperty}




/**
  * Monitor construction of a document tree. This allows a marker to be set during tree construction, in such a way
  * that the node corresponding to the marker can be retrieved at the end of tree construction. This is used in the
  * implementation of the XSLT 3.0 snapshot function.
  */
class LinkedBuilderMonitor(private var builder: LinkedTreeBuilder)
    extends BuilderMonitor(builder) {

  private var mark: Int = -1

  /*@Nullable*/

  @BeanProperty
  var markedNode: NodeInfo = _

  def markNextNode(nodeKind: Int): Unit = {
    mark = nodeKind
  }

  override def startDocument(properties: Int): Unit = {
    super.startDocument(properties)
    if (mark == Type.DOCUMENT) {
      markedNode = builder.getCurrentParentNode
    }
    mark = -1
  }

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    super.startElement(elemName,
                       `type`,
                       attributes,
                       namespaces,
                       location,
                       properties)
    if (mark == Type.ELEMENT) {
      markedNode = builder.getCurrentParentNode
    }
    mark = -1
  }

  override def characters(chars: CharSequence,
                          locationId: Location,
                          properties: Int): Unit = {
    super.characters(chars, locationId, properties)
    if (mark == Type.TEXT) {
      markedNode = builder.getCurrentLeafNode
    }
    mark = -1
  }

  override def comment(chars: CharSequence,
                       locationId: Location,
                       properties: Int): Unit = {
    super.comment(chars, locationId, properties)
    if (mark == Type.COMMENT) {
      markedNode = builder.getCurrentLeafNode
    }
    mark = -1
  }

  override def processingInstruction(target: String,
                                     data: CharSequence,
                                     locationId: Location,
                                     properties: Int): Unit = {
    super.processingInstruction(target, data, locationId, properties)
    if (mark == Type.PROCESSING_INSTRUCTION) {
      markedNode = builder.getCurrentLeafNode
    }
    mark = -1
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
