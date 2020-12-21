////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.dom

import org.orbeon.saxon.model.Type

import org.w3c.dom.Node




/**
  * This class represents a DOM text node that is the child of a DOM attribute node. The DOM attribute node
  * will be a wrapper over a Saxon attribute node or namespace node.
  */
class TextOverAttrInfo(private var attr: AttrOverNodeInfo)
    extends TextOverNodeInfo {

  this.node = attr.getUnderlyingNodeInfo

  /**
    * Returns whether this text node contains <a href='http://www.w3.org/TR/2004/REC-xml-infoset-20040204#infoitem.character'>
    * element content whitespace</a>, often abusively called "ignorable whitespace". The text node is
    * determined to contain whitespace in element content during the load
    * of the document or if validation occurs while using
    * <code>Document.normalizeDocument()</code>.
    *
    * @since DOM Level 3
    */
  override def isElementContentWhitespace(): Boolean = false

  override def getNodeType: Short = Type.TEXT

  override def compareDocumentPosition(other: Node): Short = {
    val DOCUMENT_POSITION_FOLLOWING: Short = 0x04
    if (other.isInstanceOf[TextOverAttrInfo]) {
      if (node == other.asInstanceOf[TextOverAttrInfo].node) {
        return 0
      } else {
       return attr.compareDocumentPosition(other.asInstanceOf[TextOverAttrInfo].attr)
      }
    } else if (other.isInstanceOf[AttrOverNodeInfo]) {
      if (node == other.asInstanceOf[AttrOverNodeInfo].getUnderlyingNodeInfo) {
       return DOCUMENT_POSITION_FOLLOWING
      }
    }
    attr.compareDocumentPosition(other)
  }

  override def getParentNode: Node = attr

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
