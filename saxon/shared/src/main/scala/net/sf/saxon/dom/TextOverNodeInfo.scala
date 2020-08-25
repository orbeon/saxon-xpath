////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.dom

import net.sf.saxon.model.ComplexType

import net.sf.saxon.model.SchemaType

import net.sf.saxon.model.Type

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.value.Whitespace

import org.w3c.dom.Comment

import NodeOverNodeInfo._

import org.w3c.dom.Text




class TextOverNodeInfo extends NodeOverNodeInfo with Text with Comment {

  def getData(): String = node.getStringValue

  def setData(data: String): Unit = {
    disallowUpdate()
  }

  def getLength(): Int = node.getStringValue.length

  def substringData(offset: Int, count: Int): String =
    node.getStringValue.substring(offset, offset + count)

  def appendData(arg: String): Unit = {
    disallowUpdate()
  }

  def insertData(offset: Int, arg: String): Unit = {
    disallowUpdate()
  }

  def deleteData(offset: Int, count: Int): Unit = {
    disallowUpdate()
  }

  def replaceData(offset: Int, count: Int, arg: String): Unit = {
    disallowUpdate()
  }

  def splitText(offset: Int): Text = {
    disallowUpdate()
    null
  }

  /*@Nullable*/

  def replaceWholeText(content: String): Text = {
    disallowUpdate()
    null
  }

  /**
    * Returns whether this text node contains <a href='http://www.w3.org/TR/2004/REC-xml-infoset-20040204#infoitem.character'>
    * element content whitespace</a>, often abusively called "ignorable whitespace". The text node is
    * determined to contain whitespace in element content during the load
    * of the document or if validation occurs while using
    * <code>Document.normalizeDocument()</code>.
    *
    * @since DOM Level 3
    */
  def isElementContentWhitespace(): Boolean = {
    if (node.getNodeKind != Type.TEXT) {
      throw new UnsupportedOperationException(
        "Method is defined only on text nodes")
    }
    if (!Whitespace.isWhite(node.getStringValue)) {
      return false
    }
    val parent: NodeInfo = node.getParent
    if (parent == null) {
      return false
    }
    val `type`: SchemaType = parent.getSchemaType
    `type`.isComplexType && !`type`.asInstanceOf[ComplexType].isMixedContent
  }

  /**
    * Returns all text of <code>Text</code> nodes logically-adjacent text
    * nodes to this node, concatenated in document order.
    * <br>For instance, in the example below <code>wholeText</code> on the
    * <code>Text</code> node that contains "bar" returns "barfoo", while on
    * the <code>Text</code> node that contains "foo" it returns "barfoo".
    *
    * @since DOM Level 3
    */
  def getWholeText(): String = {
    if (node.getNodeKind != Type.TEXT) {
      throw new UnsupportedOperationException(
        "Method is defined only on text nodes")
    }
    node.getStringValue
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class is an implementation of the DOM Text and Comment interfaces that wraps a Saxon NodeInfo
  * representation of a text or comment node.
  */
