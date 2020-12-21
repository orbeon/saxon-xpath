////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.om

import java.{util => ju}

import javax.xml.transform.Source
import org.orbeon.saxon.utils.Configuration


/**
  * This interface represents information about a tree as a whole. The tree may or may not be rooted
  * at a document node. In some tree models, the interface is implemented by the object representing the
  * root node of the tree (typically but not necessarily the document node). In other tree models,
  * it is a free-standing object. The TreeInfo holds information that is common to all the nodes in
  * a tree, such as the document number and a reference to the Configuration.
  *
  * Java object identity for TreeInfo objects equates to XPath node identity for the root
  * nodes of the relevant trees: that is, two root nodes are "the same node" if and only if
  * their TreeInfo objects are the same Java object. However, when sorting into document order,
  * the order of trees is based on their "document number", a unique number allocated by the
  * document number allocator for the Configuration.
  *
  * @author Michael H. Kay
  * @since 9.7. Replaces the DocumentInfo interface which represented both a document as a whole, and
  * the document node at the root of a document, but which did not apply to trees rooted at a node other
  * than a document node.
  */
trait TreeInfo extends Source {
  def getRootNode: NodeInfo
  def getConfiguration: Configuration
  def getDocumentNumber: Long
  def isTyped: Boolean = false
  def isMutable: Boolean = false
  def selectID(id: String, getParent: Boolean): NodeInfo
  def getUnparsedEntityNames: ju.Iterator[String]
  def getUnparsedEntity(name: String): Array[String]
  def setSpaceStrippingRule(rule: SpaceStrippingRule): Unit
  def getSpaceStrippingRule: SpaceStrippingRule
  def setUserData(key: String, value: Any): Unit
  def getUserData(key: String): Any
}
