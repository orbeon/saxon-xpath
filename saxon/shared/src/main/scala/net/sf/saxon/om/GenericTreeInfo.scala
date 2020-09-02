////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * A generic (model-independent) implementation of the TreeInfo interface, suitable for use with all
 * tree models where the object representing the document node does not itself act as the TreeInfo
 * implementation
 */

package net.sf.saxon.om

import java.util._

import net.sf.saxon.utils.Configuration

import scala.beans.BeanProperty


class GenericTreeInfo(private var config: Configuration) extends TreeInfo {

  private var root: NodeInfo = _

  @BeanProperty
  var systemId: String = _

  private var userData: Map[String, Any] = _

  private var documentNumber: Long = -1

  var spaceStrippingRule: SpaceStrippingRule =NoElementsSpaceStrippingRule.getInstance

  def this(config: Configuration, root: NodeInfo) = {
    this(config)
    this.root = root
  }

  def setConfiguration(config: Configuration): Unit =
    this.config = config

  def getConfiguration: Configuration = config

  def setRootNode(root: NodeInfo): Unit = {
    if (root.getParent != null)
      throw new IllegalArgumentException("The root node of a tree must be parentless")
    this.root = root
  }

  /**
    * Get the NodeInfo object representing the root of the tree (not necessarily a document node)
    *
    * @return the root node
    */
  def getRootNode: NodeInfo = root

  /**
    * Get the Public ID of the entity containing the node.
    *
    * @return null (always)
    * @since 9.7
    */
  def getPublicId: String = null

  def getDocumentNumber: Long = {
    if (documentNumber == -1) {
      val dna = config.getDocumentNumberAllocator
      this.synchronized {
        if (documentNumber == -1)
          documentNumber = dna.allocateDocumentNumber()
      }
    }
    documentNumber
  }

  def setDocumentNumber(documentNumber: Long): Unit =
    synchronized {
      this.documentNumber = documentNumber
    }

  /**
    * Get the element with a given ID, if any
    *
    * @param id        the required ID value
    * @param getParent true if running the element-with-id() function rather than the id()
    *                  function; the difference is that in the case of an element of type xs:ID, the parent of
    *                  the element should be returned, not the element itself.
    * @return the element with the given ID, or null if there is no such ID
    * present (or if the parser has not notified attributes as being of
    * type ID)
    * @since 8.4. Second argument added in 9.2.
    */
  def selectID(id: String, getParent: Boolean): NodeInfo = null

  /**
    * Get the list of unparsed entities defined in this document
    *
    * @return an Iterator, whose items are of type String, containing the names of all
    * unparsed entities defined in this document. If there are no unparsed entities or if the
    * information is not available then an empty iterator is returned
    * @since 9.1
    */
  def getUnparsedEntityNames: Iterator[String] =
    Collections.emptyList[String].iterator

  /**
    * Get the unparsed entity with a given name
    *
    * @param name the name of the entity
    * @return if the entity exists, return an array of two Strings, the first
    * holding the system ID of the entity (as an absolute URI if possible),
    * the second holding the public ID if there is one, or null if not.
    * If the entity does not exist, the method returns null.
    * Applications should be written on the assumption that this array may
    * be extended in the future to provide additional information.
    * @since 8.4
    */
  def getUnparsedEntity(name: String): Array[String] = null

  /**
    * Set user data on the tree. The user data can be retrieved subsequently
    * using {@link #getUserData}
    *
    * @param key   A string giving the name of the property to be set. Clients are responsible
    *              for choosing a key that is likely to be unique. Must not be null. Keys used internally
    *              by Saxon are prefixed "saxon:".
    * @param value The value to be set for the property. May be null, which effectively
    */
  def setUserData(key: String, value: Any): Unit = {
    if (userData == null)
      userData = new HashMap[String, Any]()
    userData.put(key, value)
  }

  /**
    * Get user data held in the tree. This retrieves properties previously set using
    * {@link #setUserData}
    *
    * @param key A string giving the name of the property to be retrieved.
    * @return the value of the property, or null if the property has not been defined.
    */
  def getUserData(key: String): Any =
    if (userData == null)
      userData
    else
      userData.get(key)

  def isStreamed: Boolean = false
  def setSpaceStrippingRule(rule: SpaceStrippingRule): Unit = this.spaceStrippingRule = spaceStrippingRule;
  def getSpaceStrippingRule: SpaceStrippingRule = this.spaceStrippingRule
}
