////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.om

import net.sf.saxon.tree.wrapper.AbstractVirtualNode

import NameOfNode._




object NameOfNode {

  def makeName(node: NodeInfo): NodeName =
    if (node.isInstanceOf[MutableNodeInfo]) {
      new FingerprintedQName(node.getPrefix, node.getURI, node.getLocalPart)
    } else if (node.isInstanceOf[AbstractVirtualNode]) {
      new NameOfNode(node.asInstanceOf[AbstractVirtualNode].getUnderlyingNode)
    } else {
      new NameOfNode(node)
    }

}

/**
  * An implementation of NodeName that gets the name of an existing NodeInfo object.
  * Useful when nodes are copied. However, it's not safe to use when the node is mutable.
  */
class NameOfNode private (private var node: NodeInfo) extends NodeName {

  /**
    * Get the prefix of the QName.
    *
    * @return the prefix. Returns the empty string if the name is unprefixed.
    */
  def getPrefix(): String = node.getPrefix

  /**
    * Get the namespace URI of the QName.
    *
    * @return the URI. Returns the empty string to represent the no-namespace
    */
  def getURI(): String = node.getURI

  /**
    * Get the local part of the QName
    *
    * @return the local part of the QName
    */
  def getLocalPart(): String = node.getLocalPart

  /**
    * Get the display name, that is the lexical QName in the form [prefix:]local-part
    *
    * @return the lexical QName
    */
  def getDisplayName(): String = node.getDisplayName

  /**
    * Get the name in the form of a StructuredQName
    *
    * @return the name in the form of a StructuredQName
    */
  def getStructuredQName(): StructuredQName =
    new StructuredQName(getPrefix, getURI, getLocalPart)

  /**
    * Test whether this name is in a given namespace
    *
    * @param ns the namespace to be tested against
    * @return true if the name is in the specified namespace
    */
  def hasURI(ns: String): Boolean = node.getURI == ns

  def getNamespaceBinding(): NamespaceBinding =
    NamespaceBinding.makeNamespaceBinding(getPrefix, getURI)

  /**
    * Ask whether this node name representation has a known namecode and fingerprint
    *
    * @return true if the methods getFingerprint() and getNameCode() will
    *         return a result other than -1
    */
  def hasFingerprint(): Boolean = node.hasFingerprint()

  /**
    * Get the fingerprint of this name if known. This method should not to any work to allocate
    * a fingerprint if none is already available
    *
    * @return the fingerprint if known; otherwise -1
    */
  def getFingerprint(): Int =
    if (hasFingerprint()) {
      node.getFingerprint
    } else {
      -1
    }

  /**
    * Get the nameCode of this name, allocating a new code from the namepool if necessary
    *
    * @param namePool the NamePool used to allocate the name
    * @return a nameCode for this name, newly allocated if necessary
    */
  def obtainFingerprint(namePool: NamePool): Int =
    if (node.hasFingerprint()) {
      node.getFingerprint
    } else {
      namePool.allocateFingerprint(node.getURI, node.getLocalPart)
    }

  /**
    * Returns a hash code value for the object.
    */
  override def hashCode(): Int =
    StructuredQName.computeHashCode(getURI, getLocalPart)

  /**
    * Indicates whether some other object is "equal to" this one.
    */
  override def equals(obj: Any): Boolean = obj match {
    case obj: NodeName => {
      val n: NodeName = obj
      if (node.hasFingerprint() && n.hasFingerprint()) {
        node.getFingerprint == n.getFingerprint
      } else {
        n.getLocalPart == node.getLocalPart && n.hasURI(node.getURI)
      }
    }
    case _ => false

  }

  /**
    * Determine whether two IdentityComparable objects are identical. This is a stronger
    * test than equality (even schema-equality); for example two dateTime values are not identical unless
    * they are in the same timezone.
    *
    * @param other
    * @return true if the two values are indentical, false otherwise
    */
  def isIdentical(other: IdentityComparable): Boolean =
    other.isInstanceOf[NodeName] && (this eq other) &&
      this.getPrefix == other.asInstanceOf[NodeName].getPrefix

  /**
    * Get a hashCode that offers the guarantee that if A.isIdentical(B), then A.identityHashCode() == B.identityHashCode()
    *
    * @return a hashCode suitable for use when testing for identity.
    */
  def identityHashCode(): Int = hashCode ^ getPrefix.hashCode

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
