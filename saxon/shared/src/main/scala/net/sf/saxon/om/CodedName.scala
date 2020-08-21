////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.om

import scala.beans.{BeanProperty, BooleanBeanProperty}




/**
  * An implementation of NodeName that encapsulates an integer fingerprint, a string prefix, and a reference to the NamePool from which
  * the fingerprint was allocated.
  */
class CodedName(@BeanProperty var fingerprint: Int,
                @BeanProperty var prefix: String,
                private var pool: NamePool)
    extends NodeName {

  /**
    * Get the namespace URI of the QName.
    *
    * @return the URI. Returns the empty string to represent the no-namespace
    */
  def getURI(): String = pool.getURI(fingerprint)

  /**
    * Get the local part of the QName
    *
    * @return the local part of the QName
    */
  def getLocalPart(): String = pool.getLocalName(fingerprint)

  /**
    * Get the display name, that is the lexical QName in the form [prefix:]local-part
    *
    * @return the lexical QName
    */
  def getDisplayName(): String =
    if (prefix.isEmpty) getLocalPart else prefix + ":" + getLocalPart

  /**
    * Get the name in the form of a StructuredQName
    *
    * @return the name in the form of a StructuredQName
    */
  def getStructuredQName(): StructuredQName = {
    val qn: StructuredQName = pool.getUnprefixedQName(fingerprint)
    if (prefix.isEmpty) {
      qn
    } else {
      new StructuredQName(prefix, qn.getURI, qn.getLocalPart)
    }
  }

  /**
    * Test whether this name is in a given namespace
    *
    * @param ns the namespace to be tested against
    * @return true if the name is in the specified namespace
    */
  def hasURI(ns: String): Boolean = getURI == ns

  def getNamespaceBinding(): NamespaceBinding =
    new NamespaceBinding(prefix, pool.getURI(fingerprint))

  /**
    * Ask whether this node name representation has a known fingerprint
    *
    * @return true if the method getFingerprint() will
    *         return a result other than -1
    */
  def hasFingerprint(): Boolean = true

  /**
    * Get the nameCode of this name, allocating a new code from the namepool if necessary
    *
    * @param namePool the NamePool used to allocate the name
    * @return a nameCode for this name, newly allocated if necessary
    */
  def obtainFingerprint(namePool: NamePool): Int = fingerprint

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
      if (n.hasFingerprint()) {
        getFingerprint == n.getFingerprint
      } else {
        n.getLocalPart == getLocalPart && n.hasURI(getURI)
      }
    }
    case _ => false

  }

  def isIdentical(other: IdentityComparable): Boolean =
    other.isInstanceOf[NodeName] && (this eq other) &&
      this.getPrefix == other.asInstanceOf[NodeName].getPrefix

  /**
    * Get a hashCode that offers the guarantee that if A.isIdentical(B), then A.identityHashCode() == B.identityHashCode()
    *
    * @return a hashCode suitable for use when testing for identity.
    */
  def identityHashCode(): Int = hashCode ^ getPrefix.hashCode

  override def toString(): String = getDisplayName

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
