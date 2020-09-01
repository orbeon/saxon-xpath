////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

package net.sf.saxon.om

import scala.beans.BeanProperty




object FingerprintedQName {

  def fromClarkName(expandedName: String): FingerprintedQName = {
    var namespace: String = null
    var localName: String = null
    if (expandedName.charAt(0) == '{') {
      val closeBrace: Int = expandedName.indexOf('}')
      if (closeBrace < 0) {
        throw new IllegalArgumentException("No closing '}' in Clark name")
      }
      namespace = expandedName.substring(1, closeBrace)
      if (closeBrace == expandedName.length) {
        throw new IllegalArgumentException("Missing local part in Clark name")
      }
      localName = expandedName.substring(closeBrace + 1)
    } else {
      namespace = ""
      localName = expandedName
    }
    new FingerprintedQName("", namespace, localName)
  }

  def fromEQName(expandedName: String): FingerprintedQName = {
    var namespace: String = null
    var localName: String = null
    if (expandedName.startsWith("Q{")) {
      val closeBrace: Int = expandedName.indexOf('}', 2)
      if (closeBrace < 0) {
        throw new IllegalArgumentException("No closing '}' in EQName")
      }
      namespace = expandedName.substring(2, closeBrace)
      if (closeBrace == expandedName.length) {
        throw new IllegalArgumentException("Missing local part in EQName")
      }
      localName = expandedName.substring(closeBrace + 1)
    } else {
      namespace = ""
      localName = expandedName
    }
    new FingerprintedQName("", namespace, localName)
  }

}

/**
  * A QName triple (prefix, URI, local) with the additional ability to hold an integer fingerprint.
  * The integer fingerprint provides a fast way of checking equality. A FingerprintedQName makes sense
  * only in the context of a known NamePool, and instances must be compared only if they relate to the
  * same NamePool. The fingerprint is optional, and is used only if present.
  */
class FingerprintedQName
    extends NodeName {

  private var qName: StructuredQName = _


  @BeanProperty
  var fingerprint: Int = -1

  def this(prefix: String, uri: String, localName: String) = {
    this()
    qName = new StructuredQName(prefix, uri, localName)
  }

  def this(prefix: String, uri: String, localName: String, fingerprint: Int) = {
    this(prefix, uri, localName)
    qName = new StructuredQName(prefix, uri, localName)
    this.fingerprint = fingerprint
  }

  def this(prefix: String, uri: String, localName: String, pool: NamePool) = {
    this(prefix, uri, localName)
    qName = new StructuredQName(prefix, uri, localName)
    this.fingerprint = pool.allocateFingerprint(uri, localName)
  }

  def this(qName: StructuredQName, fingerprint: Int) = {
    this()
    this.qName = qName
    this.fingerprint = fingerprint
  }

  def this(qName: StructuredQName, pool: NamePool) = {
    this()
    this.qName = qName
    this.fingerprint =
      pool.allocateFingerprint(qName.getURI, qName.getLocalPart)
  }

  /**
    * Ask whether this node name representation has a known namecode and fingerprint
    *
    * @return true if the methods getFingerprint() and getNameCode() will
    * return a result other than -1
    */
  def hasFingerprint(): Boolean = fingerprint != -1

  def obtainFingerprint(pool: NamePool): Int = {
    if (fingerprint == -1) {
      fingerprint = pool.allocateFingerprint(getURI, getLocalPart)
    }
    fingerprint
  }

  /**
    * Get the display name, that is the lexical QName in the form [prefix:]local-part
    *
    * @return the lexical QName
    */
  def getDisplayName(): String = qName.getDisplayName

  /**
    * Get the prefix of the QName.
    *
    * @return the prefix. Returns the empty string if the name is unprefixed.
    */
  def getPrefix(): String = qName.getPrefix

  /**
    * Get the namespace URI of the QName.
    *
    * @return the URI. Returns the empty string to represent the no-namespace
    */
  def getURI(): String = qName.getURI

  /**
    * Get the local part of the QName
    *
    * @return the local part of the QName
    */
  def getLocalPart(): String = qName.getLocalPart

  /**
    * Get the name in the form of a StructuredQName
    *
    * @return the name in the form of a StructuredQName
    */
  def getStructuredQName(): StructuredQName = qName

  /**
    * Test whether this name is in a given namespace
    *
    * @param ns the namespace to be tested against
    * @return true if the name is in the specified namespace
    */
  def hasURI(ns: String): Boolean = qName.hasURI(ns)

  /**
    * Get a {@link NamespaceBinding} whose (prefix, uri) pair are the prefix and URI of this
    * node name
    *
    * @return the corresponding NamespaceBinding
    */
  def getNamespaceBinding(): NamespaceBinding = qName.getNamespaceBinding

  /**
    * Get a hashCode that offers the guarantee that if A.isIdentical(B), then A.identityHashCode() == B.identityHashCode()
    *
    * @return a hashCode suitable for use when testing for identity.
    */
  def identityHashCode(): Int = 0

  override def equals(other: Any): Boolean = other match {
    case other: NodeName =>
      if (fingerprint != -1 && other.hasFingerprint()) {
        getFingerprint == other.getFingerprint
      } else {
        getLocalPart == other.getLocalPart && hasURI(other.getURI)
      }
    case _ => false

  }

  /**
    * Returns a hash code value for the object.
    */
  override def hashCode(): Int = qName.hashCode

  /**
    * Determine whether two IdentityComparable objects are identical. This is a stronger
    * test than equality (even schema-equality); for example two dateTime values are not identical unless
    * they are in the same timezone.
    *
    * @param other the other value
    * @return true if the two values are indentical, false otherwise
    */
  def isIdentical(other: IdentityComparable): Boolean =
    other.isInstanceOf[NodeName] && (this eq other) &&
      this.getPrefix == other.asInstanceOf[NodeName].getPrefix

  override def toString: String = qName.getDisplayName

}

