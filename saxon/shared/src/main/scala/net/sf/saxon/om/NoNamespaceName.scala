////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.om




/**
  * An implementation of NodeName for the common case of a name in no namespace
  */
class NoNamespaceName(private var localName: String) extends NodeName {

  private var nameCode: Int = -1

  def this(localName: String, nameCode: Int) = {
    this(localName)
    this.localName = localName
    this.nameCode = nameCode
  }

  /**
    * Get the prefix of the QName.
    *
    * @return the prefix. Returns the empty string if the name is unprefixed.
    */
  def getPrefix(): String = ""

  /**
    * Get the namespace URI of the QName.
    *
    * @return the URI. Returns the empty string to represent the no-namespace
    */
  def getURI(): String = ""

  /**
    * Get the local part of the QName
    *
    * @return the local part of the QName
    */
  def getLocalPart(): String = localName

  /**
    * Get the display name, that is the lexical QName in the form [prefix:]local-part
    *
    * @return the lexical QName
    */
  def getDisplayName(): String = localName

  /**
    * Get the name in the form of a StructuredQName
    *
    * @return the name in the form of a StructuredQName
    */
  def getStructuredQName(): StructuredQName =
    new StructuredQName("", "", getLocalPart)

  /**
    * Test whether this name is in a given namespace
    *
    * @param ns the namespace to be tested against
    * @return true if the name is in the specified namespace
    */
  def hasURI(ns: String): Boolean = ns.isEmpty

  /**
    * Get a {@link net.sf.saxon.om.NamespaceBinding} whose (prefix, uri) pair are the prefix and URI of this
    * node name
    *
    * @return the corresponding NamespaceBinding
    */
  def getNamespaceBinding(): NamespaceBinding =
    NamespaceBinding.DEFAULT_UNDECLARATION

  /**
    * Ask whether this node name representation has a known namecode and fingerprint
    *
    * @return true if the methods getFingerprint() and getNameCode() will
    *         return a result other than -1
    */
  def hasFingerprint(): Boolean = nameCode != -1

  /**
    * Get the fingerprint of this name if known. This method should not to any work to allocate
    * a fingerprint if none is already available
    *
    * @return the fingerprint if known; otherwise -1
    */
  def getFingerprint(): Int = nameCode & NamePool.FP_MASK

  /**
    * Get the nameCode of this name, allocating a new code from the name pool if necessary
    *
    * @param namePool the NamePool used to allocate the name
    * @return a nameCode for this name, newly allocated if necessary
    */
  def obtainFingerprint(namePool: NamePool): Int =
    if (nameCode == -1) {
      nameCode = namePool.allocateFingerprint("", localName)
      return nameCode
    } else {
      nameCode
    }

  /**
    * Returns a hash code value for the object.
    */
  override def hashCode(): Int = StructuredQName.computeHashCode("", localName)

  /**
    * Indicates whether some other object is "equal to" this one.
    */
  override def equals(obj: Any): Boolean =
    obj.isInstanceOf[NodeName] &&
      obj.asInstanceOf[NodeName].getLocalPart == localName &&
      obj.asInstanceOf[NodeName].hasURI("")

  override def toString(): String = localName

  /**
    * Determine whether two IdentityComparable objects are identical. This is a stronger
    * test than equality (even schema-equality); for example two dateTime values are not identical unless
    * they are in the same timezone.
    *
    * @param other the value to compare with
    * @return true if the two values are indentical, false otherwise
    */
  def isIdentical(other: IdentityComparable): Boolean =
    other.isInstanceOf[NodeName] && this == other && other
      .asInstanceOf[NodeName]
      .getPrefix
      .isEmpty

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
