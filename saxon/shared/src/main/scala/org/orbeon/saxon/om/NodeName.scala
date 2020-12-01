////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.om


/**
  * This interface represents a node name. Actually it represents any QName, but it is intended for use
  * as an element or attribute name. Various implementations are available.
  *
  * An important requirement of an implementation of this interface is that the hashCode and
  * equals() methods are implemented correctly, so that any two node names compare equal if and only
  * if the local name and namespace URI parts are equal under Unicode codepoint comparison. To ensure this,
  * the hashCode must be computed using an algorithm equivalent to that used by the implementation class
  * {@link FingerprintedQName}
  *
  * This class is used to carry name information for elements and attributes on the Receiver pipeline.
  * An advantage of this is that NodeName can include a fingerprint where this is available, but the fingerprint
  * does not need to be computed if it is not needed. For example, names of nodes constructed by an XSLT
  * stylesheet and sent down an output pipeline to a Serializer will generally not have fingerprints.
  *
  * Equality comparisons between NodeNames work reasonably well: the comparison can use the fingerprints
  * if available, or the QNames otherwise. However, computing hashCodes is inefficient; it is not possible
  * to take advantage of the fingerprints, because they are not always there. Therefore, using NodeName
  * objects in structures such as maps and sets is generally a bad idea: it's better to use either the
  * StructuredQName or the fingerprint as the key.
  */
trait NodeName extends IdentityComparable {
  def getPrefix: String
  def getURI: String
  def getLocalPart: String
  def getDisplayName: String
  def getStructuredQName: StructuredQName
  def hasURI(ns: String): Boolean
  def getNamespaceBinding: NamespaceBinding
  def hasFingerprint: Boolean
  def getFingerprint: Int
  def obtainFingerprint(namePool: NamePool): Int
}
