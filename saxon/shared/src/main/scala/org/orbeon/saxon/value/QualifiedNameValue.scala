////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.value

import org.orbeon.saxon.expr.sort.AtomicMatchKey

import org.orbeon.saxon.lib.ConversionRules

import org.orbeon.saxon.lib.StringCollator

import org.orbeon.saxon.model.AtomicType

import org.orbeon.saxon.model.BuiltInAtomicType

import org.orbeon.saxon.model.ValidationFailure

import org.orbeon.saxon.om.StandardNames

import org.orbeon.saxon.om.StructuredQName

import org.orbeon.saxon.trans.XPathException

import javax.xml.namespace.QName

import QualifiedNameValue._




object QualifiedNameValue {

  /*@Nullable*/

  def makeQName(prefix: String,
                uri: String,
                local: String,
                targetType: AtomicType,
                lexicalForm: CharSequence,
                rules: ConversionRules): AtomicValue =
    if (targetType.getFingerprint == StandardNames.XS_QNAME) {
      new QNameValue(prefix, uri, local, BuiltInAtomicType.QNAME, true)
    } else {
      var qnv: QualifiedNameValue = null
      qnv =
        if (targetType.getPrimitiveType == StandardNames.XS_QNAME)
          new QNameValue(prefix, uri, local, targetType, true)
        else new NotationValue(prefix, uri, local, null)
      val vf: ValidationFailure = targetType.validate(qnv, lexicalForm, rules)
      if (vf != null) {
        throw vf.makeException()
      }
      qnv.setTypeLabel(targetType)
      qnv
    }

}

abstract class QualifiedNameValue extends AtomicValue with AtomicMatchKey {

  /*@NotNull*/

   var qName: StructuredQName = _

  def getPrimitiveStringValue(): String = qName.getDisplayName

  def getClarkName: String = qName.getClarkName

  def getEQName: String = qName.getEQName

  /*@NotNull*/

  def getLocalName: String = qName.getLocalPart

  /*@NotNull*/

  def getNamespaceURI: String = qName.getURI

  /*@NotNull*/

  def getPrefix: String = qName.getPrefix

  /*@Nullable*/

  def getXPathComparable(ordered: Boolean,
                         collator: StringCollator,
                         implicitTimezone: Int): AtomicMatchKey =
    if (ordered) null else this

  override def hashCode: Int = qName.hashCode

  override def isIdentical(v: AtomicValue): Boolean =
    super.isIdentical(v) &&
      qName.getPrefix == v.asInstanceOf[QualifiedNameValue].getPrefix

  /**
    * Get a hashCode that offers the guarantee that if A.isIdentical(B), then A.identityHashCode() == B.identityHashCode()
    *
    * @return a hashCode suitable for use when testing for identity.
    */
  override def identityHashCode(): Int = qName.identityHashCode()

  /*@NotNull*/

  override def toString: String =
    "QName(\"" + getNamespaceURI + "\", \"" + getLocalName +
      "\")"

  def toJaxpQName: QName = qName.toJaxpQName

  /*@NotNull*/

  def getStructuredQName: StructuredQName = qName

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A qualified name: this is an abstract superclass for QNameValue and NotationValue, representing the
  * XPath primitive types xs:QName and xs:NOTATION respectively
  */
