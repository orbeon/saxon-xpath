////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.sxpath

import org.orbeon.saxon.expr.StaticContext

import org.orbeon.saxon.expr.instruct.SlotManager

import org.orbeon.saxon.om.NamespaceResolver

import org.orbeon.saxon.value.QNameValue




/**
  * This interface defines methods that must be provided when Saxon's free-standing XPath API is used.
  * The default implementation of this interface is {@link org.orbeon.saxon.sxpath.IndependentContext}, and
  * that implementation should be adequate for most purposes; but for extra customization, a user-written
  * implementation of this interface may be used instead.
  */
trait XPathStaticContext extends StaticContext {

  def setDefaultElementNamespace(uri: String): Unit

  def setNamespaceResolver(resolver: NamespaceResolver): Unit

  def declareVariable(qname: QNameValue): XPathVariable

  def declareVariable(namespaceURI: String, localName: String): XPathVariable

  def getStackFrameMap: SlotManager

  def isContextItemParentless: Boolean

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
