////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.expr.sort

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.lib.StringCollator

import org.orbeon.saxon.trans.NoDynamicContextException

import org.orbeon.saxon.value.AtomicValue




trait AtomicComparer {

  def getCollator: StringCollator

  def provideContext(context: XPathContext): AtomicComparer

  def compareAtomicValues(a: AtomicValue, b: AtomicValue): Int

  def comparesEqual(a: AtomicValue, b: AtomicValue): Boolean

  def save(): String

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Interface representing an object that can be used to compare two XPath atomic values for equality or
  * for ordering.
  */
