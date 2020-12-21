////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.om

import org.orbeon.saxon.event.ProxyReceiver

import org.orbeon.saxon.event.Receiver

import org.orbeon.saxon.model.SchemaType

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trans.XPathException




trait SpaceStrippingRule {

  def isSpacePreserving(nodeName: NodeName, schemaType: SchemaType): Int

  def makeStripper(next: Receiver): ProxyReceiver

  def export(presenter: ExpressionPresenter): Unit

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * Interface for deciding whether a particular element is to have whitespace text nodes stripped
  */
