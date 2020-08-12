////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.om

import net.sf.saxon.event.ProxyReceiver

import net.sf.saxon.event.Receiver

import net.sf.saxon.model.SchemaType

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException




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
