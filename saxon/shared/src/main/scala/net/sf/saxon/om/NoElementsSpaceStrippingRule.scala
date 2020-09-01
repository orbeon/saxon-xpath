////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.om

import net.sf.saxon.event.ProxyReceiver

import net.sf.saxon.event.Receiver

import net.sf.saxon.event.Stripper

import net.sf.saxon.model.SchemaType

import net.sf.saxon.trace.ExpressionPresenter

import NoElementsSpaceStrippingRule._




object NoElementsSpaceStrippingRule {

  private val THE_INSTANCE: NoElementsSpaceStrippingRule =
    new NoElementsSpaceStrippingRule()

  def getInstance: NoElementsSpaceStrippingRule = THE_INSTANCE

}

class NoElementsSpaceStrippingRule extends SpaceStrippingRule {

  def isSpacePreserving(fingerprint: NodeName, schemaType: SchemaType): Int =
    Stripper.ALWAYS_PRESERVE

  /**
    * Make a filter to implement these space-stripping rules, or null if no filtering
    * is necessary
    *
    * @param next the Receiver that is to receiver the filtered event stream
    * @return a filter in the form of a ProxyReceiver, or null
    */
  override def makeStripper(next: Receiver): ProxyReceiver = null

  /**
    * Export this rule as part of an exported stylesheet
    *
    * @param presenter the output handler
    */
  def export(presenter: ExpressionPresenter): Unit = {
    presenter.startElement("strip.none")
    presenter.endElement()
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A whitespace stripping rule that retains all whitespace text nodes
  */
