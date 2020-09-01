////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.om

import net.sf.saxon.event.ProxyReceiver

import net.sf.saxon.event.Receiver

import net.sf.saxon.event.Stripper

import net.sf.saxon.model.SchemaType

import net.sf.saxon.trace.ExpressionPresenter

import AllElementsSpaceStrippingRule._




object AllElementsSpaceStrippingRule {

  private val THE_INSTANCE: AllElementsSpaceStrippingRule =
    new AllElementsSpaceStrippingRule()

  def getInstance: AllElementsSpaceStrippingRule = THE_INSTANCE

}

class AllElementsSpaceStrippingRule extends SpaceStrippingRule {

  def isSpacePreserving(fingerprint: NodeName, schemaType: SchemaType): Int =
    Stripper.STRIP_DEFAULT

  /**
    * Make a filter to implement these space-stripping rules, or null if no filtering
    * is necessary
    *
    * @return a filter in the form of a ProxyReceiver, or null
    * @param next
    */
  override def makeStripper(next: Receiver): ProxyReceiver =
    new Stripper(this, next)

  /**
    * Export this rule as part of an exported stylesheet
    *
    * @param presenter the output handler
    */
  def export(presenter: ExpressionPresenter): Unit = {
    presenter.startElement("strip.all")
    presenter.endElement()
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A whitespace stripping rule that strips all elements unless xml:space indicates that whitespace
  * should be preserved.
  */
