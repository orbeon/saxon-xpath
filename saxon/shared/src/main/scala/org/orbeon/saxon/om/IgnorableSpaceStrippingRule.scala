////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.om

import org.orbeon.saxon.event.IgnorableWhitespaceStripper

import org.orbeon.saxon.event.ProxyReceiver

import org.orbeon.saxon.event.Receiver

import org.orbeon.saxon.event.Stripper

import org.orbeon.saxon.model.ComplexType

import org.orbeon.saxon.model.SchemaType

import org.orbeon.saxon.model.Untyped

import org.orbeon.saxon.trace.ExpressionPresenter

import IgnorableSpaceStrippingRule._




object IgnorableSpaceStrippingRule {

  private val THE_INSTANCE: IgnorableSpaceStrippingRule =
    new IgnorableSpaceStrippingRule()

  def getInstance: IgnorableSpaceStrippingRule = THE_INSTANCE

}

class IgnorableSpaceStrippingRule extends SpaceStrippingRule {

  def isSpacePreserving(name: NodeName, schemaType: SchemaType): Int =
    if (schemaType != Untyped.getInstance && schemaType.isComplexType &&
        !schemaType.asInstanceOf[ComplexType].isSimpleContent &&
        !schemaType.asInstanceOf[ComplexType].isMixedContent) {
      Stripper.ALWAYS_STRIP
    } else {
      Stripper.ALWAYS_PRESERVE
    }

  /**
    * Make a filter to implement these space-stripping rules, or null if no filtering
    * is necessary
    *
    * @return a filter in the form of a ProxyReceiver, or null
    * @param next
    */
  override def makeStripper(next: Receiver): ProxyReceiver =
    new IgnorableWhitespaceStripper(next)

  /**
    * Export this rule as part of an exported stylesheet
    *
    * @param presenter the output handler
    */
  def export(presenter: ExpressionPresenter): Unit = {
    presenter.startElement("strip.ignorable")
    presenter.endElement()
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A whitespace stripping rule that strips whitespace text node children from all elements having an element-only content
  * model, regardless of the value of the xml:space attribute
  */
