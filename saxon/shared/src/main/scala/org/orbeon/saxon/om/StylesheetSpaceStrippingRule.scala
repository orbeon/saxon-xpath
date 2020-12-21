////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.om

import org.orbeon.saxon.event.ProxyReceiver

import org.orbeon.saxon.event.Receiver

import org.orbeon.saxon.event.Stripper

import org.orbeon.saxon.model.SchemaType

import org.orbeon.saxon.trace.ExpressionPresenter

import java.util.Arrays

import StylesheetSpaceStrippingRule._




object StylesheetSpaceStrippingRule {

  private val specials: Array[Int] = Array(
    StandardNames.XSL_ANALYZE_STRING,
    StandardNames.XSL_APPLY_IMPORTS,
    StandardNames.XSL_APPLY_TEMPLATES,
    StandardNames.XSL_ATTRIBUTE_SET,
    StandardNames.XSL_CALL_TEMPLATE,
    StandardNames.XSL_CHARACTER_MAP,
    StandardNames.XSL_CHOOSE,
    StandardNames.XSL_EVALUATE,
    StandardNames.XSL_MERGE,
    StandardNames.XSL_MERGE_SOURCE,
    StandardNames.XSL_NEXT_ITERATION,
    StandardNames.XSL_NEXT_MATCH,
    StandardNames.XSL_STYLESHEET,
    StandardNames.XSL_TRANSFORM
  )

}

class StylesheetSpaceStrippingRule(pool: NamePool) extends SpaceStrippingRule {

  private var namePool: NamePool = pool

  def isSpacePreserving(elementName: NodeName, schemaType: SchemaType): Int = {
    val fingerprint: Int = elementName.obtainFingerprint(namePool)
    if (fingerprint == (StandardNames.XSL_TEXT & NamePool.FP_MASK)) {
      Stripper.ALWAYS_PRESERVE
    }
    if (Arrays.binarySearch(specials, fingerprint) >= 0) {
      Stripper.ALWAYS_STRIP
    }
    Stripper.STRIP_DEFAULT
  }

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
  def export(presenter: ExpressionPresenter): Unit = ()
// no action
// no action

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A whitespace stripping rule that strips whitespace according to the rules defined for XSLT stylesheets
  */
