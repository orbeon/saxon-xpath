////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Callable

import org.orbeon.saxon.expr.CardinalityCheckingIterator

import org.orbeon.saxon.expr.StaticProperty

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.expr.parser.RoleDiagnostic

import org.orbeon.saxon.om.LazySequence

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.SequenceIterator

import org.orbeon.saxon.trans.XPathException

import TreatFn._




object TreatFn {

  class ExactlyOne extends TreatFn {

    def getRequiredCardinality(): Int = StaticProperty.EXACTLY_ONE

    override def getErrorCodeForTypeErrors(): String = "FORG0005"

  }

  class OneOrMore extends TreatFn {

    def getRequiredCardinality(): Int = StaticProperty.ALLOWS_ONE_OR_MORE

    override def getErrorCodeForTypeErrors(): String = "FORG0004"

  }

  class ZeroOrOne extends TreatFn {

    def getRequiredCardinality(): Int = StaticProperty.ALLOWS_ZERO_OR_ONE

    override def getErrorCodeForTypeErrors(): String = "FORG0003"

  }

}

abstract class TreatFn extends SystemFunction {

  def getErrorCodeForTypeErrors: String

  def getRequiredCardinality: Int

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    var iterator: SequenceIterator = arguments(0).iterate()
    val card: Int = getRequiredCardinality
    val role: RoleDiagnostic = makeRoleDiagnostic()
    iterator = new CardinalityCheckingIterator(iterator, card, role, null)
    new LazySequence(iterator)
  }

  def makeRoleDiagnostic(): RoleDiagnostic = {
    val role: RoleDiagnostic = new RoleDiagnostic(
      RoleDiagnostic.FUNCTION,
      getFunctionName.getDisplayName,
      0)
    role.setErrorCode(getErrorCodeForTypeErrors)
    role
  }

  override def getStreamerName: String = "TreatFn"

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * This class supports the XPath 2.0 functions exactly-one(), one-or-more(), zero-or-one().
  * Because Saxon doesn't do strict static type checking, these are essentially identity
  * functions; the run-time type checking is done as part of the function call mechanism
  */
