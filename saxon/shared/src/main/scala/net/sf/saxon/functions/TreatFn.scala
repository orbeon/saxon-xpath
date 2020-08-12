////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.functions

import net.sf.saxon.expr.Callable

import net.sf.saxon.expr.CardinalityCheckingIterator

import net.sf.saxon.expr.StaticProperty

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.parser.RoleDiagnostic

import net.sf.saxon.om.LazySequence

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.SequenceIterator

import net.sf.saxon.trans.XPathException

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

abstract class TreatFn extends SystemFunction with Callable {

  def getErrorCodeForTypeErrors(): String

  def getRequiredCardinality(): Int

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

  override def getStreamerName(): String = "TreatFn"

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
