////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.Literal

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.GroundedValue

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.BooleanValue

import org.orbeon.saxon.value.IntegerValue

import ConstantFunction._




object ConstantFunction {

  class True extends ConstantFunction(BooleanValue.TRUE)

  class False extends ConstantFunction(BooleanValue.FALSE)

}

class ConstantFunction(var value: GroundedValue) extends SystemFunction {

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = value

  def makeFunctionCall(arguments: Array[Expression]): Expression =
    Literal.makeLiteral(value)

  override def getIntegerBounds(): Array[IntegerValue] =
    if (value.isInstanceOf[IntegerValue]) {
      Array(value.asInstanceOf[IntegerValue], value.asInstanceOf[IntegerValue])
    } else {
      null
    }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
  * A ConstantFunction is a zero-argument function that always delivers the same result, supplied
  * at the time the function is instantiated.
  */
