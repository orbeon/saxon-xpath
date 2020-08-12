////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.functions

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.Literal

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.GroundedValue

import net.sf.saxon.om.Sequence

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.BooleanValue

import net.sf.saxon.value.IntegerValue

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
