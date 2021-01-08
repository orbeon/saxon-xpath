////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.{Expression, Literal, XPathContext}
import org.orbeon.saxon.om.{GroundedValue, Sequence}
import org.orbeon.saxon.value.{BooleanValue, IntegerValue}


/**
  * A ConstantFunction is a zero-argument function that always delivers the same result, supplied
  * at the time the function is instantiated.
  */
object ConstantFunction {
  class True  extends ConstantFunction(BooleanValue.TRUE)
  class False extends ConstantFunction(BooleanValue.FALSE)
}

class ConstantFunction(var value: GroundedValue) extends SystemFunction {

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = value

  def makeFunctionCall(arguments: Array[Expression]): Expression =
    Literal.makeLiteral(value)

  override def getIntegerBounds: Array[IntegerValue] =
    value match {
      case integerValue: IntegerValue => Array(integerValue, integerValue)
      case _                          => null
    }
}
