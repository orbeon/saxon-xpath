package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.One

import org.orbeon.saxon.om.ZeroOrOne

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.AtomicValue

import org.orbeon.saxon.value.Int64Value

import org.orbeon.saxon.value.IntegerValue

import org.orbeon.saxon.value.StringValue

class StringLength_1 extends ScalarSystemFunction {

  override def getIntegerBounds(): Array[IntegerValue] =
    Array(Int64Value.ZERO, Expression.MAX_STRING_LENGTH)

  override def resultWhenEmpty(): ZeroOrOne[_ <: Item] = One.integer(0)

  override def evaluate(arg: Item, context: XPathContext): AtomicValue =
    if (arg.isInstanceOf[StringValue]) {
      Int64Value.makeIntegerValue(
        arg.asInstanceOf[StringValue].getStringLength)
    } else {
      var s: CharSequence = null
      s = arg.getStringValueCS
      Int64Value.makeIntegerValue(StringValue.getStringLength(s))
    }

  override def getCompilerName(): String = "StringLengthCompiler"

}
