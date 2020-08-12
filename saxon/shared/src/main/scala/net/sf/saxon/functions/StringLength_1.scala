package net.sf.saxon.functions

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.Item

import net.sf.saxon.om.One

import net.sf.saxon.om.ZeroOrOne

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.Int64Value

import net.sf.saxon.value.IntegerValue

import net.sf.saxon.value.StringValue

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
