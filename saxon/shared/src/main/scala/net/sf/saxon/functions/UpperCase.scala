package net.sf.saxon.functions

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.Item

import net.sf.saxon.om.One

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.StringValue

class UpperCase extends ScalarSystemFunction {

  override def evaluate(arg: Item, context: XPathContext): AtomicValue =
    StringValue.makeStringValue(arg.getStringValue.toUpperCase())

  override def resultWhenEmpty(): One[StringValue] = ScalarSystemFunction.ZERO_LENGTH_STRING

  override def getCompilerName(): String = "ForceCaseCompiler"

}
