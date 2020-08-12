package net.sf.saxon.functions

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.Item

import net.sf.saxon.om.ZeroOrOne

import net.sf.saxon.value.StringValue

class LowerCase extends ScalarSystemFunction {

  override def evaluate(arg: Item, context: XPathContext): StringValue =
    StringValue.makeStringValue(arg.getStringValue.toLowerCase())

  override def resultWhenEmpty(): ZeroOrOne[StringValue] = ScalarSystemFunction.ZERO_LENGTH_STRING

  override def getCompilerName(): String = "ForceCaseCompiler"

}
