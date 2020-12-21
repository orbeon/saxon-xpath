package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.ZeroOrOne

import org.orbeon.saxon.value.StringValue

class LowerCase extends ScalarSystemFunction {

  override def evaluate(arg: Item, context: XPathContext): StringValue =
    StringValue.makeStringValue(arg.getStringValue.toLowerCase())

  override def resultWhenEmpty(): ZeroOrOne[StringValue] = ScalarSystemFunction.ZERO_LENGTH_STRING

  override def getCompilerName(): String = "ForceCaseCompiler"

}
