package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.One

import org.orbeon.saxon.value.AtomicValue

import org.orbeon.saxon.value.StringValue

class UpperCase extends ScalarSystemFunction {

  override def evaluate(arg: Item, context: XPathContext): AtomicValue =
    StringValue.makeStringValue(arg.getStringValue.toUpperCase())

  override def resultWhenEmpty(): One[StringValue] = ScalarSystemFunction.ZERO_LENGTH_STRING

  override def getCompilerName(): String = "ForceCaseCompiler"

}
