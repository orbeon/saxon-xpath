package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.om.ZeroOrOne

import org.orbeon.saxon.value.StringValue

import ScalarSystemFunction._

class Name_1 extends ScalarSystemFunction {

  override def evaluate(item: Item, context: XPathContext): StringValue =
    StringValue.makeStringValue(item.asInstanceOf[NodeInfo].getDisplayName)

  override def resultWhenEmpty(): ZeroOrOne[StringValue] = ZERO_LENGTH_STRING

  override def getCompilerName(): String = "NameCompiler"

}
