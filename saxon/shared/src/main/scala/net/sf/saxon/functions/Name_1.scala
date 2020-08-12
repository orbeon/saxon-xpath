package net.sf.saxon.functions

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.Item

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.om.ZeroOrOne

import net.sf.saxon.value.StringValue

import ScalarSystemFunction._

class Name_1 extends ScalarSystemFunction {

  override def evaluate(item: Item, context: XPathContext): StringValue =
    StringValue.makeStringValue(item.asInstanceOf[NodeInfo].getDisplayName)

  override def resultWhenEmpty(): ZeroOrOne[StringValue] = ZERO_LENGTH_STRING

  override def getCompilerName(): String = "NameCompiler"

}
