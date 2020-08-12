package net.sf.saxon.functions

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.Item

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.om.ZeroOrOne

import ScalarSystemFunction._

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.StringValue


class LocalName_1 extends ScalarSystemFunction {

  override def evaluate(item: Item, context: XPathContext): AtomicValue =
    StringValue.makeStringValue(item.asInstanceOf[NodeInfo].getLocalPart)

  override def resultWhenEmpty(): ZeroOrOne[StringValue] = ZERO_LENGTH_STRING

  override def getCompilerName(): String = "LocalNameCompiler"

}
