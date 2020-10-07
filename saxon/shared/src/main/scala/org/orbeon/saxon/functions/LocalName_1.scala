package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.om.ZeroOrOne

import ScalarSystemFunction._

import org.orbeon.saxon.value.AtomicValue

import org.orbeon.saxon.value.StringValue


class LocalName_1 extends ScalarSystemFunction {

  override def evaluate(item: Item, context: XPathContext): AtomicValue =
    StringValue.makeStringValue(item.asInstanceOf[NodeInfo].getLocalPart)

  override def resultWhenEmpty(): ZeroOrOne[StringValue] = ZERO_LENGTH_STRING

  override def getCompilerName(): String = "LocalNameCompiler"

}
