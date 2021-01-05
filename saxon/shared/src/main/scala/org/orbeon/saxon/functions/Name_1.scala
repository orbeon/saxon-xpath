package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.functions.ScalarSystemFunction._
import org.orbeon.saxon.om.{Item, NodeInfo, ZeroOrOne}
import org.orbeon.saxon.value.StringValue


class Name_1 extends ScalarSystemFunction {

  def evaluate(item: Item, context: XPathContext): StringValue =
    StringValue.makeStringValue(item.asInstanceOf[NodeInfo].getDisplayName)

  override def resultWhenEmpty(): ZeroOrOne[StringValue] = ZERO_LENGTH_STRING

  override def getCompilerName: String = "NameCompiler"
}
