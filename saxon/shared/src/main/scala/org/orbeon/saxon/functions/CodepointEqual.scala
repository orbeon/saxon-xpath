package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Callable
import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.om.{Item, Sequence, ZeroOrOne}
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.value.BooleanValue
import org.orbeon.saxon.value.StringValue

class CodepointEqual extends SystemFunction with Callable {

  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[Item] = {
    val op1: StringValue = arguments(0).head.asInstanceOf[StringValue]
    val op2: StringValue = arguments(1).head.asInstanceOf[StringValue]
    if (op1 == null || op2 == null)  return ZeroOrOne.empty().asInstanceOf[ZeroOrOne[Item]]
    new ZeroOrOne(BooleanValue.get(op1.getStringValue == op2.getStringValue))
  }

}
