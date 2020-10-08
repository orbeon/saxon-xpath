package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.{Callable, XPathContext}
import org.orbeon.saxon.om.{Item, Sequence, ZeroOrOne}
import org.orbeon.saxon.value.{BooleanValue, StringValue}

class CodepointEqual extends SystemFunction with Callable {

  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[Item] = {
    val op1 = arguments(0).head.asInstanceOf[StringValue]
    val op2 = arguments(1).head.asInstanceOf[StringValue]
    if (op1 == null || op2 == null)
      ZeroOrOne.empty
    else
      new ZeroOrOne(BooleanValue.get(op1.getStringValue == op2.getStringValue))
  }
}
