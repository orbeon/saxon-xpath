package net.sf.saxon.functions

import net.sf.saxon.expr.Callable
import net.sf.saxon.expr.XPathContext
import net.sf.saxon.om.{Item, Sequence, ZeroOrOne}
import net.sf.saxon.trans.XPathException
import net.sf.saxon.value.BooleanValue
import net.sf.saxon.value.StringValue

class CodepointEqual extends SystemFunction with Callable {

  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[Item] = {
    val op1: StringValue = arguments(0).head.asInstanceOf[StringValue]
    val op2: StringValue = arguments(1).head.asInstanceOf[StringValue]
    if (op1 == null || op2 == null)  return ZeroOrOne.empty().asInstanceOf[ZeroOrOne[Item]]
    new ZeroOrOne(BooleanValue.get(op1.getStringValue == op2.getStringValue))
  }

}
