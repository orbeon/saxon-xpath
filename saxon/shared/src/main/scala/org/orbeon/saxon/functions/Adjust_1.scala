package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.om.{Item, Sequence, ZeroOrOne}
import org.orbeon.saxon.value.CalendarValue


class Adjust_1 extends SystemFunction {

  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[Item] = {
    val in = arguments(0).head.asInstanceOf[CalendarValue]
    if (in == null)
      ZeroOrOne.empty.asInstanceOf[ZeroOrOne[Item]]
    else
      new ZeroOrOne(in.adjustTimezone(context.getImplicitTimezone))
  }
}
