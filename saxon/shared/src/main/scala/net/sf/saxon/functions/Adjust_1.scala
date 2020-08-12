package net.sf.saxon.functions

import net.sf.saxon.expr.XPathContext
import net.sf.saxon.om.{Item, Sequence, ZeroOrOne}
import net.sf.saxon.trans.XPathException
import net.sf.saxon.value.CalendarValue


class Adjust_1 extends SystemFunction {

  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[Item] = {
    val in: CalendarValue = arguments(0).head().asInstanceOf[CalendarValue]
    if (in == null) {
      ZeroOrOne.empty().asInstanceOf[ZeroOrOne[Item]]
    } else {
      new ZeroOrOne(in.adjustTimezone(context.getImplicitTimezone))
    }
  }

}
