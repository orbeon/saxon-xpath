package net.sf.saxon.functions

import net.sf.saxon.expr.XPathContext
import net.sf.saxon.om.{Item, Sequence, ZeroOrOne}
import net.sf.saxon.trans.XPathException
import net.sf.saxon.value.CalendarValue
import net.sf.saxon.value.DayTimeDurationValue

class Adjust_2 extends SystemFunction {

  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[Item] = {
    val in: CalendarValue = arguments(0).head.asInstanceOf[CalendarValue]
    if (in == null) {
      ZeroOrOne.empty().asInstanceOf[ZeroOrOne[Item]]
    } else {
      val tz: DayTimeDurationValue =
        arguments(1).head.asInstanceOf[DayTimeDurationValue]
      if (tz == null) {
        new ZeroOrOne(in.removeTimezone)
      } else {
        new ZeroOrOne(in.adjustTimezone(tz))
      }
    }
  }

}
