package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.om.{Item, Sequence, ZeroOrOne}
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.value.CalendarValue
import org.orbeon.saxon.value.DayTimeDurationValue

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
