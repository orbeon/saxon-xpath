package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.om.{Sequence, ZeroOrOne}
import org.orbeon.saxon.value.{DateTimeValue, DateValue, TimeValue}

class DateTimeConstructor extends SystemFunction {

  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[DateTimeValue] = {
    val arg0 = arguments(0).head.asInstanceOf[DateValue]
    val arg1 = arguments(1).head.asInstanceOf[TimeValue]
    if (arg0 == null || arg1 == null)
      ZeroOrOne.empty
    else
      new ZeroOrOne(DateTimeValue.makeDateTimeValue(arg0, arg1))
  }

  override def getCompilerName: String = "DateTimeConstructorCompiler"
}
