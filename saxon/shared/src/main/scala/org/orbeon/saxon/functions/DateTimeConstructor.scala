package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.ZeroOrOne

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.DateTimeValue

import org.orbeon.saxon.value.DateValue

import org.orbeon.saxon.value.TimeValue

class DateTimeConstructor extends SystemFunction {

  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[DateTimeValue] = {
    val arg0: DateValue = arguments(0).head.asInstanceOf[DateValue]
    val arg1: TimeValue = arguments(1).head.asInstanceOf[TimeValue]
    if (arg0 == null || arg1 == null) {
      return ZeroOrOne.empty().asInstanceOf[ZeroOrOne[DateTimeValue]]
    }
    new ZeroOrOne(DateTimeValue.makeDateTimeValue(arg0, arg1))
  }

  override def getCompilerName(): String = "DateTimeConstructorCompiler"

}
