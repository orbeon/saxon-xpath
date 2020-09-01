package net.sf.saxon.functions

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.ZeroOrOne

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.DateTimeValue

import net.sf.saxon.value.DateValue

import net.sf.saxon.value.TimeValue

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
