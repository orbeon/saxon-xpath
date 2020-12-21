package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.StaticProperty

import org.orbeon.saxon.expr.SystemFunctionCall

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.model.BuiltInAtomicType

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.AtomicValue

import org.orbeon.saxon.value.DateTimeValue

import org.orbeon.saxon.value.StringValue

object DynamicContextAccessor {

  class ImplicitTimezone extends DynamicContextAccessor {

    override def evaluate(context: XPathContext): AtomicValue = {
      val now: DateTimeValue = DateTimeValue.getCurrentDateTime(context)
      now.getComponent(AccessorFn.Component.TIMEZONE)
    }

  }

  class CurrentDateTime extends DynamicContextAccessor {

    override def evaluate(context: XPathContext): AtomicValue =
      DateTimeValue.getCurrentDateTime(context)

  }

  class CurrentDate extends DynamicContextAccessor {

    override def evaluate(context: XPathContext): AtomicValue = {
      val now: DateTimeValue = DateTimeValue.getCurrentDateTime(context)
      now.toDateValue
    }

  }

  class CurrentTime extends DynamicContextAccessor {

    override def evaluate(context: XPathContext): AtomicValue = {
      val now: DateTimeValue = DateTimeValue.getCurrentDateTime(context)
      now.toTimeValue
    }

  }

  class DefaultLanguage extends DynamicContextAccessor {

    override def evaluate(context: XPathContext): AtomicValue = {
      val lang: String = context.getConfiguration.getDefaultLanguage
      new StringValue(lang, BuiltInAtomicType.LANGUAGE)
    }

  }

}

abstract class DynamicContextAccessor extends SystemFunction {

  private var boundValue: AtomicValue = _

  def bindContext(context: XPathContext): Unit = {
    boundValue = evaluate(context)
  }

  def evaluate(context: XPathContext): AtomicValue

  def call(context: XPathContext, arguments: Array[Sequence]): AtomicValue =
    if (boundValue != null) {
      boundValue
    } else {
      evaluate(context)
    }

  def makeFunctionCall(arguments: Array[Expression]): Expression =
    new SystemFunctionCall(this, arguments) {
      override def evaluateItem(context: XPathContext): Item =
        evaluate(context)

      override def getIntrinsicDependencies: Int =
        StaticProperty.DEPENDS_ON_RUNTIME_ENVIRONMENT
    }

}
