package net.sf.saxon.functions

import net.sf.saxon.expr.Expression
import net.sf.saxon.expr.SystemFunctionCall
import net.sf.saxon.expr.XPathContext
import net.sf.saxon.om.Item
import net.sf.saxon.om.One
import net.sf.saxon.om.Sequence
import net.sf.saxon.om.ZeroOrOne
import net.sf.saxon.value.{AtomicValue, StringValue}

object ScalarSystemFunction {

  val ZERO_LENGTH_STRING: One[StringValue] = One.string("")

}

abstract class ScalarSystemFunction extends SystemFunction {

  def evaluate(arg: Item, context: XPathContext): AtomicValue

  def resultWhenEmpty(): ZeroOrOne[_] = ZeroOrOne.empty()

  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[AtomicValue] = {
    val val0: Item = arguments(0).head
    if (val0 == null) {
      return resultWhenEmpty().asInstanceOf[ZeroOrOne[AtomicValue]]
    }
    new ZeroOrOne(evaluate(val0, context))
  }

  def makeFunctionCall(arguments: Array[Expression]): Expression = {
    val call: SystemFunctionCall = new SystemFunctionCall(this, arguments) {
      override def evaluateItem(context: XPathContext): AtomicValue = {
        var `val`: Item = getArg(0).evaluateItem(context)
        if (`val` == null) resultWhenEmpty().head.asInstanceOf[AtomicValue]
        else evaluate(`val`, context)
      }
    }
    call.setRetainedStaticContext(getRetainedStaticContext)
    call
  }

}
