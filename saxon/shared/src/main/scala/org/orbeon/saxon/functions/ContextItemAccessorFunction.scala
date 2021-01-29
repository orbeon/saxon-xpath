package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Callable

import org.orbeon.saxon.expr.ContextItemExpression

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.model.FunctionItemType

import org.orbeon.saxon.model.SpecificFunctionType

import org.orbeon.saxon.om._

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.DoubleValue

import org.orbeon.saxon.value.SequenceType

import org.orbeon.saxon.value.StringValue

object ContextItemAccessorFunction {

  class StringAccessor extends ContextItemAccessorFunction {

    override def makeFunctionCall(arguments: Array[Expression]): Expression = {
      val ci: Expression = new ContextItemExpression()
      val sv: Expression =
        SystemFunction.makeCall("string", getRetainedStaticContext, ci)
      SystemFunction.makeCall(getFunctionName.getLocalPart,
        getRetainedStaticContext,
        sv)
    }

    override def evaluate(item: Item, context: XPathContext): GroundedValue = {
      val f: SystemFunction = SystemFunction.makeFunction(
        getDetails.name.getLocalPart,
        getRetainedStaticContext,
        1)
      val `val`: StringValue = new StringValue(item.getStringValueCS)
      f.call(context, Array(`val`)).materialize
    }

  }

  class Number_0 extends ContextItemAccessorFunction {

    override def makeFunctionCall(arguments: Array[Expression]): Expression = {
      val ci: Expression = new ContextItemExpression()
      val sv: Expression =
        SystemFunction.makeCall("data", getRetainedStaticContext, ci)
      SystemFunction.makeCall(getFunctionName.getLocalPart,
        getRetainedStaticContext,
        sv)
    }

    override def evaluate(item: Item, context: XPathContext): GroundedValue = {
      val f: SystemFunction = SystemFunction.makeFunction(
        getDetails.name.getLocalPart,
        getRetainedStaticContext,
        1)
      val `val`: AtomicSequence = item.atomize()
      `val`.getLength match {
        case 0 => DoubleValue.NaN
        case 1 => f.call(context, Array(`val`.head)).materialize
        case _ =>
          var err: XPathException = new XPathException(
            "When number() is called with no arguments, the atomized value of the context node must " +
              "not be a sequence of several atomic values",
            "XPTY0004")
          err.setIsTypeError(true)
          throw err

      }
    }

  }

}

class ContextItemAccessorFunction extends ContextAccessorFunction {

  def bindContext(context: XPathContext): Function = {
    val ci = context.getContextItem
    if (ci == null) {
      val callable: Callable = (context1, arguments) =>
        throw new XPathException(
          "Context item for " + getFunctionName.getDisplayName +
            " is absent",
          "XPDY0002")
      val fit = new SpecificFunctionType(Array(), SequenceType.ANY_SEQUENCE)
      return new CallableFunction(0, callable, fit)
    }
    val fn = new ConstantFunction(evaluate(ci, context))
    fn.setDetails(getDetails)
    fn.setRetainedStaticContext(getRetainedStaticContext)
    fn
  }

  def evaluate(item: Item, context: XPathContext): GroundedValue = {
    val f = SystemFunction.makeFunction(
      getDetails.name.getLocalPart,
      getRetainedStaticContext,
      1)
    f.call(context, Array(item)).materialize
  }

  override def call(context: XPathContext, arguments: Array[Sequence]): Sequence =
    evaluate(context.getContextItem, context)

  def makeFunctionCall(arguments: Array[Expression]): Expression = {
    val arg = new ContextItemExpression
    SystemFunction.makeCall(getFunctionName.getLocalPart,
      getRetainedStaticContext,
      arg)
  }

  def makeContextItemExplicit(): Expression = {
    val args = Array(new ContextItemExpression())
    SystemFunction.makeCall(getFunctionName.getLocalPart,
      getRetainedStaticContext,
      args.toIndexedSeq: _*)
  }
}
