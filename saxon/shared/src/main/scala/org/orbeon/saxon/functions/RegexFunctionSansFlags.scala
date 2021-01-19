package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.{Expression, StringLiteral, XPathContext}
import org.orbeon.saxon.om.Sequence
import org.orbeon.saxon.value.StringValue


class RegexFunctionSansFlags extends SystemFunction {

  private def addFlagsArgument(): SystemFunction = {
    val config = getRetainedStaticContext.getConfiguration
//    val fixed  = new CollationKeyFn().asInstanceOf[SystemFunction]
    val fixed = config.makeSystemFunction(getFunctionName.getLocalPart, getArity + 1)
    fixed.setRetainedStaticContext(getRetainedStaticContext)
    fixed
  }

  override def makeFunctionCall(arguments: Expression*): Expression = {
    val withFlags = addFlagsArgument()
    val newArgs   = Array.ofDim[Expression](arguments.length + 1)
    System.arraycopy(arguments.toArray, 0, newArgs, 0, arguments.length)
    newArgs(arguments.length) = new StringLiteral("")
    withFlags.makeFunctionCall(newArgs: _*)
  }

  def call(context: XPathContext, args: Array[Sequence]): Sequence = {
    val withFlags = addFlagsArgument()
    val newArgs   = Array.ofDim[Sequence](args.length + 1)
    System.arraycopy(args, 0, newArgs, 0, args.length)
    newArgs(args.length) = StringValue.EMPTY_STRING
    withFlags.call(context, newArgs)
  }
}
