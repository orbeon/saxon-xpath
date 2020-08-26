package net.sf.saxon.functions

import net.sf.saxon.utils.Configuration

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.StringLiteral

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.Sequence

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.StringValue

class RegexFunctionSansFlags extends SystemFunction {

  private def addFlagsArgument(): SystemFunction = {
    val config: Configuration = getRetainedStaticContext.getConfiguration
    val fixed: SystemFunction = new CollationKeyFn().asInstanceOf[SystemFunction]
      //config.makeSystemFunction(getFunctionName.getLocalPart, getArity + 1) // required changes in Configuration class
    fixed.setRetainedStaticContext(getRetainedStaticContext)
    fixed
  }

  override def makeFunctionCall(arguments: Expression*): Expression = {
    val withFlags: SystemFunction = addFlagsArgument()
    val newArgs: Array[Expression] =
      Array.ofDim[Expression](arguments.length + 1)
    System.arraycopy(arguments, 0, newArgs, 0, arguments.length)
    newArgs(arguments.length) = new StringLiteral("")
    withFlags.makeFunctionCall(newArgs.toIndexedSeq: _*)
  }

  def call(context: XPathContext, args: Array[Sequence]): Sequence = {
    val withFlags: SystemFunction = addFlagsArgument()
    val newArgs: Array[Sequence] = Array.ofDim[Sequence](args.length + 1)
    System.arraycopy(args, 0, newArgs, 0, args.length)
    newArgs(args.length) = StringValue.EMPTY_STRING
    withFlags.call(context, newArgs)
  }

}
