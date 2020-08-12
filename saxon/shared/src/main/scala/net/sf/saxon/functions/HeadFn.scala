package net.sf.saxon.functions

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.FirstItemExpression

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.Item

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.ZeroOrOne

class HeadFn extends SystemFunction {

  def makeFunctionCall(arguments: Array[Expression]): Expression =
    FirstItemExpression.makeFirstItemExpression(arguments(0))

  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[Item] = {
    val head: Item = arguments(0).head()
    new ZeroOrOne(head)
  }

}
