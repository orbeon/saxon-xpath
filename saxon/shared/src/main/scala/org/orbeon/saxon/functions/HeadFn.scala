package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.FirstItemExpression

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.ZeroOrOne

class HeadFn extends SystemFunction {

  def makeFunctionCall(arguments: Array[Expression]): Expression =
    FirstItemExpression.makeFirstItemExpression(arguments(0))

  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[Item] = {
    val head: Item = arguments(0).head
    new ZeroOrOne(head)
  }

}
