package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.{Callable, XPathContext}
import org.orbeon.saxon.functions.Nilled_1._
import org.orbeon.saxon.model.Type
import org.orbeon.saxon.om.{NodeInfo, One, Sequence, ZeroOrOne}
import org.orbeon.saxon.value.BooleanValue

object Nilled_1 {

  private def getNilledProperty(node: NodeInfo): BooleanValue = {
    if (node == null || node.getNodeKind != Type.ELEMENT)
      return null
    BooleanValue.get(node.isNilled)
  }

  def isNilled(node: NodeInfo): Boolean = {
    val b = getNilledProperty(node)
    b != null && b.getBooleanValue
  }
}

class Nilled_1 extends SystemFunction with Callable {

  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[BooleanValue] = {
    val node = arguments(0).head.asInstanceOf[NodeInfo]
    if (node == null || node.getNodeKind != Type.ELEMENT)
      ZeroOrOne.empty
    else
      One.bool(isNilled(node))
  }
}
