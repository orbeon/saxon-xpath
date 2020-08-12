package net.sf.saxon.functions

import net.sf.saxon.expr.Callable

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.model.Type

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.om.One

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.ZeroOrOne

import net.sf.saxon.value.BooleanValue

import Nilled_1._

object Nilled_1 {

  private def getNilledProperty(node: NodeInfo): BooleanValue = {
    if (node == null || node.getNodeKind != Type.ELEMENT) {
       null
    }
    BooleanValue.get(node.isNilled)
  }

  def isNilled(node: NodeInfo): Boolean = {
    val b: BooleanValue = getNilledProperty(node)
    b != null && b.getBooleanValue
  }

}

class Nilled_1 extends SystemFunction with Callable {

  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[BooleanValue] = {
    val node: NodeInfo = arguments(0).head().asInstanceOf[NodeInfo]
    if (node == null || node.getNodeKind != Type.ELEMENT) {
      ZeroOrOne.empty()
    }
    One.bool(isNilled(node))
  }

}
