package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Callable

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.model.Type

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.om.One

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.ZeroOrOne

import org.orbeon.saxon.value.BooleanValue

import Nilled_1._

object Nilled_1 {

  private def getNilledProperty(node: NodeInfo): BooleanValue = {
    if (node == null || node.getNodeKind != Type.ELEMENT) {
      return null
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
    val node: NodeInfo = arguments(0).head.asInstanceOf[NodeInfo]
    if (node == null || node.getNodeKind != Type.ELEMENT) {
      ZeroOrOne.empty()
    }
    One.bool(isNilled(node))
  }

}
