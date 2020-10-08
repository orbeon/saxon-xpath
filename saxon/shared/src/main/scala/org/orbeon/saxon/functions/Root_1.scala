package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.{Expression, StaticProperty, XPathContext}
import org.orbeon.saxon.om.{NodeInfo, Sequence, ZeroOrOne}

class Root_1 extends SystemFunction {

  override def getSpecialProperties(arguments: Array[Expression]): Int = {
    var prop = StaticProperty.ORDERED_NODESET | StaticProperty.SINGLE_DOCUMENT_NODESET | StaticProperty.NO_NODES_NEWLY_CREATED
    if ((getArity == 0) || (arguments(0).getSpecialProperties & StaticProperty.CONTEXT_DOCUMENT_NODESET) != 0)
      prop |= StaticProperty.CONTEXT_DOCUMENT_NODESET
    prop
  }

  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[NodeInfo] = {
    val node = arguments(0).head.asInstanceOf[NodeInfo]
    if (node == null)
      ZeroOrOne.empty
    else
      new ZeroOrOne(node.getRoot)
  }

  override def getStreamerName: String = "Root"
  override def getCompilerName: String = "RootFunctionCompiler"
}
