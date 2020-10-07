package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Expression
import org.orbeon.saxon.expr.StaticProperty
import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.om.{NodeInfo, Sequence, ZeroOrOne}

class Root_1 extends SystemFunction {

  override def getSpecialProperties(arguments: Array[Expression]): Int = {
    var prop: Int = StaticProperty.ORDERED_NODESET | StaticProperty.SINGLE_DOCUMENT_NODESET |
      StaticProperty.NO_NODES_NEWLY_CREATED
    if ((getArity == 0) ||
      (arguments(0).getSpecialProperties & StaticProperty.CONTEXT_DOCUMENT_NODESET) !=
        0) {
      prop |= StaticProperty.CONTEXT_DOCUMENT_NODESET
    }
    prop
  }

  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[NodeInfo] = {
    val node: NodeInfo = arguments(0).head.asInstanceOf[NodeInfo]
    if (node == null) {
      ZeroOrOne.empty().asInstanceOf[ZeroOrOne[NodeInfo]]
    } else {
      new ZeroOrOne(node.getRoot)
    }
  }

  override def getStreamerName: String = "Root"

  override def getCompilerName(): String = "RootFunctionCompiler"

}
