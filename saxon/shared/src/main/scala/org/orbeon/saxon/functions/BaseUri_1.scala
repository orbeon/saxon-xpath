package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Callable
import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.om.{Item, NodeInfo, Sequence, ZeroOrOne}
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.value.AnyURIValue

class BaseUri_1 extends SystemFunction with Callable {

  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[Item] = {
    val node: NodeInfo = arguments(0).head.asInstanceOf[NodeInfo]
    if (node == null) return ZeroOrOne.empty().asInstanceOf[ZeroOrOne[Item]]
    val s: String = node.getBaseURI
    if (s == null) return ZeroOrOne.empty().asInstanceOf[ZeroOrOne[Item]]

    new ZeroOrOne(new AnyURIValue(s))
  }

  override def getCompilerName(): String = "BaseURICompiler"

}
