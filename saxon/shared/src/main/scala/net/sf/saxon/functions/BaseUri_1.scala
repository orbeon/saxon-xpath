package net.sf.saxon.functions

import net.sf.saxon.expr.Callable
import net.sf.saxon.expr.XPathContext
import net.sf.saxon.om.{Item, NodeInfo, Sequence, ZeroOrOne}
import net.sf.saxon.trans.XPathException
import net.sf.saxon.value.AnyURIValue

class BaseUri_1 extends SystemFunction with Callable {

  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[Item] = {
    val node: NodeInfo = arguments(0).head().asInstanceOf[NodeInfo]
    if (node == null) return ZeroOrOne.empty().asInstanceOf[ZeroOrOne[Item]]
    val s: String = node.getBaseURI
    if (s == null) return ZeroOrOne.empty().asInstanceOf[ZeroOrOne[Item]]

    new ZeroOrOne(new AnyURIValue(s))
  }

  override def getCompilerName(): String = "BaseURICompiler"

}
