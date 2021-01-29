package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.om.{Item, NodeInfo, One, ZeroOrOne}
import org.orbeon.saxon.value.{AnyURIValue, AtomicValue}


class NamespaceUri_1 extends ScalarSystemFunction {

  def evaluate(item: Item, context: XPathContext): AtomicValue = {
    val uri = item.asInstanceOf[NodeInfo].getURI
    new AnyURIValue(uri)
  }

  override def resultWhenEmpty(): ZeroOrOne[Item] = new One(new AnyURIValue(""))
  override def getCompilerName: String = "NamespaceUriFnCompiler"
}
