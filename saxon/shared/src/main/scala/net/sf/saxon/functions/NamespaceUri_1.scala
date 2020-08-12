package net.sf.saxon.functions

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.Item

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.om.One

import net.sf.saxon.om.ZeroOrOne

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AnyURIValue

import net.sf.saxon.value.AtomicValue

class NamespaceUri_1 extends ScalarSystemFunction {

  override def evaluate(item: Item, context: XPathContext): AtomicValue = {
    val uri: String = item.asInstanceOf[NodeInfo].getURI
    new AnyURIValue(uri)
  }

  override def resultWhenEmpty(): ZeroOrOne[Item] = new One(new AnyURIValue(""))

  override def getCompilerName(): String = "NamespaceUriFnCompiler"

}
