package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.Item

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.om.One

import org.orbeon.saxon.om.ZeroOrOne

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.AnyURIValue

import org.orbeon.saxon.value.AtomicValue

class NamespaceUri_1 extends ScalarSystemFunction {

  override def evaluate(item: Item, context: XPathContext): AtomicValue = {
    val uri: String = item.asInstanceOf[NodeInfo].getURI
    new AnyURIValue(uri)
  }

  override def resultWhenEmpty(): ZeroOrOne[Item] = new One(new AnyURIValue(""))

  override def getCompilerName(): String = "NamespaceUriFnCompiler"

}
