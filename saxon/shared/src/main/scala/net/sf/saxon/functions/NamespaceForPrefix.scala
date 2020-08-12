package net.sf.saxon.functions

import net.sf.saxon.expr.Callable

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.om.NamespaceResolver

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.ZeroOrOne

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AnyURIValue

import net.sf.saxon.value.StringValue

import NamespaceForPrefix._

object NamespaceForPrefix {

  private def namespaceUriForPrefix(p: StringValue,
                                    element: NodeInfo): AnyURIValue = {
    var prefix: String = null
    prefix = if (p == null) "" else p.getStringValue
    val resolver: NamespaceResolver = element.getAllNamespaces
    val uri: String = resolver.getURIForPrefix(prefix, true)
    if (uri == null || uri.isEmpty) {
      null
    }
    new AnyURIValue(uri)
  }

}

class NamespaceForPrefix extends SystemFunction with Callable {

  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[AnyURIValue] = {
    val result: AnyURIValue = namespaceUriForPrefix(
      arguments(0).head().asInstanceOf[StringValue],
      arguments(1).head().asInstanceOf[NodeInfo])
    new ZeroOrOne(result)
  }

}
