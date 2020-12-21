package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Callable

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.om.NamespaceResolver

import org.orbeon.saxon.om.NodeInfo

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.ZeroOrOne

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.AnyURIValue

import org.orbeon.saxon.value.StringValue

import NamespaceForPrefix._

object NamespaceForPrefix {

  private def namespaceUriForPrefix(p: StringValue,
                                    element: NodeInfo): AnyURIValue = {
    var prefix: String = null
    prefix = if (p == null) "" else p.getStringValue
    val resolver: NamespaceResolver = element.getAllNamespaces
    val uri: String = resolver.getURIForPrefix(prefix, useDefault = true)
    if (uri == null || uri.isEmpty) {
      return null
    }
    new AnyURIValue(uri)
  }

}

class NamespaceForPrefix extends SystemFunction {

  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[AnyURIValue] = {
    val result: AnyURIValue = namespaceUriForPrefix(
      arguments(0).head.asInstanceOf[StringValue],
      arguments(1).head.asInstanceOf[NodeInfo])
    new ZeroOrOne(result)
  }

}
