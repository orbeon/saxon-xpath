package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.model.BuiltInAtomicType

import org.orbeon.saxon.om._

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.AtomicValue

import org.orbeon.saxon.value.QNameValue

import ResolveQName._

object ResolveQName {

  def resolveQName(lexicalQName: CharSequence, element: NodeInfo): QNameValue = {
    val resolver: NamespaceResolver = element.getAllNamespaces
    val qName: StructuredQName =
      StructuredQName.fromLexicalQName(lexicalQName, useDefault = true, allowEQName = false, resolver)
    new QNameValue(qName, BuiltInAtomicType.QNAME)
  }

}

class ResolveQName extends SystemFunction {

  def call(context: XPathContext, arguments: Array[Sequence]): ZeroOrOne[QNameValue] = {
    val lex: AtomicValue = arguments(0).head.asInstanceOf[AtomicValue]
    new ZeroOrOne(
      if (lex == null) null
      else
        resolveQName(lex.getStringValueCS,
          arguments(1).head.asInstanceOf[NodeInfo]))
  }

}
