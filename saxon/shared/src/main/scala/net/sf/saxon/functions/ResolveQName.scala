package net.sf.saxon.functions

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.model.BuiltInAtomicType

import net.sf.saxon.om._

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.QNameValue

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
