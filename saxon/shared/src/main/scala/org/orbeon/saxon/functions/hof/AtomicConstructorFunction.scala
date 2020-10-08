package org.orbeon.saxon.functions.hof

import org.orbeon.saxon.utils.Configuration

import org.orbeon.saxon.expr.StaticProperty

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.functions.AbstractFunction

import org.orbeon.saxon.model.AtomicType

import org.orbeon.saxon.model.Converter

import org.orbeon.saxon.model.FunctionItemType

import org.orbeon.saxon.model.SpecificFunctionType

import org.orbeon.saxon.om.NamespaceResolver

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.om.StructuredQName

import org.orbeon.saxon.om.ZeroOrOne

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.AtomicValue

import org.orbeon.saxon.value.SequenceType

class AtomicConstructorFunction(private var targetType: AtomicType,
                                resolver: NamespaceResolver)
  extends AbstractFunction {

  private var nsResolver: NamespaceResolver = resolver

  def getFunctionItemType: FunctionItemType =
    new SpecificFunctionType(
      Array(SequenceType.OPTIONAL_ATOMIC),
      SequenceType.makeSequenceType(targetType,
        StaticProperty.ALLOWS_ZERO_OR_ONE))

  def getFunctionName: StructuredQName = targetType.getTypeName

  def getDescription: String = getFunctionName.getDisplayName

  def getArity: Int = 1

  def call(context: XPathContext, args: Array[Sequence]): ZeroOrOne[AtomicValue] = {
    val _val = args(0).head.asInstanceOf[AtomicValue]
    if (_val == null) {
      ZeroOrOne.empty
    } else {
      val config = context.getConfiguration
      var converter = config.getConversionRules.getConverter(_val.getItemType, targetType)
      if (converter == null) {
        val ex: XPathException = new XPathException(
          "Cannot convert " + _val.getItemType + " to " + targetType,
          "XPTY0004")
        ex.setIsTypeError(true)
        throw ex
      }
      converter = converter.setNamespaceResolver(nsResolver)
      new ZeroOrOne(converter.convert(_val).asAtomic())
    }
  }

  override def export(out: ExpressionPresenter): Unit = {
    out.startElement("acFnRef")
    out.emitAttribute("name", targetType.getTypeName)
    out.endElement()
  }

  override def isTrustedResultType: Boolean = true
}
