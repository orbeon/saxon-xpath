package net.sf.saxon.functions.hof

import net.sf.saxon.utils.Configuration

import net.sf.saxon.expr.StaticProperty

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.functions.AbstractFunction

import net.sf.saxon.model.AtomicType

import net.sf.saxon.model.Converter

import net.sf.saxon.model.FunctionItemType

import net.sf.saxon.model.SpecificFunctionType

import net.sf.saxon.om.NamespaceResolver

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.om.ZeroOrOne

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.SequenceType

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
    val `val`: AtomicValue = args(0).head.asInstanceOf[AtomicValue]
    if (`val` == null) {
      ZeroOrOne.empty()
    }
    val config: Configuration = context.getConfiguration
    var converter: Converter =
      config.getConversionRules.getConverter(`val`.getItemType, targetType)
    if (converter == null) {
      val ex: XPathException = new XPathException(
        "Cannot convert " + `val`.getItemType + " to " + targetType,
        "XPTY0004")
      ex.setIsTypeError(true)
      throw ex
    }
    converter = converter.setNamespaceResolver(nsResolver)
    new ZeroOrOne(converter.convert(`val`).asAtomic())
  }

  override def export(out: ExpressionPresenter): Unit = {
    out.startElement("acFnRef")
    out.emitAttribute("name", targetType.getTypeName)
    out.endElement()
  }

  override def isTrustedResultType(): Boolean = true

}
