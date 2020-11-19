package org.orbeon.saxon.functions.hof

import org.orbeon.saxon.expr.{StaticProperty, XPathContext}
import org.orbeon.saxon.functions.AbstractFunction
import org.orbeon.saxon.model.{AtomicType, FunctionItemType, SpecificFunctionType}
import org.orbeon.saxon.om.{NamespaceResolver, Sequence, StructuredQName, ZeroOrOne}
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.value.{AtomicValue, SequenceType}

class AtomicConstructorFunction(private var targetType: AtomicType,
                                nsResolver: NamespaceResolver)
  extends AbstractFunction {

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
      new ZeroOrOne(converter.convert(_val).asAtomic)
    }
  }

  override def export(out: ExpressionPresenter): Unit = {
    out.startElement("acFnRef")
    out.emitAttribute("name", targetType.getTypeName)
    out.endElement()
  }

  override def isTrustedResultType: Boolean = true
}
