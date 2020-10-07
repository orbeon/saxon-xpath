package org.orbeon.saxon.functions.hof

import org.orbeon.saxon.expr.Literal
import org.orbeon.saxon.expr.XPathContext
import org.orbeon.saxon.expr.XPathContextMajor
import org.orbeon.saxon.expr.instruct.UserFunction
import org.orbeon.saxon.functions.AbstractFunction
import org.orbeon.saxon.model.FunctionItemType
import org.orbeon.saxon.model.SpecificFunctionType
import org.orbeon.saxon.om.Function
import org.orbeon.saxon.om.Sequence
import org.orbeon.saxon.om.StructuredQName
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.value.SequenceType
import java.util.Arrays

import org.orbeon.saxon.query.AnnotationList

class CurriedFunction(private var targetFunction: Function, private var boundValues: Array[Sequence])
  extends AbstractFunction {

  private var functionType: FunctionItemType = _

  def getFunctionItemType: FunctionItemType = {
    if (functionType == null) {
      val baseItemType = targetFunction.getFunctionItemType
      var resultType = SequenceType.ANY_SEQUENCE
      if (baseItemType.isInstanceOf[SpecificFunctionType])
        resultType = baseItemType.getResultType
      var placeholders = 0
      for (boundArgument <- boundValues if boundArgument == null)
        placeholders += 1
      val argTypes = Array.ofDim[SequenceType](placeholders)
      if (baseItemType.isInstanceOf[SpecificFunctionType]) {
        var j = 0
        for (i <- 0 until boundValues.length if boundValues(i) == null) {
          argTypes(j) = baseItemType.getArgumentTypes(i)
          j += 1
        }
      } else {
        Arrays.fill(argTypes.asInstanceOf[Array[AnyRef]], SequenceType.ANY_SEQUENCE)
      }
      functionType = new SpecificFunctionType(argTypes, resultType)
    }
    functionType
  }

  def getFunctionName: StructuredQName = null

  def getDescription: String =
    "partially-applied function " + targetFunction.getDescription

  def getArity: Int = {
    var count = 0
    for (v <- boundValues if v == null)
      count += 1
    count
  }

  override def getAnnotations(): AnnotationList = targetFunction.getAnnotations

  def call(context: XPathContext, args: Array[Sequence]): Sequence = {
    val newArgs = Array.ofDim[Sequence](boundValues.length)
    var j = 0
    for (i <- newArgs.indices) {
      newArgs(i) = if (boundValues(i) == null) args(j) else boundValues(i)
      j += 1
    }
    val c2 = targetFunction.makeNewContext(context, null)
    targetFunction match {
      case fn: UserFunction => c2.asInstanceOf[XPathContextMajor].setCurrentComponent(fn.getDeclaringComponent)
      case _ =>
    }
    targetFunction.call(c2, newArgs)
  }

  override def export(out: ExpressionPresenter): Unit = {
    out.startElement("curriedFunc")
    targetFunction.export(out)
    out.startElement("args")
    for (seq <- boundValues) {
      if (seq == null) {
        out.startElement("x")
        out.endElement()
      } else {
        Literal.exportValue(seq, out)
      }
    }
    out.endElement()
    out.endElement()
  }
}
