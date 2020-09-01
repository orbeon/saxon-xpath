package net.sf.saxon.functions.hof

import net.sf.saxon.expr.Literal
import net.sf.saxon.expr.XPathContext
import net.sf.saxon.expr.XPathContextMajor
import net.sf.saxon.expr.instruct.UserFunction
import net.sf.saxon.functions.AbstractFunction
import net.sf.saxon.model.FunctionItemType
import net.sf.saxon.model.SpecificFunctionType
import net.sf.saxon.om.Function
import net.sf.saxon.om.Sequence
import net.sf.saxon.om.StructuredQName
import net.sf.saxon.trace.ExpressionPresenter
import net.sf.saxon.value.SequenceType
import java.util.Arrays

import net.sf.saxon.query.AnnotationList

class CurriedFunction(private var targetFunction: Function,
                      private var boundValues: Array[Sequence])
  extends AbstractFunction {

  private var functionType: FunctionItemType = _

  def getFunctionItemType(): FunctionItemType = {
    if (functionType == null) {
      val baseItemType: FunctionItemType = targetFunction.getFunctionItemType
      var resultType: SequenceType = SequenceType.ANY_SEQUENCE
      if (baseItemType.isInstanceOf[SpecificFunctionType]) {
        resultType = baseItemType.getResultType
      }
      var placeholders: Int = 0
      for (boundArgument <- boundValues if boundArgument == null) {
        placeholders += 1
      }
      val argTypes: Array[SequenceType] =
        Array.ofDim[SequenceType](placeholders)
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

  def getDescription(): String =
    "partially-applied function " + targetFunction.getDescription

  def getArity(): Int = {
    var count: Int = 0
    for (v <- boundValues if v == null) {
      {
        count += 1;
        count - 1
      }
    }
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
    val c2: XPathContext = targetFunction.makeNewContext(context, null)
    if (targetFunction.isInstanceOf[UserFunction]) {
      c2.asInstanceOf[XPathContextMajor]
        .setCurrentComponent(
          targetFunction.asInstanceOf[UserFunction].getDeclaringComponent)
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
