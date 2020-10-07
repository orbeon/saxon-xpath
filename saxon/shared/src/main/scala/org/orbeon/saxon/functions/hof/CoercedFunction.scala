package org.orbeon.saxon.functions.hof

import org.orbeon.saxon.expr.{FunctionCall, XPathContext}
import org.orbeon.saxon.expr.parser.{ContextItemStaticInfo, ExpressionVisitor, Loc, RoleDiagnostic}
import org.orbeon.saxon.functions.AbstractFunction
import org.orbeon.saxon.functions.hof.CoercedFunction._
import org.orbeon.saxon.model.{FunctionItemType, SpecificFunctionType, TypeHierarchy}
import org.orbeon.saxon.om.{Function, Sequence, StructuredQName}
import org.orbeon.saxon.trace.ExpressionPresenter
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.value.SequenceType

object CoercedFunction {

  def coerce(suppliedFunction: Function,
             requiredType: SpecificFunctionType,
             role: RoleDiagnostic): CoercedFunction = {
    val arity: Int = requiredType.getArity
    if (suppliedFunction.getArity != arity) {
      var msg: String =
        role.composeErrorMessage(requiredType, suppliedFunction, null)
      msg += ". " + wrongArityMessage(suppliedFunction, arity)
      throw new XPathException(msg, "XPTY0004")
    }
    new CoercedFunction(suppliedFunction, requiredType)
  }

  private def wrongArityMessage(supplied: Function, expected: Int): String =
    "The supplied function (" + supplied.getDescription +
      ") has " +
      FunctionCall.pluralArguments(supplied.getArity) +
      " - expected " +
      expected

}

class CoercedFunction(private var targetFunction: Function, private val requiredType: SpecificFunctionType) extends AbstractFunction {

  if (targetFunction.getArity != requiredType.getArity)
    throw new XPathException(wrongArityMessage(targetFunction, requiredType.getArity), "XPTY0004")

  def this(requiredType: SpecificFunctionType) =
    this(null, requiredType)

  def setTargetFunction(targetFunction: Function): Unit = {
    if (targetFunction.getArity != requiredType.getArity) {
      throw new XPathException(
        wrongArityMessage(targetFunction, requiredType.getArity),
        "XPTY0004")
    }
    this.targetFunction = targetFunction
  }

  override def typeCheck(visitor: ExpressionVisitor, contextItemType: ContextItemStaticInfo): Unit =
    targetFunction match {
      case function: AbstractFunction =>
        function.typeCheck(visitor, contextItemType)
      case _ =>
    }

  def getFunctionItemType: FunctionItemType = requiredType
  def getFunctionName: StructuredQName = targetFunction.getFunctionName
  def getDescription: String = "coerced " + targetFunction.getDescription
  def getArity: Int = targetFunction.getArity

  def call(context: XPathContext, args: Array[Sequence]): Sequence = {
    val req: SpecificFunctionType = requiredType
    val argTypes: Array[SequenceType] =
      targetFunction.getFunctionItemType.getArgumentTypes
    val th: TypeHierarchy = context.getConfiguration.getTypeHierarchy
    val targetArgs: Array[Sequence] = Array.ofDim[Sequence](args.length)
    for (i <- args.indices) {
      args(i) = args(i).materialize()
      if (argTypes(i).matches(args(i), th)) {
        targetArgs(i) = args(i)
      } else {
        val role: RoleDiagnostic = new RoleDiagnostic(
          RoleDiagnostic.FUNCTION,
          targetFunction.getDescription,
          i)
        targetArgs(i) =
          th.applyFunctionConversionRules(args(i), argTypes(i), role, Loc.NONE)
      }
    }
    var rawResult: Sequence = targetFunction.call(context, targetArgs)
    rawResult = rawResult.materialize()
    if (req.getResultType.matches(rawResult, th)) {
      rawResult
    } else {
      val role: RoleDiagnostic = new RoleDiagnostic(
        RoleDiagnostic.FUNCTION_RESULT,
        targetFunction.getDescription,
        0)
      th.applyFunctionConversionRules(rawResult,
        req.getResultType,
        role,
        Loc.NONE)
    }
  }

  override def export(out: ExpressionPresenter): Unit = {
    out.startElement("coercedFn")
    out.emitAttribute("type", requiredType.toExportString)
    new FunctionLiteral(targetFunction).export(out)
    out.endElement()
  }

}
