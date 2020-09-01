package net.sf.saxon.functions

import net.sf.saxon.expr.Expression

import net.sf.saxon.expr.XPathContext

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.expr.parser.Loc

import net.sf.saxon.expr.parser.RoleDiagnostic

import net.sf.saxon.functions.registry.BuiltInFunctionSet

import net.sf.saxon.ma.arrays.ArrayFunctionSet

import net.sf.saxon.ma.arrays.ArrayItem

import net.sf.saxon.ma.arrays.ArrayItemType

import net.sf.saxon.ma.arrays.SquareArrayConstructor

import net.sf.saxon.ma.map.MapFunctionSet

import net.sf.saxon.ma.map.MapType

import net.sf.saxon.model._

import net.sf.saxon.om.Function

import net.sf.saxon.om.Sequence

import net.sf.saxon.trace.ExpressionPresenter

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.SequenceType

import java.util.Properties

class ApplyFn extends SystemFunction {

  private var dynamicFunctionCall: String = _

  def setDynamicFunctionCall(fnExpr: String): Unit = {
    dynamicFunctionCall = fnExpr
  }

  def isDynamicFunctionCall: Boolean = dynamicFunctionCall != null

  override def getResultItemType(args: Array[Expression]): ItemType = {
    val fnType: ItemType = args(0).getItemType
    if (fnType.isInstanceOf[MapType]) {
      fnType.asInstanceOf[MapType].getValueType.getPrimaryType
    } else if (fnType.isInstanceOf[ArrayItemType]) {
      fnType.asInstanceOf[ArrayItemType].getMemberType.getPrimaryType
    } else if (fnType.isInstanceOf[FunctionItemType]) {
      fnType.asInstanceOf[FunctionItemType].getResultType.getPrimaryType
    } else if (fnType.isInstanceOf[AnyFunctionType]) {
      AnyItemType
    } else {
      AnyItemType
    }
  }

  override def makeOptimizedFunctionCall(
                                 visitor: ExpressionVisitor,
                                 contextInfo: ContextItemStaticInfo,
                                 arguments: Expression*): Expression = {
    if (arguments.length == 2 && arguments(1)
      .isInstanceOf[SquareArrayConstructor]) {
      val target: Expression = arguments(0)
      if (target.getItemType.isInstanceOf[MapType]) {
        makeGetCall(visitor,
          MapFunctionSet.getInstance,
          contextInfo,
          arguments.toArray)
      } else if (target.getItemType.isInstanceOf[ArrayItemType]) {
        makeGetCall(visitor,
          ArrayFunctionSet.getInstance,
          contextInfo,
          arguments.toArray)
      }
    }
    null
  }

  private def makeGetCall(visitor: ExpressionVisitor,
                          fnSet: BuiltInFunctionSet,
                          contextInfo: ContextItemStaticInfo,
                          arguments: Array[Expression]): Expression = {
    val target: Expression = arguments(0)
    val key: Expression = arguments(1)
      .asInstanceOf[SquareArrayConstructor]
      .getOperanda
      .getOperand(0)
      .getChildExpression
    val getter: Expression =
      fnSet.makeFunction("get", 2).makeFunctionCall(target, key)
    getter.setRetainedStaticContext(target.getRetainedStaticContext)
    getter.typeCheck(visitor, contextInfo)
  }

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence = {
    val function: Function = arguments(0).head.asInstanceOf[Function]
    val args: ArrayItem = arguments(1).head.asInstanceOf[ArrayItem]
    if (function.getArity != args.arrayLength()) {
      val errorCode: String =
        if (isDynamicFunctionCall) "XPTY0004" else "FOAP0001"
      val err = new XPathException(
        "Number of arguments required for dynamic call to " +
          function.getDescription +
          " is " +
          function.getArity +
          "; number supplied = " +
          args.arrayLength(),
        errorCode
      )
      err.setIsTypeError(isDynamicFunctionCall)
      err.setXPathContext(context)
      throw err
    }
    val th: TypeHierarchy = context.getConfiguration.getTypeHierarchy
    val fit: FunctionItemType = function.getFunctionItemType
    val argArray: Array[Sequence] = Array.ofDim[Sequence](args.arrayLength())
    if (fit == AnyFunctionType.ANY_FUNCTION) {
      for (i <- 0 until argArray.length) {
        argArray(i) = args.get(i)
      }
    } else {
      for (i <- 0 until argArray.length) {
        val expected: SequenceType = fit.getArgumentTypes(i)
        var role: RoleDiagnostic = null
        role =
          if (isDynamicFunctionCall)
            new RoleDiagnostic(RoleDiagnostic.FUNCTION,
              "result of " + dynamicFunctionCall,
              i)
          else new RoleDiagnostic(RoleDiagnostic.FUNCTION, "fn:apply", i + 1)
        val converted: Sequence = th.applyFunctionConversionRules(args.get(i),
          expected,
          role,
          Loc.NONE)
        argArray(i) = converted.materialize()
      }
    }
    val rawResult: Sequence = SystemFunction.dynamicCall(function, context, argArray)
    if (function.isTrustedResultType) {
      rawResult
    } else {
      val resultRole: RoleDiagnostic =
        new RoleDiagnostic(RoleDiagnostic.FUNCTION_RESULT, "fn:apply", -1)
      th.applyFunctionConversionRules(rawResult,
        fit.getResultType,
        resultRole,
        Loc.NONE)
    }
  }

  override def exportAttributes(out: ExpressionPresenter): Unit = {
    out.emitAttribute("dyn", dynamicFunctionCall)
  }

  override def importAttributes(attributes: Properties): Unit = {
    dynamicFunctionCall = attributes.getProperty("dyn")
  }

}
