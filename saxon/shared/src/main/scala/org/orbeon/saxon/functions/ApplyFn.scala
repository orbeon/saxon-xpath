package org.orbeon.saxon.functions

import org.orbeon.saxon.expr.Expression

import org.orbeon.saxon.expr.XPathContext

import org.orbeon.saxon.expr.parser.ContextItemStaticInfo

import org.orbeon.saxon.expr.parser.ExpressionVisitor

import org.orbeon.saxon.expr.parser.Loc

import org.orbeon.saxon.expr.parser.RoleDiagnostic

import org.orbeon.saxon.functions.registry.BuiltInFunctionSet

import org.orbeon.saxon.ma.arrays.ArrayFunctionSet

import org.orbeon.saxon.ma.arrays.ArrayItem

import org.orbeon.saxon.ma.arrays.ArrayItemType

import org.orbeon.saxon.ma.arrays.SquareArrayConstructor

import org.orbeon.saxon.ma.map.MapFunctionSet

import org.orbeon.saxon.ma.map.MapType

import org.orbeon.saxon.model._

import org.orbeon.saxon.om.Function

import org.orbeon.saxon.om.Sequence

import org.orbeon.saxon.trace.ExpressionPresenter

import org.orbeon.saxon.trans.XPathException

import org.orbeon.saxon.value.SequenceType

import java.util.Properties

class ApplyFn extends SystemFunction {

  private var dynamicFunctionCall: String = _

  def setDynamicFunctionCall(fnExpr: String): Unit = {
    dynamicFunctionCall = fnExpr
  }

  def isDynamicFunctionCall: Boolean = dynamicFunctionCall != null

  override def getResultItemType(args: Array[Expression]): ItemType = {
    val fnType = args(0).getItemType
    fnType match {
      case mapType: MapType =>
        mapType.getValueType.getPrimaryType
      case itemType: ArrayItemType =>
        itemType.getMemberType.getPrimaryType
      case itemType: FunctionItemType =>
        itemType.getResultType.getPrimaryType
      case _ =>
        AnyItemType
    }
  }

  override def makeOptimizedFunctionCall(
                                 visitor: ExpressionVisitor,
                                 contextInfo: ContextItemStaticInfo,
                                 arguments: Expression*): Expression = {
    if (arguments.length == 2 && arguments(1).isInstanceOf[SquareArrayConstructor]) {
      val target: Expression = arguments(0)
      target.getItemType match {
        case _: MapType =>
          makeGetCall(visitor,
            MapFunctionSet.getInstance,
            contextInfo,
            arguments.toArray)
        case _: ArrayItemType =>
          makeGetCall(visitor,
            ArrayFunctionSet.getInstance,
            contextInfo,
            arguments.toArray)
        case _ =>
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
      val errorCode = if (isDynamicFunctionCall) "XPTY0004" else "FOAP0001"
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
    val th = context.getConfiguration.getTypeHierarchy
    val fit: FunctionItemType = function.getFunctionItemType
    val argArray: Array[Sequence] = Array.ofDim[Sequence](args.arrayLength())
    if (fit == AnyFunctionType) {
      for (i <- argArray.indices)
        argArray(i) = args.get(i)
    } else {
      for (i <- argArray.indices) {
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
        argArray(i) = converted.materialize
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
