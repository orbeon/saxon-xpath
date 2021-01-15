package org.orbeon.saxon.expr

import org.orbeon.saxon.expr.instruct.OriginalFunction
import org.orbeon.saxon.expr.parser.{ContextItemStaticInfo, ExpressionVisitor, RebindingMap}
import org.orbeon.saxon.functions.SystemFunction
import org.orbeon.saxon.model._
import org.orbeon.saxon.om.{Function, Sequence, StructuredQName}
import org.orbeon.saxon.trace.ExpressionPresenter

//import scala.collection.compat._
import scala.jdk.CollectionConverters._


class StaticFunctionCall(private var target: Function,
                         arguments: Array[Expression])
  extends FunctionCall
    with Callable {

  if (target.getArity != arguments.length)
    throw new IllegalArgumentException("Function call to " + target.getFunctionName + " with wrong number of arguments (" + arguments.length + ")")

  setOperanda(arguments, target.getOperandRoles)

  def getTargetFunction: Function = target

  override def getTargetFunction(context: XPathContext): Function =
    getTargetFunction

  override def getFunctionName: StructuredQName = target.getFunctionName

  override def isCallOn(function: Class[_ <: SystemFunction]): Boolean =
    function.isAssignableFrom(target.getClass)

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    checkFunctionCall(target, visitor)
    super.typeCheck(visitor, contextInfo)
  }

  override def copy(rebindings: RebindingMap): Expression = {
    val args = Array.ofDim[Expression](getArity)
    for (i <- args.indices)
      args(i) = getArg(i).copy(rebindings)
    new StaticFunctionCall(target, args)
  }

  override def computeCardinality(): Int =
    target.getFunctionItemType.getResultType.getCardinality

  override def getItemType: ItemType =
    target.getFunctionItemType.getResultType.getPrimaryType

  override def getStaticUType(contextItemType: UType): UType = {
    var result = getItemType.getUType
    for (o <- operands.asScala if o.getUsage == OperandUsage.TRANSMISSION)
      result = result.intersection(o.getChildExpression.getStaticUType(contextItemType))
    result
  }

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence =
    target.call(context, arguments)

  override def getExpressionName: String = "staticFunctionCall"

  override def export(out: ExpressionPresenter): Unit = {
    target match {
      case originalFunction: OriginalFunction =>
        val options = out.getOptions.asInstanceOf[ExpressionPresenter.ExportOptions]
        out.startElement("origFC", this)
        out.emitAttribute("name", originalFunction.getFunctionName)
        out.emitAttribute("pack", options.componentMap.get(originalFunction.getComponent.getContainingPackage).toString)
        for (o <- operands.asScala)
          o.getChildExpression.export(out)
        out.endElement()
      case _ =>
        target match {
          case _: UnionCastableFunction =>
            val targetType = target.asInstanceOf[UnionConstructorFunction].getTargetType
            out.startElement("castable", this)
            if (targetType.isInstanceOf[LocalUnionType])
              out.emitAttribute("to", AlphaCode.fromItemType(targetType))
            else
              out.emitAttribute("as", targetType.toExportString)
            out.emitAttribute(
              "flags",
              "u" +
                (if (target.asInstanceOf[UnionConstructorFunction].isAllowEmpty)
                  "e"
                else ""))
            for (o <- operands.asScala)
              o.getChildExpression.export(out)
            out.endElement()
          case _: ListCastableFunction =>
            out.startElement("castable", this)
            out.emitAttribute("as",
              target
                .asInstanceOf[ListConstructorFunction]
                .getTargetType
                .getStructuredQName)
            out.emitAttribute(
              "flags",
              "l" +
                (if (target.asInstanceOf[ListConstructorFunction].isAllowEmpty) "e"
                else ""))
            for (o <- operands.asScala)
              o.getChildExpression.export(out)
            out.endElement()
          case function: UnionConstructorFunction =>
            val targetType = function.getTargetType
            out.startElement("cast", this)
            if (targetType.isInstanceOf[LocalUnionType])
              out.emitAttribute("to", AlphaCode.fromItemType(targetType))
            else
              out.emitAttribute("as", targetType.toExportString)
            out.emitAttribute(
              "flags",
              "u" +
                (if (function.isAllowEmpty)
                  "e"
                else ""))
            for (o <- operands.asScala)
              o.getChildExpression.export(out)
            out.endElement()
          case function: ListConstructorFunction =>
            out.startElement("cast", this)
            out.emitAttribute("as",
              function
                .getTargetType
                .getStructuredQName)
            out.emitAttribute(
              "flags",
              "l" +
                (if (function.isAllowEmpty) "e"
                else ""))
            for (o <- operands.asScala)
              o.getChildExpression.export(out)
            out.endElement()
          case _ =>
            super.export(out)
        }
    }
  }
}
