package net.sf.saxon.expr

import net.sf.saxon.expr.instruct.OriginalFunction

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.expr.parser.RebindingMap

import net.sf.saxon.functions.SystemFunction

import net.sf.saxon.model._

import net.sf.saxon.om.Function

import net.sf.saxon.om.Sequence

import net.sf.saxon.om.StructuredQName

import net.sf.saxon.trace.ExpressionPresenter

import scala.jdk.CollectionConverters._

class StaticFunctionCall(private var target: Function,
                         arguments: Array[Expression])
  extends FunctionCall
    with Callable {

  if (target.getArity != arguments.length) {
    throw new IllegalArgumentException("Function call to " + target.getFunctionName + " with wrong number of arguments (" + arguments.length + ")")
  }

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
    val args: Array[Expression] = Array.ofDim[Expression](getArity)
    for (i <- 0 until args.length) {
      args(i) = getArg(i).copy(rebindings)
    }
    new StaticFunctionCall(target, args)
  }

   override def computeCardinality(): Int =
    target.getFunctionItemType.getResultType.getCardinality

  override def getItemType: ItemType =
    target.getFunctionItemType.getResultType.getPrimaryType

  override def getStaticUType(contextItemType: UType): UType = {
    var result: UType = getItemType.getUType
    for (o <- operands.asScala if o.getUsage == OperandUsage.TRANSMISSION) {
      result = result.intersection(
        o.getChildExpression.getStaticUType(contextItemType))
    }
    result
  }

  def call(context: XPathContext, arguments: Array[Sequence]): Sequence =
    target.call(context, arguments)

  override def getExpressionName: String = "staticFunctionCall"

  override def export(out: ExpressionPresenter): Unit = {
    if (target.isInstanceOf[OriginalFunction]) {
      val options: ExpressionPresenter.ExportOptions =
        out.getOptions.asInstanceOf[ExpressionPresenter.ExportOptions]
      val pf: OriginalFunction = target.asInstanceOf[OriginalFunction]
      out.startElement("origFC", this)
      out.emitAttribute("name", pf.getFunctionName)
      out.emitAttribute("pack", options.componentMap.get(pf.getComponent.getContainingPackage).toString)
      for (o <- operands.asScala) {
        o.getChildExpression.export(out)
      }
      out.endElement()
    } else {
      if (target.isInstanceOf[UnionCastableFunction]) {
        val targetType: UnionType =
          target.asInstanceOf[UnionConstructorFunction].getTargetType
        out.startElement("castable", this)
        if (targetType.isInstanceOf[LocalUnionType]) {
          out.emitAttribute("to", AlphaCode.fromItemType(targetType))
        } else {
          out.emitAttribute("as", targetType.toExportString)
        }
        out.emitAttribute(
          "flags",
          "u" +
            (if (target.asInstanceOf[UnionConstructorFunction].isAllowEmpty)
              "e"
            else ""))
        for (o <- operands.asScala) {
          o.getChildExpression.export(out)
        }
        out.endElement()
      } else if (target.isInstanceOf[ListCastableFunction]) {
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
        for (o <- operands.asScala) {
          o.getChildExpression.export(out)
        }
        out.endElement()
      } else if (target.isInstanceOf[UnionConstructorFunction]) {
        val targetType: UnionType =
          target.asInstanceOf[UnionConstructorFunction].getTargetType
        out.startElement("cast", this)
        if (targetType.isInstanceOf[LocalUnionType]) {
          out.emitAttribute("to", AlphaCode.fromItemType(targetType))
        } else {
          out.emitAttribute("as", targetType.toExportString)
        }
        out.emitAttribute(
          "flags",
          "u" +
            (if (target.asInstanceOf[UnionConstructorFunction].isAllowEmpty)
              "e"
            else ""))
        for (o <- operands.asScala) {
          o.getChildExpression.export(out)
        }
        out.endElement()
      } else if (target.isInstanceOf[ListConstructorFunction]) {
        out.startElement("cast", this)
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
        for (o <- operands.asScala) {
          o.getChildExpression.export(out)
        }
        out.endElement()
      } else {
        super.export(out)
      }
    }
  }

}
