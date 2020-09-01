package net.sf.saxon.functions

import net.sf.saxon.utils.Configuration

import net.sf.saxon.expr._

import net.sf.saxon.expr.parser.ContextItemStaticInfo

import net.sf.saxon.expr.parser.ExpressionTool

import net.sf.saxon.expr.parser.ExpressionVisitor

import net.sf.saxon.expr.parser.TypeChecker

import net.sf.saxon.model.BuiltInAtomicType

import net.sf.saxon.model.TypeHierarchy

import net.sf.saxon.om.GroundedValue

import net.sf.saxon.om.Sequence

import net.sf.saxon.pattern.NodeTest

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.BooleanValue

import BooleanFn._

object BooleanFn {

  def rewriteEffectiveBooleanValue(exp: Expression,
                                   visitor: ExpressionVisitor,
                                   contextItemType: ContextItemStaticInfo): Expression = {
    var exprssn = exp
    val config: Configuration = visitor.getConfiguration
    val th: TypeHierarchy = config.getTypeHierarchy
    val forStreaming: Boolean = visitor.isOptimizeForStreaming
    exprssn = ExpressionTool.unsortedIfHomogeneous(exp, forStreaming)
    if (exprssn.isInstanceOf[Literal]) {
      val `val`: GroundedValue = exprssn.asInstanceOf[Literal].value
      if (`val`.isInstanceOf[BooleanValue]) return exprssn
      Literal.makeLiteral(
        BooleanValue.get(
          ExpressionTool.effectiveBooleanValue(`val`.iterate())),
        exprssn)
    }
    if (exprssn.isInstanceOf[ValueComparison]) {
      val vc: ValueComparison = exprssn.asInstanceOf[ValueComparison]
      if (vc.getResultWhenEmpty == null) {
        vc.setResultWhenEmpty(BooleanValue.FALSE)
      }
      exprssn
    } else if (exprssn.isCallOn(classOf[BooleanFn])) {
      exprssn.asInstanceOf[SystemFunctionCall].getArg(0)
    } else if (th.isSubType(exprssn.getItemType, BuiltInAtomicType.BOOLEAN) &&
      exprssn.getCardinality == StaticProperty.EXACTLY_ONE) {
      exprssn
    } else if (exprssn.isCallOn(classOf[Count])) {
      val exists: Expression = SystemFunction.makeCall(
        "exists",
        exprssn.getRetainedStaticContext,
        exprssn.asInstanceOf[SystemFunctionCall].getArg(0))
      assert(exists != null)
      ExpressionTool.copyLocationInfo(exprssn, exists)
      exists.optimize(visitor, contextItemType)
    } else if (exprssn.getItemType.isInstanceOf[NodeTest]) {
      val exists: Expression =
        SystemFunction.makeCall("exists", exprssn.getRetainedStaticContext, exprssn)
      assert(exists != null)
      ExpressionTool.copyLocationInfo(exprssn, exists)
      exists.optimize(visitor, contextItemType)
    } else {
      null
    }
  }

}

class BooleanFn extends SystemFunction {

  override def supplyTypeInformation(visitor: ExpressionVisitor,
                                     contextItemType: ContextItemStaticInfo,
                                     arguments: Array[Expression]): Unit = {
    val err = TypeChecker.ebvError(
      arguments(0),
      visitor.getConfiguration.getTypeHierarchy)
    if (err != null) {
      throw err
    }
  }

  def call(c: XPathContext, arguments: Array[Sequence]): BooleanValue = {
    val bValue: Boolean =
      ExpressionTool.effectiveBooleanValue(arguments(0).iterate())
    BooleanValue.get(bValue)
  }

  def makeFunctionCall(arguments: Array[Expression]): Expression =
    new SystemFunctionCall(this, arguments) {
      override def optimize(visitor: ExpressionVisitor,
                            contextItemType: ContextItemStaticInfo): Expression = {
        val e: Expression = super.optimize(visitor, contextItemType)
        if (e == this) {
          var ebv: Expression =
            rewriteEffectiveBooleanValue(getArg(0), visitor, contextItemType)
          if (ebv != null) {
            ebv = ebv.optimize(visitor, contextItemType)
            if (ebv.getItemType == BuiltInAtomicType.BOOLEAN && ebv.getCardinality == StaticProperty.EXACTLY_ONE) {
              ebv.setParentExpression(getParentExpression)
              return ebv
            } else {
              setArg(0, ebv)
              adoptChildExpression(ebv)
              return this
            }
          }
        }
        e
      }

      override def effectiveBooleanValue(c: XPathContext): Boolean =
        try getArg(0).effectiveBooleanValue(c)
        catch {
          case e: XPathException => {
            e.maybeSetLocation(getLocation)
            e.maybeSetContext(c)
            throw e
          }

        }

      override def evaluateItem(context: XPathContext): BooleanValue =
        BooleanValue.get(effectiveBooleanValue(context))
    }

  override def getCompilerName(): String = "BooleanFnCompiler"

  override def getStreamerName: String = "BooleanFn"

}
