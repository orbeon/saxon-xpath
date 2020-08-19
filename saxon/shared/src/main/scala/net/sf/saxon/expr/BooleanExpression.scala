package net.sf.saxon.expr

import net.sf.saxon.expr.parser._

import net.sf.saxon.functions.BooleanFn

import net.sf.saxon.functions.SystemFunction

import net.sf.saxon.model.BuiltInAtomicType

import net.sf.saxon.model.ItemType

import net.sf.saxon.model.TypeHierarchy

import net.sf.saxon.model.UType

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.BooleanValue

import java.util.List

import scala.jdk.CollectionConverters._

object BooleanExpression {

  def listAndComponents(exp: Expression, list: List[Expression]): Unit = {
    if (exp.isInstanceOf[BooleanExpression] &&
      exp.asInstanceOf[BooleanExpression].getOperator == Token.AND) {
      for (o <- exp.operands().asScala) {
        listAndComponents(o.getChildExpression, list)
      }
    } else {
      list.add(exp)
    }
  }

}

abstract class BooleanExpression(p1: Expression, operator: Int, p2: Expression)
  extends BinaryExpression(p1, operator, p2)
    with Negatable {

  override def getExpressionName(): String =
    Token.tokens(getOperator) + "-expression"

  override def typeCheck(visitor: ExpressionVisitor,
                         contextInfo: ContextItemStaticInfo): Expression = {
    getLhs.typeCheck(visitor, contextInfo)
    getRhs.typeCheck(visitor, contextInfo)
    val th: TypeHierarchy = visitor.getConfiguration.getTypeHierarchy
    val err0: XPathException = TypeChecker.ebvError(getLhsExpression, th)
    if (err0 != null) {
      err0.setLocator(getLocation)
      throw err0
    }
    val err1: XPathException = TypeChecker.ebvError(getRhsExpression, th)
    if (err1 != null) {
      err1.setLocator(getLocation)
      throw err1
    }
    if (getLhsExpression.isInstanceOf[Literal] &&
      !(getLhsExpression
        .asInstanceOf[Literal]
        .getValue
        .isInstanceOf[BooleanValue])) {
      this.setLhsExpression(Literal.makeLiteral(
        BooleanValue.get(
          getLhsExpression.effectiveBooleanValue(
            visitor.makeDynamicContext())),
        this))
    }
    if (getRhsExpression.isInstanceOf[Literal] &&
      !(getRhsExpression
        .asInstanceOf[Literal]
        .getValue
        .isInstanceOf[BooleanValue])) {
      this.setRhsExpression(Literal.makeLiteral(
        BooleanValue.get(
          getRhsExpression.effectiveBooleanValue(
            visitor.makeDynamicContext())),
        this))
    }
    preEvaluate()
  }

  override def computeCardinality(): Int = StaticProperty.EXACTLY_ONE

  override def optimize(visitor: ExpressionVisitor,
                        contextItemType: ContextItemStaticInfo): Expression = {
    optimizeChildren(visitor, contextItemType)
    val forStreaming: Boolean = visitor.isOptimizeForStreaming
    this.setLhsExpression(ExpressionTool.unsortedIfHomogeneous(getLhsExpression, forStreaming))
    this.setRhsExpression(ExpressionTool.unsortedIfHomogeneous(getRhsExpression, forStreaming))
    val op0: Expression = BooleanFn.rewriteEffectiveBooleanValue(
      getLhsExpression,
      visitor,
      contextItemType)
    if (op0 != null) {
      this.setLhsExpression(op0)
    }
    val op1: Expression = BooleanFn.rewriteEffectiveBooleanValue(
      getRhsExpression,
      visitor,
      contextItemType)
    if (op1 != null) {
      this.setRhsExpression(op1)
    }
    preEvaluate()
  }

   def preEvaluate(): Expression

   def forceToBoolean(in: Expression): Expression =
    if (in.getItemType == BuiltInAtomicType.BOOLEAN && in.getCardinality == StaticProperty.ALLOWS_ONE) {
      in
    } else {
      SystemFunction.makeCall("boolean", getRetainedStaticContext, in)
    }

  def isNegatable(th: TypeHierarchy): Boolean = true

  def negate(): Expression

  override def evaluateItem(context: XPathContext): BooleanValue =
    BooleanValue.get(effectiveBooleanValue(context))

  def effectiveBooleanValue(c: XPathContext): Boolean

  def getItemType(): ItemType = BuiltInAtomicType.BOOLEAN

  override def getStaticUType(contextItemType: UType): UType = UType.BOOLEAN

   override def getOperandRole(arg: Int): OperandRole =
    OperandRole.INSPECT

}
