package net.sf.saxon.expr

import net.sf.saxon.expr.instruct.Choose

import net.sf.saxon.expr.parser._

import net.sf.saxon.functions.SystemFunction

import net.sf.saxon.model.BuiltInAtomicType

import net.sf.saxon.model.TypeHierarchy

import net.sf.saxon.trans.XPathException

import net.sf.saxon.value.BooleanValue

import java.util.Collection

import scala.jdk.CollectionConverters._

object AndExpression {

  def distribute(exprs: Collection[Expression]): Expression = {
    var result: Expression = null
    if (exprs != null) {
      var first: Boolean = true
      for (e <- exprs.asScala) {
        if (first) {
          first = false
          result = e
        } else {
          result = new AndExpression(result, e)
        }
      }
    }
    result
  }

}

class AndExpression(p1: Expression, p2: Expression)
  extends BooleanExpression(p1, Token.AND, p2) {

  override def preEvaluate(): Expression =
    if (Literal.isConstantBoolean(getLhsExpression, value = false) || Literal
      .isConstantBoolean(getRhsExpression, value = false)) {
      Literal.makeLiteral(BooleanValue.FALSE, this)
    } else if (Literal.hasEffectiveBooleanValue(getLhsExpression, value = true)) {
      forceToBoolean(getRhsExpression)
    } else if (Literal.hasEffectiveBooleanValue(getRhsExpression, value = true)) {
      forceToBoolean(getLhsExpression)
    } else {
      this
    }

  override def optimize(visitor: ExpressionVisitor,
                        contextInfo: ContextItemStaticInfo): Expression = {
    val t: Expression = super.optimize(visitor, contextInfo)
    if (t != this) {
      return t
    }
    val th: TypeHierarchy = visitor.getConfiguration.getTypeHierarchy
    if (getRhsExpression.isInstanceOf[UserFunctionCall] &&
      th.isSubType(getRhsExpression.getItemType, BuiltInAtomicType.BOOLEAN) &&
      !ExpressionTool.isLoopingSubexpression(this, null)) {
      val cond: Expression = Choose.makeConditional(
        getLhsExpression,
        getRhsExpression,
        Literal.makeLiteral(BooleanValue.FALSE, this))
      ExpressionTool.copyLocationInfo(this, cond)
      return cond
    }
    this
  }

  override def getCost: Double =
    getLhsExpression.getCost + getRhsExpression.getCost / 2

  def copy(rebindings: RebindingMap): Expression = {
    val a2: AndExpression = new AndExpression(
      getLhsExpression.copy(rebindings),
      getRhsExpression.copy(rebindings))
    ExpressionTool.copyLocationInfo(this, a2)
    a2
  }

  def negate(): Expression = {
    val not0: Expression = SystemFunction.makeCall("not",
      getRetainedStaticContext,
      getLhsExpression)
    val not1: Expression = SystemFunction.makeCall("not",
      getRetainedStaticContext,
      getRhsExpression)
    new OrExpression(not0, not1)
  }

  override def tag(): String = "and"

  override def effectiveBooleanValue(c: XPathContext): Boolean =
    getLhsExpression.effectiveBooleanValue(c) && getRhsExpression
      .effectiveBooleanValue(c)

}
